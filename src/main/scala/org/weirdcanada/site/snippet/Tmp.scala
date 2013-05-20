package org.weirdcanada.model
import scala.reflect.runtime.universe._

// Scala
import scalaz.Lens
import Lens.{mapVLens, lensId}

// Weird Canada
import org.weirdcanada.site.lib.{ApplyOnce}
import org.weirdcanada.site.snippet.{DynamicFormHelpers}

// Lift
import net.liftweb._
import http._
import js.{JsCmd, JsCmds}
import JsCmds.{After, Alert, Run}
import common._
import util.Helpers._

// Scala
import scala.xml.{NodeSeq, Text, Elem}
import scala.annotation.tailrec

trait HasFields[A] {
  val fields: List[DynamicField[A]]

  // Must be lazy owing to Trait initialization issues.
  lazy val nodeSeqTransformer: NodeSeq => NodeSeq = 
    fields
      .foldLeft( (ns: NodeSeq) => ns) { (acc, field) => acc andThen field.render(lensId[A], None) }
}

trait HasZero[A] {
  val zero: A
}

object MultiRender {
  private def lensReducer[A : HasZero, B](lens: Lens[B, Option[A]]): Lens[B,A] = {
    val zeroA = implicitly[HasZero[A]].zero
    Lens.lensu(
      get = (b) => lens.get(b).getOrElse(zeroA)
    , set = (b,a) => lens.set(b, Some(a))
    )
  }
}
trait MultiRender[A] { this: DynamicField[A] => 

  implicit val zeroA: HasZero[A]

  def renderOption[B](outerLens: Lens[B, Option[A]], outerName: Option[String]): NodeSeq => NodeSeq = {
    val newLens: Lens[B,A] = MultiRender.lensReducer(outerLens)
    render(newLens, outerName)
  }
}

trait HasMultiFields[A] extends HasFields[A] {
  override val fields: List[DynamicField[A] with MultiRender[A]]
}

/**
 * TODO: Does the lens returned from lensReducer obey the lens laws}
 */
object DynamicField {

  def label(outerName: Option[String], name: String): String =
    "%s%s".format(outerName.map{ _ + "-"}.getOrElse(""), name)

  def makeName(outerName: Option[String], name: String): String =
    "name=%s".format(label(outerName, name))

  def makeNameAdd(outerName: Option[String], name: String): String =
    "name=add-%s".format(label(outerName, name))

  def makeAdd(outerName: Option[String], name: String): String =
    "add-%s".format(label(outerName, name))

  private def lensReducer[A : HasZero, B](lens: Lens[B, Option[A]]): Lens[B,A] = {
    val zeroA = implicitly[HasZero[A]].zero
    Lens.lensu(
      get = (b) => lens.get(b).getOrElse(zeroA)
    , set = (b,a) => lens.set(b, Some(a))
    )
  }
}

sealed trait DynamicField[A] {
  import DynamicField.makeName
  val name: String
  def render[B](outerLens: Lens[B,A], outerName: Option[String]): NodeSeq => NodeSeq
}

case class BasicField[A](name: String, lens: Lens[A,String]) extends DynamicField[A] {
  import DynamicField.makeName
  def render[B](outerLens: Lens[B,A], outerName: Option[String] = None): NodeSeq => NodeSeq = {
    println(makeName(outerName, name))
    makeName(outerName, name) #> "" // SHtml.ajaxtext(OuterLens andThen lens)
  }
}

case class RecordField[A, B : HasFields](name: String, lens: Lens[A,B]) extends DynamicField[A] {
  val bRecord = implicitly[HasFields[B]]
  def render[C](outerLens: Lens[C,A], outerName: Option[String]): NodeSeq => NodeSeq = 
    bRecord
      .fields
      .foldLeft( (ns: NodeSeq) => ns ){ (acc, field) => acc andThen field.render(outerLens andThen lens, Some(name)) }
}

case class ManyRecordField[A : HasZero, B : HasMultiFields](name: String, lens: Lens[A, Map[Int,B]]) extends DynamicField[A] {
  import DynamicField.{makeAdd, makeName, makeNameAdd, label}
  val bRecord = implicitly[HasMultiFields[B]]
  val zeroA = implicitly[HasZero[A]]

  private def renderAtIndex[C](outerLens: Lens[C,A], outerName: Option[String])(index: Int): NodeSeq => NodeSeq = 
    makeNameAdd(outerName, name) #> makeAdd(outerName, name) andThen
    makeName(outerName, "%s-number".format(name)) #> (index+1) andThen
    bRecord
      .fields
      .foldLeft( (ns: NodeSeq) => ns ){ (acc, field) => 
        val newLens: Lens[C, Option[B]] = outerLens andThen lens andThen mapVLens(index)
        acc andThen field.renderOption( newLens, outerName)
      } andThen
    makeName(outerName, "%s-add [onclick]".format(name)) #> "" & // SHtml( renderAtIndex(index + 1) )
    makeName(outerName, "%s-remove [onclick]".format(name)) #> "" // SHtml.onEvent( removal )

  def render[C](outerLens: Lens[C,A], outerName: Option[String]): NodeSeq => NodeSeq = {
    def curriedIndex = renderAtIndex(outerLens, outerName) _
    /*def addNewRecordForm(index: Int): () => JsCmd = { () =>
      JsCdms.Replace("%s-elements".format(label(outerName, name), Text("") ++ */
    val addRecordMemoize = SHtml.memoize( curriedIndex(0) )
    "%s [id]".format(makeName(outerName, name)) #>  ""
  }

}


case class Artist(name: String, url: String)
object Artist {
  implicit object ArtistZero extends HasZero[Artist] {
    val zero = Artist("","")
  }
  implicit object ArtistRecord extends HasFields[Artist] {
    val fields = List(
      BasicField("name", Lens.lensu[Artist,String]((a,n) => a.copy(name = n), (a) => a.name))
    , BasicField("url", Lens.lensu[Artist,String]((a,n) => a.copy(url = n), (a) => a.url))
    )
  }
  
}

case class Publisher(name: String)
object Publisher {
  private val publisherNameLens: Lens[Publisher, String] = Lens.lensu( (p,n) => p.copy(name = n), (p) => p.name)
  implicit object PublisherZero extends HasZero[Publisher] {
    val zero = Publisher("")
  }
  implicit object PublisherRecord extends HasMultiFields[Publisher] {
    val fields: List[DynamicField[Publisher] with MultiRender[Publisher]] = List(
      new BasicField[Publisher]("name", publisherNameLens) with MultiRender[Publisher] { val zeroA = implicitly[HasZero[Publisher]] }
    )
  }
}
case class Release(title: String, artist: Artist, publishers: List[Publisher])
object Release {
  implicit object ReleaseZero extends HasZero[Release] { 
    val zero = Release("", implicitly[HasZero[Artist]].zero, Nil)
  }
  implicit object ReleaseRecord extends HasFields[Release] {

    private def releasePublishersLens: Lens[Release, Map[Int,Publisher]] = Lens.lensu(
      set = (r: Release, pm: Map[Int,Publisher]) => r.copy(publishers = pm.toList.sortBy { _._1 }.map { _._2})
      , get = (r: Release) => r.publishers.zipWithIndex.map { x => (x._2, x._1)}.toMap
      )

    val fields = List(
      BasicField[Release]("title", Lens.lensu[Release,String]((r,t) => r.copy(title = t), (r) => r.title))
    , RecordField[Release, Artist]("artist", Lens.lensu[Release,Artist]((r,a) => r.copy(artist = a), (r) => r.artist))
    , ManyRecordField[Release, Publisher]("publisher", releasePublishersLens) 
    )
  }
}

object Main extends App {
  override def main(args: Array[String]) = {
    import Release._
    println(ReleaseRecord.nodeSeqTransformer)
  }
}

