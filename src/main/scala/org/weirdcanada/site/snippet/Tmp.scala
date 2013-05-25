package org.weirdcanada.site.model
import scala.reflect.runtime.universe._

// Scala
import scalaz.Lens
import Lens.{mapVLens => xxx, lensId}
import scalaz.syntax._

// Weird Canada
import org.weirdcanada.site.lib.{ApplyOnce}
import org.weirdcanada.site.snippet.{DynamicFormHelpers}

// Lift
import net.liftweb._
import http._
import js.{JsCmd, JsCmds}
import JsCmds.{After, Alert, Replace, Run, Noop}
import common._
import util.Helpers._

// Scala
import scala.xml.{NodeSeq, Text, Elem}
import scala.annotation.tailrec

object Cool {
  def mapVLens[K,V](index: K): Lens[Map[K,V],Option[V]] = Lens.lensu(
    set = (m: Map[K,V], ov: Option[V]) => { println("mapVLens set(%s -- %s)".format(m,ov)); ov match {
      case None => m - index
      case Some(v) => m.updated(index,v)
    }}
  , get = (m: Map[K,V]) => {println("mapVLens get: %s".format(m.get(index))); m.get(index)}
  )
}
import Cool._

trait DynamicFormCreator extends DynamicFormHelpers { 
  println("\n")

  import DynamicField.{liftLens}

  /** updateStateFunction: (A => String => A) => (() => JsCmd) => (String => JsCmd)
   */
  def renderField[A : HasFields](formState: RequestVar[A]): NodeSeq => NodeSeq = {
    val record = implicitly[HasFields[A]]
    val updateStateFunction = getUpdateAndSaveFuncForDynamicForm[A](formState)
    record
      .fields
      .foldLeft( (ns: NodeSeq) => ns){ (acc, field) => 
        acc andThen field.render(updateStateFunction)(liftLens(lensId[A]), None)
      }
  }
}

trait HasFields[A] {
  val fields: List[DynamicField[A]]
}

trait HasEmpty[A] {
  val empty: A
}

/**
 * TODO: Does the lens returned from lensReducer obey the lens laws}
 */
object DynamicField {

  type FormStateUpdate[A] = (Option[A] => String => Option[A]) => (() => JsCmd) => (String => JsCmd)

  def label(outerName: Option[String], name: String): String =
    "%s%s".format(outerName.map{ _ + "-"}.getOrElse(""), name)

  def makeName(outerName: Option[String], name: String): String =
    "name=%s".format(label(outerName, name))

  def makeNameAdd(outerName: Option[String], name: String): String =
    "name=add-%s".format(label(outerName, name))

  def makeAdd(outerName: Option[String], name: String): String =
    "add-%s".format(label(outerName, name))

  def makeInput(outerName: Option[String], name: String): String =
    "%s-input".format(makeName(outerName, name))

  def optionLens[A : HasEmpty]: Lens[Option[A],A] = Lens.lensu(
    get = (oa) => oa.getOrElse( implicitly[HasEmpty[A]].empty )
  , set = (oa,a) => Some(a)
  )

  def liftLens[A,B](lens: Lens[B,A]): Lens[Option[B], Option[A]] = {
    Lens.lensu(
      get = (ob) => ob.map { lens.get } 
    , set = (ob,oa) => { println("liftLens: set(%s -- %s)".format(ob, oa)); for {
      b <- ob
      a <- oa
    } yield {lens.set(b,a)} }
    )
  }

  def liftLensA[A,B](lens: Lens[A,Option[B]]): Lens[Option[A],Option[B]] = 
    Lens.lensu(
      get = (oa) => oa.flatMap { lens.get }
    , set = (oa, ob) => {println("liftLensA: set(%s -- %s)".format(oa, ob)); oa.map { a =>  lens.set(a,ob) } }
    )
}

sealed trait DynamicField[A] {
  import DynamicField.{makeName,liftLens,FormStateUpdate}
  val name: String
  //def render[B](outerLens: Lens[Option[B],Option[A]], outerName: Option[String]): NodeSeq => NodeSeq
  def render[B](formStateUpdater: FormStateUpdate[B])(outerLens: Lens[Option[B],Option[A]], outerName: Option[String]): NodeSeq => NodeSeq
}

case class BasicField[A](name: String, lens: Lens[A,String]) extends DynamicField[A] {
  import DynamicField.{makeName,makeNameAdd,makeInput, FormStateUpdate, liftLens, label}
  def render[B](formStateUpdater: FormStateUpdate[B])(outerLens: Lens[Option[B],Option[A]], outerName: Option[String] = None): NodeSeq => NodeSeq = {
    // I cry to hav to call .get at the end of this, but it, theoretically, should be safe. 
    def updateFunc(stateOption: Option[B])(inputString: String): Option[B] = {
      println("updateFunc: set(%s -- %s)".format(stateOption, Some(inputString)))
      println("outerLens(state) = %s".format(outerLens.get(stateOption)))
      (outerLens >=> liftLens(lens)).set(stateOption, Some(inputString))
    }

    val jsCmd = () => Noop
    val fieldUpdateFunc: String => JsCmd = formStateUpdater(updateFunc)(jsCmd)
    makeInput(None, name) #> SHtml.ajaxText("", fieldUpdateFunc, "placeholder" -> label(None, name))
  }
}

case class RecordField[A, B : HasFields](name: String, lens: Lens[A,B]) extends DynamicField[A] {
  import DynamicField.{makeNameAdd,liftLens,FormStateUpdate}
  val bRecord = implicitly[HasFields[B]]
  def render[C](formStateUpdater: FormStateUpdate[C])(outerLens: Lens[Option[C],Option[A]], outerName: Option[String]): NodeSeq => NodeSeq = {
    // Checked. this actually triggers "name=release" (checked by replacing with 'cool')
    makeNameAdd(None, name) #> 
      bRecord
        .fields
        .foldLeft( (ns: NodeSeq) => ns ){ (acc, field) => acc andThen field.render(formStateUpdater)(outerLens andThen liftLens(lens), Some(name)) }
  }
}

case class ManyRecordField[A, B : HasFields : HasEmpty](name: String, lens: Lens[A, Map[Int,B]]) extends DynamicField[A] {
  import DynamicField.{makeAdd, makeName, makeNameAdd, label, liftLensA,FormStateUpdate}
  val bRecord = implicitly[HasFields[B]]
  val bEmpty = implicitly[HasEmpty[B]]

  // Primary render method
  def render[C](formStateUpdater: FormStateUpdate[C])(outerLens: Lens[Option[C],Option[A]], outerName: Option[String]): NodeSeq => NodeSeq = {

    def lensAtIndex(index: Int): Lens[Option[C], Option[B]] = outerLens andThen liftLensA(lens andThen mapVLens(index))

    // Method to add a new record, dynamically.
    def addNewRecordForm(index: Int): () => JsCmd = { () =>
      Replace(
        "%s-elements".format(label(outerName, name)), 
        renderAtIndex(index)( addRecordMemoize.applyAgain() ) ++ <div id={"%s-elements".format(label(outerName,name))}></div>
      )
    }

    def removeRecordFromState(index: Int)(stateOption: Option[C])(inputString: String): Option[C] = lensAtIndex(index).set(stateOption, None)

    def removeRecordFromForm(index: Int): () => JsCmd = { () =>
      val jsCmd = () => Replace("%s-%s".format(makeNameAdd(outerName, name), index), Nil)
      val fieldUpdateFunc: String => JsCmd = formStateUpdater( removeRecordFromState(index) )( jsCmd )
    }

    
    def renderAtIndex(index: Int): NodeSeq => NodeSeq = {
      "%s [id]".format(makeNameAdd(None, name)) #> "%s-%s".format(makeAdd(outerName, name), index) andThen
      makeName(None, "%s-number".format(name)) #> (index+1) andThen
      bRecord
        .fields
        .foldLeft( (ns: NodeSeq) => ns ){ (acc, field) => 
           val newLens: Lens[Option[C], Option[B]] = lensAtIndex(index)
           acc andThen field.render(formStateUpdater)(newLens, outerName)
         } andThen
      makeName(None, "%s-add [onclick]".format(name)) #> SHtml.onEvent( (s: String) => addNewRecordForm(index+1)() ) & 
      makeName(None, "%s-remove [onclick]".format(name)) #> SHtml.onEvent( (s: String) => removeRecordFromForm(index)() ) 
    }

   // made lazy to avoid: http://stackoverflow.com/questions/13328502/what-does-forward-reference-extends-over-definition-of-value-mean-in-scala
    lazy val addRecordMemoize = SHtml.memoize( renderAtIndex(0) )

    // This is making the right selection, has been tested. 
    makeNameAdd(None, name) #> addRecordMemoize &
    "#%s-elements [id]".format(label(None, name)) #> "%s-elements".format(label(outerName, name))
  }

}

case class Author(name: String, url: String)
object Author {
  private val authorNameLens: Lens[Author, String] = Lens.lensu( (a, n) => a.copy(name = n), (a) => a.name )
  private val authorUrlLens: Lens[Author,String] = Lens.lensu( (a, u) => a.copy(url = u), (a) => a.url )

  implicit object AuthorRecord extends HasFields[Author] {
    val fields: List[DynamicField[Author]] = List(
      BasicField[Author]("author-name", authorNameLens)
    , BasicField[Author]("author-url", authorUrlLens)
    )
  }
} 
case class Translator(name: String, url: String)
object Translator {
  private val translatorNameLens: Lens[Translator, String] = Lens.lensu( (t, n) => t.copy(name = n), (t) => t.name )
  private val translatorUrlLens: Lens[Translator, String] = Lens.lensu( (t, u) => t.copy(url = u), (t) => t.url )

  implicit object TranslatorRecord extends HasFields[Translator] {
    val fields: List[DynamicField[Translator]] = List(
      BasicField[Translator]("translator-name", translatorNameLens)
    , BasicField[Translator]("translator-url", translatorUrlLens)
    )
  }
}
case class Artist(name: String, url: String, city: String, province: String)
object Artist {
  private val artistNameLens: Lens[Artist, String] = Lens.lensu( (a, n) => {println("artistNameLens: set(%s)".format(n)); a.copy(name = n)}, (a) => a.name )
  private val artistUrlLens: Lens[Artist, String] = Lens.lensu( (a, u) => a.copy(url = u), (a) => a.url )
  private val artistCityLens: Lens[Artist, String] = Lens.lensu( (a, c) => a.copy(city = c), (a) => a.city )
  private val artistProvinceLens: Lens[Artist, String] = Lens.lensu( (a, p) => a.copy(province = p), (a) => a.province )

  implicit object ArtistRecord extends HasFields[Artist] {
    val fields: List[DynamicField[Artist]] = List(
      BasicField[Artist]("artist-name", artistNameLens)
    , BasicField[Artist]("artist-url", artistUrlLens)
    , BasicField[Artist]("artist-city", artistCityLens)
    , BasicField[Artist]("artist-province", artistProvinceLens)
    )
  }
}
case class Publisher(name: String, url: String, city: String, province: String)

object Publisher {
  private val publisherNameLens: Lens[Publisher, String] = Lens.lensu( (p, n) => p.copy(name = n), (p) => p.name )
  private val publisherUrlLens: Lens[Publisher, String] = Lens.lensu( (p, u) => p.copy(url = u), (p) => p.url )
  private val publisherCityLens: Lens[Publisher, String] = Lens.lensu( (p, c) => p.copy(city = c), (p) => p.city )
  private val publisherProvinceLens: Lens[Publisher, String] = Lens.lensu( (p, pr) => p.copy(province = pr), (p) => p.province )

  implicit object PublisherRecord extends HasFields[Publisher] {
    val fields: List[DynamicField[Publisher]] = List(
      BasicField[Publisher]("publisher-name", publisherNameLens)
    , BasicField[Publisher]("publisher-url", publisherUrlLens)
    , BasicField[Publisher]("publisher-city", publisherCityLens)
    , BasicField[Publisher]("publisher-province", publisherProvinceLens)
    )
  }
}
case class Release(title: String, artists: List[Artist], publishers: List[Publisher], format: String, releaseDate: String)
object Release {
  private val releaseTitleLens: Lens[Release, String] = Lens.lensu( (r,t) => r.copy(title = t), (r) => r.title)
  private val releaseArtistsLens: Lens[Release, Map[Int,Artist]] = Lens.lensu(
    set = (r: Release, am: Map[Int,Artist]) => {println("releaseArtistsLens: set(%s)".format(am));r.copy(artists = am.toList.sortBy { _._1 }.map { _._2})}
  , get = (r: Release) => r.artists.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  private val releasePublishersLens: Lens[Release, Map[Int,Publisher]] = Lens.lensu(
    set = (r: Release, pm: Map[Int,Publisher]) => r.copy(publishers = pm.toList.sortBy { _._1 }.map { _._2})
  , get = (r: Release) => r.publishers.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  private val releaseFormatLens: Lens[Release, String] = Lens.lensu( (r,f) => r.copy(format = f), (r) => r.format)
  private val releaseReleaseDateLens: Lens[Release, String] = Lens.lensu( (r,d) => r.copy(releaseDate = d), (r) => r.releaseDate)

  implicit object ReleaseRecord extends HasFields[Release] {
    val fields: List[DynamicField[Release]] = List(
      BasicField[Release]("release-title", releaseTitleLens)
    , ManyRecordField[Release, Artist]("artist", releaseArtistsLens)
    , ManyRecordField[Release, Publisher]("publisher", releasePublishersLens)
    , BasicField[Release]("release-format", releaseFormatLens)
    , BasicField[Release]("release-release-date", releaseReleaseDateLens)
    )
  }
}

case class Post(
  release: Release,
  authors: List[Author],
  translators: List[Translator], 
  translatorText: String,
  fromThe: String, 
  contentEnglish: String, 
  deLa: String, 
  contentFrench: String
)
object Post {
  private val postReleaseLens: Lens[Post,Release] = Lens.lensu( (p,r) => p.copy(release = r), (p) => p.release)
  private val postAuthorsLens: Lens[Post, Map[Int, Author]] = Lens.lensu(
    set = (p: Post, am: Map[Int,Author]) => p.copy(authors = am.toList.sortBy { _._1 }.map { _._2})
  , get = (p: Post) => p.authors.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  private val postTranslatorsLens: Lens[Post, Map[Int, Translator]] = Lens.lensu( 
    set = (p: Post, tm: Map[Int,Translator]) => p.copy(translators = tm.toList.sortBy { _._1 }.map { _._2})
  , get = (p: Post) => p.translators.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  private val postFromTheLens: Lens[Post, String] = Lens.lensu( (p,ft) => p.copy(fromThe = ft), (p) => p.fromThe)
  private val postContentEnglishLens: Lens[Post, String] = Lens.lensu( (p,ce) => p.copy(contentEnglish = ce), (p) => p.contentEnglish)
  private val postDeLaLens: Lens[Post, String] = Lens.lensu( (p,dl) => p.copy(deLa = dl), (p) => p.deLa)
  private val postContentFrenchLens: Lens[Post, String] = Lens.lensu( (p,cf) => p.copy(contentFrench = cf), (p) => p.contentFrench)

  implicit object PostRecord extends HasFields[Post] {
    val fields: List[DynamicField[Post]] = List(
      RecordField[Post, Release]("release", postReleaseLens)
    , ManyRecordField[Post, Author]("author", postAuthorsLens)
    , ManyRecordField[Post, Translator]("translator", postTranslatorsLens)
    , BasicField[Post]("from-the", postFromTheLens)
    , BasicField[Post]("content-english", postContentEnglishLens)
    , BasicField[Post]("de-la", postDeLaLens)
    , BasicField[Post]("content-french", postContentFrenchLens)
    )
  }
}

object Main extends App {
  override def main(args: Array[String]) = {
    import Release._
    println("")
    //println(ReleaseRecord.nodeSeqTransformer)
  }
}

