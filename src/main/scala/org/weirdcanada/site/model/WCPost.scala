package org.weirdcanada.site.model

// scala
import scala.xml.{Elem, NodeSeq, Text, Unparsed}

// weirdcanada
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty, HasFields, ManyRecordField, RecordField}

// scalaz
import scalaz.Lens

// Lift
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

// 3rd Party
import org.clapper.markwrap.{MarkupType, MarkWrap}
import org.joda.time.DateTime

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
  val postReleaseLens: Lens[Post,Release] = Lens.lensu( (p,r) => p.copy(release = r), (p) => p.release)
  val postAuthorsMapLens: Lens[Post, Map[Int, Author]] = Lens.lensu(
    set = (p: Post, am: Map[Int,Author]) => p.copy(authors = am.toList.sortBy { _._1 }.map { _._2})
  , get = (p: Post) => p.authors.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  val postAuthorsLens: Lens[Post, List[Author]] = Lens.lensu(
    set = (p: Post, as: List[Author]) => p.copy(authors = as)
  , get = (p: Post) => p.authors
  )
  val postTranslatorsMapLens: Lens[Post, Map[Int, Translator]] = Lens.lensu(
    set = (p: Post, tm: Map[Int,Translator]) => p.copy(translators = tm.toList.sortBy { _._1 }.map { _._2})
  , get = (p: Post) => p.translators.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  val postTranslatorsLens: Lens[Post, List[Translator]] = Lens.lensu(
    set = (p: Post, ts: List[Translator]) => p.copy(translators = ts)
  , get = (p: Post) => p.translators
  )
  val postTranslatorTextLens: Lens[Post, String] = Lens.lensu( (p, tt) => p.copy(translatorText = tt), (p) => p.translatorText)
  val postFromTheLens: Lens[Post, String] = Lens.lensu( (p,ft) => p.copy(fromThe = ft), (p) => p.fromThe)
  val postContentEnglishLens: Lens[Post, String] = Lens.lensu( (p,ce) => p.copy(contentEnglish = ce), (p) => p.contentEnglish)
  val postDeLaLens: Lens[Post, String] = Lens.lensu( (p,dl) => p.copy(deLa = dl), (p) => p.deLa)
  val postContentFrenchLens: Lens[Post, String] = Lens.lensu( (p,cf) => p.copy(contentFrench = cf), (p) => p.contentFrench)

  private def textAreaRender(selector: String)(filler: String)(updateFunc: String => JsCmd): NodeSeq => NodeSeq =
    selector #> SHtml.ajaxTextarea("", updateFunc, "placeholder" -> filler)

  private val englishRender = textAreaRender("name=content-english-input")("English Content") _
  private val frenchRender = textAreaRender("name=content-french-input")("French Content") _

  implicit object PostRecord extends HasFields[Post] {
    val fields: List[DynamicField[Post]] = List(
      RecordField[Post, Release]("release", postReleaseLens)
    , ManyRecordField[Post, Author]("author", postAuthorsMapLens)
    , ManyRecordField[Post, Translator]("translator", postTranslatorsMapLens)
    , BasicField[Post]("translator-text", postTranslatorTextLens)
    , BasicField[Post]("from-the-english", postFromTheLens)
    , BasicField[Post]("content-english", postContentEnglishLens, Some(englishRender) )
    , BasicField[Post]("de-la-french", postDeLaLens)
    , BasicField[Post]("content-french", postContentFrenchLens, Some(frenchRender) )
    )
  }

  private val markdownParser = MarkWrap.parserFor(MarkupType.Markdown)
  private val postArtistsLens = postReleaseLens >=> Release.releaseArtistsLens
  private val postPublishersLens = postReleaseLens >=> Release.releasePublishersLens
  private val postTitleLens = postReleaseLens >=> Release.releaseTitleLens

  def renderAsXml(post: Post): NodeSeq = {
    val artistsString = postArtistsLens.get(post).map { Artist.artistNameLens.get }.mkString(" // ")
    val cities = postArtistsLens.get(post).map { Artist.artistCityLens.get }
    val provinces = postArtistsLens.get(post).map { Artist.artistProvinceLens.get }
    val geoString = (cities zip provinces).map { case (c,p) => "%s, %s".format(c,p) }.mkString(" // ")
    val publishersXml = postPublishersLens.get(post).map { Publisher.renderAsXml }
    val webSounds = 
      postArtistsLens
        .get(post)
        .map { Artist.artistUrlLens.get }
        .map { url => if(url.isEmpty) Text(url) else <a href={url} target="_blank">::web/sounds::</a> }
        .foldLeft(NodeSeq.Empty){ case (a,b) => a ++ Text(" // ") ++ b } match {
          case NodeSeq.Empty => NodeSeq.Empty
          case xs => xs.tail
        }

    val dateStamp = (new DateTime).getMillis()

    val authors = 
      postAuthorsLens
        .get(post)
        .map { Author.renderAsXml }
        .foldLeft(NodeSeq.Empty){ case (a,b) => a ++ Text(" and ") ++ b } match {
          case NodeSeq.Empty => NodeSeq.Empty
          case xs => xs.tail
        }
    val translators = 
      postTranslatorsLens
        .get(post)
        .map { Translator.renderAsXml }
        .foldLeft(NodeSeq.Empty){ case(a,b) => a ++ Text(" and ") ++ b } match {
          case NodeSeq.Empty => NodeSeq.Empty
          case xs => xs.tail
        }

<div class="contentContainer">
  <div class="contentImage">
  IMAGE
  </div>
  <div class="contentInfo">
    <ul>
      <li class="contentArtist">{ artistsString }</li>
      <li class="contentTitle">{ postTitleLens.get(post) }</li>
      <li class="contentPublisher">({ publishersXml })</li>
      <li class="contentCity">{ geoString }</li>
      <li class="contentWebSounds">{ webSounds }</li>
    </ul>
  </div>
</div>
<div class="contentReview">
  <ul id="languageTab" class="nav nav-tabs">
    <li class="active"><a href={"#english-%s".format(dateStamp)} data-toggle="tab" onclick="_gaq.push([_trackEvent, posts, translation-toggle, english]);">english</a></li>
    <li><a href={"#francais-%s".format(dateStamp)} data-toggle="tab" onclick="_gaq.push([_trackEvent, posts, translation-toggle, francais]);">français</a></li>
  </ul>
  <div id="languageTabContent" class="tab-content">
    <div class="tab-pane fade in active" id={"english-%s".format(dateStamp)}>
      <p class="contentAuthor">{Text(post.fromThe + " ") ++ authors}:</p>
      <p>
        { if(postContentEnglishLens.get(post).isEmpty) Text("") else Unparsed(markdownParser.parseToHTML(postContentEnglishLens.get(post))) }
      </p>
    </div>
    <div class="tab-pane fade" id={"francais-%s".format(dateStamp)}>
      <p class="contentAuthor">{Text(postDeLaLens.get(post) + " ") ++ authors}:
      (<em>{Text(postTranslatorTextLens.get(post) + " ") ++ translators}</em>)</p>
      <p>
        { if(postContentFrenchLens.get(post).isEmpty) Text("") else Unparsed(markdownParser.parseToHTML(postContentFrenchLens.get(post))) }
      </p>
    </div>
  </div>
</div>
  }
}

