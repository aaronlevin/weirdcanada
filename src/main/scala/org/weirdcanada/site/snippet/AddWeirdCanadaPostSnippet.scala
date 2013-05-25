package org.weirdcanada.site.snippet

// Weird Canada
import org.weirdcanada.site.lib.{ApplyOnce}

// Lift
import net.liftweb._
import http._
import js.{JsCmd, JsCmds}
import JsCmds.{After, Alert, Run}
import common._
import util.Helpers._

// Scala
import scala.xml.{NodeSeq, Text, Elem, Unparsed, XML}
import scala.annotation.tailrec
import org.xml.sax.SAXParseException

// 3rd Party
import org.clapper.markwrap.{MarkupType, MarkWrap}
import org.joda.time.DateTime
import scalaz.Lens

trait Renderable {
  def renderAsXml: NodeSeq
  def renderAsMarkdown: String
}

case class Author(name: String, url: String) extends Renderable {
  def renderAsMarkdown: String = if(url.isEmpty) name else """[%s](%s)""".format(name, url)
  def renderAsXml: NodeSeq = if(url.isEmpty) Text(name) else <a href={url} target="_blank">{name}</a>
}
case class Translator(name: String, url: String) extends Renderable {
  def renderAsMarkdown: String = if(url.isEmpty) name else """[%s](%s)""".format(name, url)
  def renderAsXml: NodeSeq = if(url.isEmpty) Text(name) else <a href={url} target="_blank">{name}</a>
}
case class Artist(name: String, url: String, city: String, province: String) extends Renderable {
  def renderAsMarkdown: String = if(url.isEmpty) name else  """[%s](%s)""".format(name, url)
  def renderAsXml: NodeSeq = if(url.isEmpty) Text(name) else <a href={url} target="_blank">{name}</a>
}
case class Publisher(name: String, url: String, city: String, province: String) extends Renderable {
  def renderAsMarkdown: String = if(url.isEmpty) name else  """[%s](%s)""".format(name, url)
  def renderAsXml: NodeSeq = if(url.isEmpty) Text(name) else <a href={url} target="_blank">{name}</a>
}
case class Release(title: String, artists: List[Artist], publishers: List[Publisher], format: String, releaseDate: String)
case class Post(
  release: Release,
  authors: List[Author],
  translators: List[Translator], 
  translatorText: String,
  fromThe: String, 
  contentEnglish: String, 
  deLa: String, 
  contentFrench: String
) extends Renderable {

  import SupportLenses._

  def renderAsXml = {
    val artistsString = postArtistsLens.get(this).map { artistNameLens.get }.mkString(" // ")
    val cities = postArtistsLens.get(this).map { artistCityLens.get }
    val provinces = postArtistsLens.get(this).map { artistProvinceLens.get }
    val geoString = (cities zip provinces).map { case (c,p) => "%s, %s".format(c,p) }.mkString(" // ")
    val publishersXml = postPublishersLens.get(this).map { _.renderAsXml }
    val webSounds = 
      postArtistsLens
        .get(this)
        .map { artistUrlLens.get }
        .map { url => if(url.isEmpty) Text(url) else <a href={url} target="_blank">::web/sounds::</a> }
        .foldLeft(NodeSeq.Empty){ case (a,b) => a ++ Text(" // ") ++ b } match {
          case NodeSeq.Empty => NodeSeq.Empty
          case xs => xs.tail
        }

    val dateStamp = (new DateTime).getMillis()

    val authors = 
      postAuthorsLens
        .get(this)
        .map { _.renderAsXml }
        .foldLeft(NodeSeq.Empty){ case (a,b) => a ++ Text(" and ") ++ b } match {
          case NodeSeq.Empty => NodeSeq.Empty
          case xs => xs.tail
        }
    val translators = 
      postTranslatorsLens
        .get(this)
        .map { _.renderAsXml }
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
    <li class="contentTitle">{ postTitleLens.get(this) }</li>
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
    <p class="contentAuthor">{Text(fromThe + " ") ++ authors}:</p>
    <p>
    { if(contentEnglish.isEmpty) Text("") else Unparsed(Post.markdownParser.parseToHTML(contentEnglish)) }
    </p>
    </div>
    <div class="tab-pane fade" id={"francais-%s".format(dateStamp)}>
    <p class="contentAuthor">{Text(deLa + " ") ++ authors}:
    (<em>{Text(translatorText + " ") ++ translators}</em>)</p>
    <p>
    { if(contentFrench.isEmpty) Text("") else Unparsed(Post.markdownParser.parseToHTML(contentFrench)) }
    </p>
    </div>
    </div>
    </div>
  }

  def renderAsMarkdown = ""

}

/**
 * Very likely we'll migrate from these case classes to something else.
 * So, let's setup some lenses
 */
object SupportLenses {


  // views on a release
  val releaseTitleLens = Lens.lensu( (r: Release, newTitle: String) => r.copy(title = newTitle), (r: Release) => r.title)
  val releaseArtistsLens = Lens.lensu( (r: Release, newArtists: List[Artist]) => r.copy(artists = newArtists), (r: Release) => r.artists)
  val releasePublishersLens = Lens.lensu( (r: Release, newPublishers: List[Publisher]) => r.copy(publishers = newPublishers), (r: Release) => r.publishers)
  val releaseFormatLens = Lens.lensu( (r: Release, newFormat: String) => r.copy(format = newFormat), (r: Release) => r.format)
  val releaseReleaseDateLens = Lens.lensu( (r: Release, newDate: String) => r.copy(releaseDate = newDate), (r: Release) => r.releaseDate)


  // views on an artist
  val artistNameLens = Lens.lensu( (artist: Artist, newName: String) => artist.copy(name = newName), (artist: Artist) => artist.name )
  val artistUrlLens = Lens.lensu( (artist: Artist, newUrl: String) => artist.copy(url = newUrl), (artist: Artist) => artist.url )
  val artistCityLens = Lens.lensu( (artist: Artist, newCity: String) => artist.copy(city = newCity), (artist: Artist) => artist.city )
  val artistProvinceLens = Lens.lensu( (artist: Artist, newProvince: String) => artist.copy(province = newProvince), (artist: Artist) => artist.province )

  // views on an publisher
  val publisherNameLens = Lens.lensu( (publisher: Publisher, newName: String) => publisher.copy(name = newName), (publisher: Publisher) => publisher.name )
  val publisherUrlLens = Lens.lensu( (publisher: Publisher, newUrl: String) => publisher.copy(url = newUrl), (publisher: Publisher) => publisher.url )
  val publisherCityLens = Lens.lensu( (publisher: Publisher, newCity: String) => publisher.copy(city = newCity), (publisher: Publisher) => publisher.city )
  val publisherProvinceLens = Lens.lensu( (publisher: Publisher, newProvince: String) => publisher.copy(province = newProvince), (publisher: Publisher) => publisher.province )

  // views on an author
  val authorNameLens = Lens.lensu( (author: Author, newName: String) => author.copy(name = newName), (author: Author) => author.name )
  val authorUrlLens = Lens.lensu( (author: Author, newUrl: String) => author.copy(url = newUrl), (author: Author) => author.url )

  // views on an translator
  val translatorNameLens = Lens.lensu( (translator: Translator, newName: String) => translator.copy(name = newName), (translator: Translator) => translator.name )
  val translatorUrlLens = Lens.lensu( (translator: Translator, newUrl: String) => translator.copy(url = newUrl), (translator: Translator) => translator.url )

  // views on a post
  val postReleaseLens = Lens.lensu( (post: Post, newRelease: Release) => post.copy(release = newRelease), (post: Post) => post.release)
  val postArtistsLens = postReleaseLens andThen releaseArtistsLens 
  val postPublishersLens = postReleaseLens andThen releasePublishersLens 
  val postTitleLens = postReleaseLens andThen releaseTitleLens
  val postFormatLens = postReleaseLens andThen releaseFormatLens
  val postReleaseDateLens = postReleaseLens andThen releaseReleaseDateLens
  val postAuthorsLens = Lens.lensu( (post: Post, newAuthors: List[Author]) => post.copy(authors = newAuthors), (post: Post) => post.authors)
  val postTranslatorsLens = Lens.lensu( (post: Post, newTranslators: List[Translator]) => post.copy(translators = newTranslators), (post: Post) => post.translators)
  val postTranslatorTextLens = Lens.lensu( (post: Post, text: String) => post.copy(translatorText = text), (post: Post) => post.translatorText)
  val postFromTheLens = Lens.lensu( (p: Post, text: String) => p.copy(fromThe = text), (post: Post) => post.fromThe)
  val postDeLaLens = Lens.lensu( (p: Post, text: String) => p.copy(deLa = text), (p: Post) => p.deLa)
  val postContentEnglishLens = Lens.lensu( (p: Post, text: String) => p.copy(contentEnglish = text), (p: Post) => p.contentEnglish)
  val postContentFrenchLens = Lens.lensu( (p: Post, text: String) => p.copy(contentFrench = text), (p: Post) => p.contentFrench)

}


/**
 * Note: break out of this hell and use a state monad for god sake
 */

// Companion object to post that contains update functions for Post state
object Post {

  import SupportLenses._

  val markdownParser = MarkWrap.parserFor(MarkupType.Markdown)

  // Update artist name for a given index
  def updateArtistName(state: Post, text: String, index: Int): Post = {
    val oldArtist = postArtistsLens.get(state)(index)
    val newArtists = postArtistsLens.get(state).updated(index, artistNameLens.set(oldArtist, text))
    postArtistsLens.set(state, newArtists)
  }
  // Update artist URL for a given index
  def updateArtistUrl(state: Post, text: String, index: Int): Post = {
    val oldArtist = postArtistsLens.get(state)(index)
    val newArtists = postArtistsLens.get(state).updated(index, artistUrlLens.set(oldArtist, text))
    postArtistsLens.set(state, newArtists)
  }
  // Update artist City for a given index
  def updateArtistCity(state: Post, text: String, index: Int): Post = {
    val oldArtist = postArtistsLens.get(state)(index)
    val newArtists = postArtistsLens.get(state).updated(index, artistCityLens.set(oldArtist, text))
    postArtistsLens.set(state, newArtists)
  }
  // Update artist Province for a given index
  def updateArtistProvince(state: Post, text: String, index: Int): Post = {
    val oldArtist = postArtistsLens.get(state)(index)
    val newArtists = postArtistsLens.get(state).updated(index, artistProvinceLens.set(oldArtist, text))
    postArtistsLens.set(state, newArtists)
  }
  // Update publisher 
  def updatePublisherName(state: Post, text: String, index: Int): Post = {
    val oldPublisher = postPublishersLens.get(state)(index)
    val newPublishers = postPublishersLens.get(state).updated(index, publisherNameLens.set(oldPublisher, text))
    postPublishersLens.set(state, newPublishers)
  }
  // Update publisher URL for a given index
  def updatePublisherUrl(state: Post, text: String, index: Int): Post = {
    val oldPublisher = postPublishersLens.get(state)(index)
    val newPublishers = postPublishersLens.get(state).updated(index, publisherUrlLens.set(oldPublisher, text))
    postPublishersLens.set(state, newPublishers)
  }
  // Update publisher City for a given index
  def updatePublisherCity(state: Post, text: String, index: Int): Post = {
    val oldPublisher = postPublishersLens.get(state)(index)
    val newPublishers = postPublishersLens.get(state).updated(index, publisherCityLens.set(oldPublisher, text))
    postPublishersLens.set(state, newPublishers)
  }
  // Update publisher Province for a given index
  def updatePublisherProvince(state: Post, text: String, index: Int): Post = {
    val oldPublisher = postPublishersLens.get(state)(index)
    val newPublishers = postPublishersLens.get(state).updated(index, publisherProvinceLens.set(oldPublisher, text))
    postPublishersLens.set(state, newPublishers)
  }
  // Update author Name for a given index
  def updateAuthorName(state: Post, text: String, index: Int): Post = {
    val oldAuthor = postAuthorsLens.get(state)(index)
    val newAuthors = postAuthorsLens.get(state).updated(index, authorNameLens.set(oldAuthor, text))
    postAuthorsLens.set(state, newAuthors)
  }
  // Update author Url for a given index
  def updateAuthorUrl(state: Post, text: String, index: Int): Post = {
    val oldAuthor = postAuthorsLens.get(state)(index)
    val newAuthors = postAuthorsLens.get(state).updated(index, authorUrlLens.set(oldAuthor, text))
    postAuthorsLens.set(state, newAuthors)
  }
  // Update translator Name for a given index
  def updateTranslatorName(state: Post, text: String, index: Int): Post = {
    val oldTranslator = postTranslatorsLens.get(state)(index)
    val newTranslators = postTranslatorsLens.get(state).updated(index, translatorNameLens.set(oldTranslator, text))
    postTranslatorsLens.set(state, newTranslators)
  }
  // Update translator Url for a given index
  def updateTranslatorUrl(state: Post, text: String, index: Int): Post = {
    val oldTranslator = postTranslatorsLens.get(state)(index)
    val newTranslators = postTranslatorsLens.get(state).updated(index, translatorUrlLens.set(oldTranslator, text))
    postTranslatorsLens.set(state, newTranslators)
  }
  // Update Translator Text on a post
  def updatePostTranslatorText(state: Post)(text: String): Post = {
    postTranslatorTextLens.set(state, text)
  }
  // Update english content on a post
  def updatePostContentEnglish(state: Post)(text: String): Post = {
    postContentEnglishLens.set(state, text)
  }
  // Update the "From the" on a post
  def updatePostFromThe(state: Post)(text: String): Post = {
    postFromTheLens.set(state, text)
  }
  // Update french content on a post
  def updatePostContentFrench(state: Post)(text: String): Post = {
    postContentFrenchLens.set(state, text)
  }
  // Update the "De La" on a post
  def updatePostDeLa(state: Post)(text: String): Post = {
    postDeLaLens.set(state, text)
  }
  // Update release title on a post
  def updatePostReleaseTitle(state: Post)(text: String): Post = {
    postTitleLens.set(state, text)
  }
  // Update release format on a post
  def updatePostReleaseFormat(state: Post)(text: String): Post = {
    postFormatLens.set(state, text)
  }
  // Update release releaseDate on a post
  def updatePostReleaseReleaseDate(state: Post)(text: String): Post = {
    postReleaseDateLens.set(state, text)
  }

  /** Append methods **/

  // Append a "blank" artist
  def appendBlankArtist(state: Post): Post = 
    postArtistsLens.set(state, postArtistsLens.get(state) ::: Artist("","","","") :: Nil )
  // Append a "blank" publisher
  def appendBlankPublisher(state: Post): Post = 
    postPublishersLens.set(state, postPublishersLens.get(state) ::: Publisher("","","","") :: Nil )
  // Append a "blank" author
  def appendBlankAuthor(state: Post): Post = 
    postAuthorsLens.set(state, postAuthorsLens.get(state) ::: Author("","") :: Nil )
  // Append a "blank" translator
  def appendBlankTranslator(state: Post): Post = 
    postTranslatorsLens.set(state, postTranslatorsLens.get(state) ::: Translator("","") :: Nil )

  /**
   * Method used to create a function to create a new artist row given a index
   */
  @tailrec
  def rowUpdateFunc[A](lens: Lens[Post, List[A]])(updateFunc: (Post, String, Int) => Post)(incState: (Post) => Post)(index: Int)(state: Post)(inputText: String): Post = {
    lens.get(state).length match {
      case length if length <= index => rowUpdateFunc(lens)(updateFunc)(incState)(index)( incState(state) )(inputText)
      case _ => 
        updateFunc(state, inputText, index)
    }
  }

  // Remove a row
  def removeArtist(index: Int)(state: Post)(inputText: String): Post = {
    val artists = postArtistsLens.get(state)
    artists.length match {
      case 0 => state
      case stateLength => 
        val (unaffected, indexReduceNeeded) = artists.splitAt(index)
        postArtistsLens.set(state, unaffected ::: indexReduceNeeded.tail)
    }
  }
  def removePublisher(index: Int)(state: Post)(inputText: String): Post = {
    val publishers = postPublishersLens.get(state)
    publishers.length match {
      case 0 => state
      case stateLength =>
        val (unaffected, indexReduceNeeded) = publishers.splitAt(index)
        postPublishersLens.set(state, unaffected ::: indexReduceNeeded.tail)
    }
  }
  def removeAuthor(index: Int)(state: Post)(inputText: String): Post = {
    val authors = postAuthorsLens.get(state)
    authors.length match {
      case 0 => state
      case stateLength => 
        val (unaffected, reduce) = authors.splitAt(index)
        postAuthorsLens.set(state, unaffected ::: reduce.tail)
    }
  }
  def removeTranslator(index: Int)(state: Post)(inputText: String): Post = {
    val translators = postTranslatorsLens.get(state)
    translators.length match {
      case 0 => state
      case stateLength => 
        val (unaffected, reduce) = translators.splitAt(index)
        postTranslatorsLens.set(state, unaffected ::: reduce.tail)
    }
  }

}

object AddWeirdCanadaPostSnippet extends DynamicFormHelpers {

  // import lenses
  import SupportLenses._

  // Import methods from Post object
  import Post._

  private object postState extends RequestVar[Post](Post(Release("", Nil, Nil, "",""), Nil, Nil, "", "", "", "", ""))

  // Add and save function
  def updateState = getUpdateAndSaveFuncForField[Post](postState)

  // Id for divs containing artist forms
  private val artistIdName = "add-artist"
  private val publisherIdName = "add-publisher"
  private val authorIdName = "add-author"
  private val translatorIdName = "add-translator"

  // Update artist 
  def artistNameUpdateFunc(index: Int) = updateState( rowUpdateFunc( postArtistsLens )( updateArtistName _)(appendBlankArtist _)(index) _)( () => JsCmds.Noop)
  def artistUrlUpdateFunc(index: Int) = updateState( rowUpdateFunc( postArtistsLens )( updateArtistUrl _)(appendBlankArtist _)(index) _)( () => JsCmds.Noop)
  def artistCityUpdateFunc(index: Int) = updateState( rowUpdateFunc( postArtistsLens )( updateArtistCity _)(appendBlankArtist _)(index) _)( () => JsCmds.Noop)
  def artistProvinceUpdateFunc(index: Int) = updateState( rowUpdateFunc( postArtistsLens )( updateArtistProvince _)(appendBlankArtist _)(index) _)( () => JsCmds.Noop)
  // Update publisher 
  def publisherNameUpdateFunc(index: Int) = updateState( rowUpdateFunc( postPublishersLens )( updatePublisherName _)(appendBlankPublisher _)(index) _)( () => JsCmds.Noop)
  def publisherUrlUpdateFunc(index: Int) = updateState( rowUpdateFunc( postPublishersLens )( updatePublisherUrl _)(appendBlankPublisher _)(index) _)( () => JsCmds.Noop)
  def publisherCityUpdateFunc(index: Int) = updateState( rowUpdateFunc( postPublishersLens )( updatePublisherCity _)(appendBlankPublisher _)(index) _)( () => JsCmds.Noop)
  def publisherProvinceUpdateFunc(index: Int) = updateState( rowUpdateFunc( postPublishersLens )( updatePublisherProvince _)(appendBlankPublisher _)(index) _)( () => JsCmds.Noop)
  // Update Author
  def authorNameUpdateFunc(index: Int) = updateState( rowUpdateFunc( postAuthorsLens )( updateAuthorName _)(appendBlankAuthor _)(index) _ )( () => JsCmds.Noop)
  def authorUrlUpdateFunc(index: Int) = updateState( rowUpdateFunc( postAuthorsLens )( updateAuthorUrl _)(appendBlankAuthor _)(index) _ )( () => JsCmds.Noop)
  // Update Translator
  def translatorNameUpdateFunc(index: Int) = updateState(rowUpdateFunc(postTranslatorsLens)(updateTranslatorName _)(appendBlankTranslator _)(index) _)( () => JsCmds.Noop)
  def translatorUrlUpdateFunc(index: Int) = updateState(rowUpdateFunc(postTranslatorsLens)(updateTranslatorUrl _)(appendBlankTranslator _)(index) _)( () => JsCmds.Noop)
  // Update Translator Text
  def translatorTextUpdateFunc = updateState( updatePostTranslatorText )( () => JsCmds.Noop )
  // Update Content English
  def contentEnglishUpdateFunc = updateState( updatePostContentEnglish )( () => JsCmds.Noop )
  // Update Content French
  def contentFrenchUpdateFunc = updateState( updatePostContentFrench )( () => JsCmds.Noop )
  // Update "From The" 
  def fromTheUpdateFunc = updateState( updatePostFromThe )( () => JsCmds.Noop )
  // Update "de La"
  def deLaUpdateFunc = updateState( updatePostDeLa )( () => JsCmds.Noop )
  // Update Release Title
  def releaseTitleUpdateFunc = updateState( updatePostReleaseTitle )( () => JsCmds.Noop )
  // Update Release Format
  def releaseFormatUpdateFunc = updateState( updatePostReleaseFormat )( () => JsCmds.Noop )
  // Update Release Release Date
  def releaseReleaseDateUpdateFunc = updateState( updatePostReleaseReleaseDate )( () => JsCmds.Noop )
  // Update after removal
  def artistRemovalUpdateFunc(index: Int) = updateState( removeArtist(index)_ )(() => JsCmds.Replace("%s-%s".format(artistIdName, index), Nil)) 
  def publisherRemovalUpdateFunc(index: Int) = updateState( removePublisher(index)_ )(() => JsCmds.Replace("%s-%s".format(publisherIdName, index), Nil)) 
  def authorRemovalUpdateFunc(index: Int) = updateState( removeAuthor(index)_ )(() => JsCmds.Replace("%s-%s".format(authorIdName, index), Nil))
  def translatorRemovalUpdateFunc(index: Int) = updateState( removeTranslator(index)_)( () => JsCmds.Replace("%s-%s".format(translatorIdName, index), Nil))

  // Add a new set of artists by appending to artist-elements id
  def addNewArtistForm(index: Int): () => JsCmd = { () =>
    JsCmds.Replace("artist-elements", addArtist(index)( addArtistMemoize.applyAgain() ) ++ <div id="artist-elements"></div> )
  }
  def addNewPublisherForm(index: Int): () => JsCmd = { () =>
    JsCmds.Replace("publisher-elements", addPublisher(index)( addPublisherMemoize.applyAgain() ) ++ <div id="publisher-elements"></div> )
  }
  def addNewAuthorForm(index: Int): () => JsCmd = { () =>
    JsCmds.Replace("author-elements", addAuthor(index)( addAuthorMemoize.applyAgain() ) ++ <div id="author-elements"></div> )
  }
  def addNewTranslatorForm(index: Int): () => JsCmd = { () => 
    JsCmds.Replace("translator-elements", addTranslator(index)( addTranslatorMemoize.applyAgain() ) ++ <div id="translator-elements"></div> )
  }

  def addArtist(index: Int) = 
    "name=add-artist [id]" #> "%s-%s".format(artistIdName, index) &
    "name=artist-number *" #> (index+1) &
    "name=artist-name-input" #> SHtml.ajaxText("", artistNameUpdateFunc(index), "placeholder" -> "Artist Name")  &
    "name=artist-url-input" #> SHtml.ajaxText("", artistUrlUpdateFunc(index), "placeholder" -> "URL") &
    "name=artist-city-input" #> SHtml.ajaxText("", artistCityUpdateFunc(index), "placeholder" -> "City") &
    "name=artist-province-input" #> SHtml.ajaxText("", artistProvinceUpdateFunc(index), "placeholder" -> "Province") &
    "name=artist-add [onclick]" #> SHtml.onEvent( (s: String) => addNewArtistForm(index+1)() ) & 
    "name=artist-remove [onclick]" #> SHtml.onEvent( (s: String) => artistRemovalUpdateFunc(index)(s) )

  val addArtistMemoize = SHtml.memoize( addArtist(0) )

  def addPublisher(index: Int) = 
    "name=add-publisher [id]" #> "%s-%s".format(publisherIdName, index) &
    "name=publisher-number *" #> (index + 1) &
    "name=publisher-name-input" #> SHtml.ajaxText("", publisherNameUpdateFunc(index), "placeholder" -> "Publisher Name") &
    "name=publisher-url-input" #> SHtml.ajaxText("", publisherUrlUpdateFunc(index), "placeholder" -> "URL") &
    "name=publisher-city-input" #> SHtml.ajaxText("", publisherCityUpdateFunc(index), "placeholder" -> "City") &
    "name=publisher-province-input" #> SHtml.ajaxText("", publisherProvinceUpdateFunc(index), "placeholder" -> "Province") &
    "name=publisher-add [onclick]" #> SHtml.onEvent( (s: String) => addNewPublisherForm(index+1)() )  &
    "name=publisher-remove [onclick]" #> SHtml.onEvent( (s: String) => publisherRemovalUpdateFunc(index)(s) )

  val addPublisherMemoize = SHtml.memoize( addPublisher(0) )

  def addAuthor(index: Int) =
    "name=add-author [id]" #> "%s-%s".format(authorIdName, index) &
    "name=author-number *" #> (index+1) &
    "name=author-name-input" #> SHtml.ajaxText("", authorNameUpdateFunc(index), "placeholder" -> "Author Name") &
    "name=author-url-input" #> SHtml.ajaxText("", authorUrlUpdateFunc(index), "placeholder" -> "URL") &
    "name=author-add [onclick]" #> SHtml.onEvent( (s: String) => addNewAuthorForm(index+1)() ) &
    "name=author-remove [onclick]" #> SHtml.onEvent( (s: String) => authorRemovalUpdateFunc(index)(s) )

  val addAuthorMemoize = SHtml.memoize( addAuthor(0) )

  def addTranslator(index: Int) =
    "name=add-translator [id]" #> "%s-%s".format(translatorIdName, index) &
    "name=translator-number *" #> (index+1) &
    "name=translator-name-input" #> SHtml.ajaxText("", translatorNameUpdateFunc(index), "placeholder" -> "Translator Name") &
    "name=translator-url-input" #> SHtml.ajaxText("", translatorUrlUpdateFunc(index), "placeholder" -> "URL") &
    "name=translator-add [onclick]" #> SHtml.onEvent( (s: String) => addNewTranslatorForm(index+1)() ) &
    "name=translator-remove [onclick]" #> SHtml.onEvent( (s: String) => translatorRemovalUpdateFunc(index)(s) )

  val addTranslatorMemoize = SHtml.memoize( addTranslator(0) )

  def translatorText = 
    "name=translator-text-input" #> SHtml.ajaxText("", translatorTextUpdateFunc, "placeholder" -> "Translator Text")

  def contentEnglish =
    "name=from-the-english-input" #> SHtml.ajaxText("", fromTheUpdateFunc, "placeholder" -> "From The ...") &
    "name=content-english-input" #> SHtml.ajaxTextarea("", contentEnglishUpdateFunc, "placeholder" -> "English Content") 

  def contentFrench =
    "name=de-la-french-input" #> SHtml.ajaxText("", deLaUpdateFunc, "placeholder" -> "De La ...") &
    "name=content-french-input" #> SHtml.ajaxTextarea("", contentFrenchUpdateFunc, "placeholder" -> "French Content")

  def releaseInfo = 
    "name=release-title-input" #> SHtml.ajaxText("", releaseTitleUpdateFunc, "placeholder" -> "Title") &
    "name=release-format-input" #> "" &
    "name=release-release-date-input" #> "" &
    "name=add-artist" #> addArtistMemoize &
    "name=add-publisher" #> addPublisherMemoize

  def render = 
    "name=add-release" #> releaseInfo &
    "name=add-author" #> addAuthorMemoize &
    "name=add-translator" #> addTranslatorMemoize &
    "name=add-translator-text" #> translatorText &
    "name=add-content-english" #> contentEnglish &
    "name=add-content-french" #> contentFrench &
    "name=do-something" #> SHtml.ajaxButton("Done", () => JsCmds.SetValById("result-input", postState.is.renderAsXml.toString)) 

}
