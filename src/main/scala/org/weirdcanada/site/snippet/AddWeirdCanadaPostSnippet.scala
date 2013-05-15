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
import scala.xml.{NodeSeq, Text, Elem}
import scala.annotation.tailrec

// 3rd Party
import scalaz.Lens

case class Author(name: String, url: String)
case class Translator(name: String, url: String)
case class Artist(name: String, url: String, city: String, province: String)
case class Publisher(name: String, url: String, city: String, province: String)
case class Post(
  artists: List[Artist], 
  publishers: List[Publisher], 
  author: Option[Author],
  translator: Option[Translator], 
  fromThe: String, 
  contentEnglish: String, 
  deLa: String, 
  contentFrench: String
)

/**
 * Very likely we'll migrate from these case classes to something else.
 * So, let's setup some lenses
 */
object SupportLenses {

  // view artists on a post
  val postArtistsLens = Lens.lensu( (post: Post, newArtists: List[Artist]) => post.copy(artists = newArtists), (post: Post) => post.artists)

  // view publishers on a post
  val postPublishersLens = Lens.lensu( (post: Post, newPublishers: List[Publisher]) => post.copy(publishers = newPublishers), (post: Post) => post.publishers)

  // view name on an artist
  val artistNameLens = Lens.lensu( (artist: Artist, newName: String) => artist.copy(name = newName), (artist: Artist) => artist.name )
  val artistUrlLens = Lens.lensu( (artist: Artist, newUrl: String) => artist.copy(url = newUrl), (artist: Artist) => artist.url )
  val artistCityLens = Lens.lensu( (artist: Artist, newCity: String) => artist.copy(city = newCity), (artist: Artist) => artist.city )
  val artistProvinceLens = Lens.lensu( (artist: Artist, newProvince: String) => artist.copy(province = newProvince), (artist: Artist) => artist.province )

  // view name on an publisher
  val publisherNameLens = Lens.lensu( (publisher: Publisher, newName: String) => publisher.copy(name = newName), (publisher: Publisher) => publisher.name )
  val publisherUrlLens = Lens.lensu( (publisher: Publisher, newUrl: String) => publisher.copy(url = newUrl), (publisher: Publisher) => publisher.url )
  val publisherCityLens = Lens.lensu( (publisher: Publisher, newCity: String) => publisher.copy(city = newCity), (publisher: Publisher) => publisher.city )
  val publisherProvinceLens = Lens.lensu( (publisher: Publisher, newProvince: String) => publisher.copy(province = newProvince), (publisher: Publisher) => publisher.province )
}

/**
 * Note: break out of this hell and use a state monad for god sake
 */

// Companion object to post that contains update functions for Post state
object Post {

  import SupportLenses._

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

  // Append a "blank" artist
  def appendBlankArtist(state: Post): Post = { 
    postArtistsLens.set(state, postArtistsLens.get(state) ::: Artist("","","","") :: Nil )
  }

  // Append a "blank" pbulisher
  def appendBlankPublisher(state: Post): Post = {
    postPublishersLens.set(state, postPublishersLens.get(state) ::: Publisher("","","","") :: Nil )
  }
}

object AddWeirdCanadaPostSnippet extends DynamicFormHelpers {

  // import lenses
  import SupportLenses._

  private object postState extends RequestVar[Post](Post(Nil, Nil, None, None, "", "", "", ""))

  // Add and save function
  def updateState = getUpdateAndSaveFuncForField[Post](postState)

  /**
   * Method used to create a function to create a new artist row given a rank
   */
  @tailrec
  def rowUpdateFunc(updateFunc: (Post, String, Int) => Post)(incState: (Post) => Post)(rank: Int)(state: Post)(inputText: String): Post = {
    postArtistsLens.get(state).length match {
      case length if length <= rank => rowUpdateFunc(updateFunc)(incState)(rank)( incState(state) )(inputText)
      case _ => 
        updateFunc(state, inputText, rank)
    }
  }

  // Update artist 
  def artistNameUpdateFunc(rank: Int) = updateState( rowUpdateFunc( Post.updateArtistName _)(Post.appendBlankArtist _)(rank) _)( () => JsCmds.Noop)
  def artistUrlUpdateFunc(rank: Int) = updateState( rowUpdateFunc( Post.updateArtistUrl _)(Post.appendBlankArtist _)(rank) _)( () => JsCmds.Noop)
  def artistCityUpdateFunc(rank: Int) = updateState( rowUpdateFunc( Post.updateArtistCity _)(Post.appendBlankArtist _)(rank) _)( () => JsCmds.Noop)
  def artistProvinceUpdateFunc(rank: Int) = updateState( rowUpdateFunc( Post.updateArtistProvince _)(Post.appendBlankArtist _)(rank) _)( () => JsCmds.Noop)
  // Update publisher 
  def publisherNameUpdateFunc(rank: Int) = updateState( rowUpdateFunc( Post.updatePublisherName _)(Post.appendBlankPublisher _)(rank) _)( () => JsCmds.Noop)
  def publisherUrlUpdateFunc(rank: Int) = updateState( rowUpdateFunc( Post.updatePublisherUrl _)(Post.appendBlankPublisher _)(rank) _)( () => JsCmds.Noop)
  def publisherCityUpdateFunc(rank: Int) = updateState( rowUpdateFunc( Post.updatePublisherCity _)(Post.appendBlankPublisher _)(rank) _)( () => JsCmds.Noop)
  def publisherProvinceUpdateFunc(rank: Int) = updateState( rowUpdateFunc( Post.updatePublisherProvince _)(Post.appendBlankPublisher _)(rank) _)( () => JsCmds.Noop)

  // Add a new set of artists by appending to artist-elements id
  def addNewArtistForm(rank: Int): () => JsCmd = { () =>
    JsCmds.Replace("artist-elements", addArtist(rank)( addArtistMemoize.applyAgain() ) )
  }

  def addArtist(rank: Int) = 
    "name=artist-number *" #> (rank+1) &
    "name=artist-name-input" #> SHtml.ajaxText("", artistNameUpdateFunc(rank), "placeholder" -> "Artist Name")  &
    "name=artist-url-input" #> SHtml.ajaxText("", artistUrlUpdateFunc(rank), "placeholder" -> "URL") &
    "name=artist-city-input" #> SHtml.ajaxText("", artistCityUpdateFunc(rank), "placeholder" -> "City") &
    "name=artist-province-input" #> SHtml.ajaxText("", artistProvinceUpdateFunc(rank), "placeholder" -> "Province") &
    "name=artist-add [onclick]" #> SHtml.onEvent( (s: String) => addNewArtistForm(rank+1)() ) & 
    "name=artist-remove" #> ""

  val addArtistMemoize = SHtml.memoize( addArtist(0) )

  def addPublisher = 
    "name=publisher-name-input" #> "" &
    "name=publisher-url-input" #> "" &
    "name=publisher-city-input" #> "" &
    "name=publisher-province-input" #> "" &
    "name=publisher-add" #> "" &
    "name=publisher-remove" #> ""

  def author =
    "name=author-name-input" #> "" &
    "name=author-url-input" #> ""

  def translator =
    "name=translator-name-input" #> "" &
    "name=translator-url-input" #> "" &
    "name=translator-text-input" #> ""

  def contentEnglish =
    "name=from-the-english" #> "" &
    "name=content-english" #> ""

  def contentFrench =
    "name=de-la-french" #> "" &
    "name=content-french" #> ""

  def render = 
    "#add-artist" #> addArtistMemoize

}
