package org.weirdcanada.distro.data

import argonaut._
import Argonaut._
import java.net.{MalformedURLException, URL}
import org.weirdcanada.common.util.JsonUtils
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty,HasFields, RecordField}
import scalaz.Lens

sealed trait SocialMediaEntity

case class TwitterEntity(
  val name: String,
  val url: URL
) extends SocialMediaEntity
object TwitterEntity {


  import JsonUtils._

  val nameLens: Lens[TwitterEntity, String] = Lens.lensu(
    (t,n) => t.copy(name = n),
    (t) => t.name
  )
  val urlLens: Lens[TwitterEntity, String] = Lens.lensu(
    (t,u) => try { t.copy(url = new URL(u)) } catch {case _:MalformedURLException => t},
    (t) => t.url.toString
  )

  implicit def TwitterEntityCodec = 
    casecodec2(TwitterEntity.apply, TwitterEntity.unapply)("name", "url")

  implicit object twitterFields extends HasFields[TwitterEntity] {
    val fields: List[DynamicField[TwitterEntity]] = List(
      BasicField[TwitterEntity]("twitter-name", nameLens),
      BasicField[TwitterEntity]("twitter-url", urlLens)
    )
  }
  implicit object twitterEmpty extends HasEmpty[TwitterEntity] {
    val empty = TwitterEntity("", new URL("https://twitter.com"))
  }
}

case class FacebookEntity(
  val url: URL
) extends SocialMediaEntity
object FacebookEntity {

  import JsonUtils._

  val urlLens: Lens[FacebookEntity, String] = Lens.lensu(
    (f,u) => try { f.copy(url = new URL(u)) } catch {case _:MalformedURLException => f},
    (f) => f.url.toString
  )

  implicit def FacebookEntityCodec  =
    casecodec1(FacebookEntity.apply, FacebookEntity.unapply)("url")

  implicit object facebookFields extends HasFields[FacebookEntity] {
    val fields: List[DynamicField[FacebookEntity]] = List(
      BasicField[FacebookEntity]("facebook-url" ,urlLens)
    )
  }

  implicit object facebookEmpty extends HasEmpty[FacebookEntity] {
    val empty = FacebookEntity(new URL("http://facebook.com"))
  }

}

case class BandcampEntity(
  val url: URL
) extends SocialMediaEntity
object BandcampEntity {

  import JsonUtils._

  val urlLens: Lens[BandcampEntity, String] = Lens.lensu(
    (b,u) => try { b.copy(url = new URL(u)) } catch {case _:MalformedURLException => b },
    (b) => b.url.toString
  )

  implicit def BandcampEntityCodec = 
    casecodec1(BandcampEntity.apply, BandcampEntity.unapply)("url")

  implicit object bandcampFields extends HasFields[BandcampEntity] {
    val fields: List[DynamicField[BandcampEntity]] = List(
      BasicField[BandcampEntity]("bandcamp-url", urlLens)
    )
  }

  implicit object bandcampEmpty extends HasEmpty[BandcampEntity] {
    val empty = BandcampEntity(new URL("http://bandcamp.com"))
  }

}

/**
 * ADT for Social data
 */
case class SocialData(
  val twitter: Option[TwitterEntity],
  val facebook: Option[FacebookEntity],
  val bandcamp: Option[BandcampEntity]
)
object SocialData {

  val twitterLens: Lens[SocialData, TwitterEntity] = Lens.lensu(
    (s,t) => s.copy(twitter = Some(t)),
    (s) => s.twitter.getOrElse { implicitly[HasEmpty[TwitterEntity]].empty }
  )

  val facebookLens: Lens[SocialData, FacebookEntity] = Lens.lensu(
    (s,f) => s.copy(facebook = Some(f)),
    (s) => s.facebook.getOrElse { implicitly[HasEmpty[FacebookEntity]].empty }
  )

  val bandcampLens: Lens[SocialData, BandcampEntity] = Lens.lensu(
    (s,b) => s.copy(bandcamp = Some(b)),
    (s) => s.bandcamp.getOrElse { implicitly[HasEmpty[BandcampEntity]].empty }
  )

  implicit def SocialDataCodec = 
    casecodec3(SocialData.apply, SocialData.unapply)("twitter", "facebook", "bandcamp")

  implicit object socialFields extends HasFields[SocialData] {
    val fields: List[DynamicField[SocialData]] = List(
      RecordField[SocialData, TwitterEntity]("social-twitter", twitterLens),
      RecordField[SocialData, FacebookEntity]("social-facebook", facebookLens),
      RecordField[SocialData, BandcampEntity]("social-bandcamp", bandcampLens)
    )
  }
  implicit object socialEmpty extends HasEmpty[SocialData] {
    val empty = SocialData(None, None, None)
  }

}
