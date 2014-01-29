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

/**
 * ADT for Social data
 */
case class SocialData(
  val twitter: Option[TwitterEntity],
  val facebook: Option[FacebookEntity]
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


  implicit def SocialDataCodec = 
    casecodec2(SocialData.apply, SocialData.unapply)("twitter", "facebook")

  implicit object socialFields extends HasFields[SocialData] {
    val fields: List[DynamicField[SocialData]] = List(
      RecordField[SocialData, TwitterEntity]("social-twitter", twitterLens),
      RecordField[SocialData, FacebookEntity]("social-facebook", facebookLens)
    )
  }
  implicit object socialEmpty extends HasEmpty[SocialData] {
    val empty = SocialData(None, None)
  }

}
