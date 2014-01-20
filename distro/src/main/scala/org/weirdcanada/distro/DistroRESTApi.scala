package org.weirdcanada.distro

import argonaut._
import Argonaut._
import net.liftweb.http._
import net.liftweb.http.rest._
import org.weirdcanada.distro.data.Artist
import org.weirdcanada.distro.service.Service

trait TwitterDatum {
  val value: String
  val tokens: List[String]
}
case class ArtistDatum(
  value: String,
  tokens: List[String],
  name: String,
  city: String,
  province: String,
  id: String
) extends TwitterDatum
object ArtistDatum {
  implicit def ArtistDatumCodec = 
    casecodec6(ArtistDatum.apply, ArtistDatum.unapply)("value", "tokens", "name", "city", "province", "id")

  def artistToDatum(artist: Artist): ArtistDatum = ArtistDatum(
    artist.name.is,
    artist.name.is.split(' ').toList,
    artist.name.is,
    artist.city.is,
    artist.province.is,
    artist.id.is.toString
  )

}

class RestAPI(service: Service) extends RestHelper {

  import ArtistDatum._

  serve {
    case "api" :: "artist" :: artistName :: _ JsonGet _ => {

      val datums = Artist.findByPartialName(artistName.toLowerCase).map { artistToDatum }.asJson

      //JsonResponse(datums.nospaces)
      PlainTextResponse(datums.nospaces)

    }
  }

}
