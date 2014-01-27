package org.weirdcanada.distro

import argonaut._
import Argonaut._
import net.liftweb.actor.LAFuture
import net.liftweb.common.{Full}
import net.liftweb.http.{PlainTextResponse}
import net.liftweb.http.rest.{RestHelper}
import org.weirdcanada.distro.data.{Account, Album, Artist, ConsignedItem, Publisher}
import org.weirdcanada.distro.service.Service
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

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
    "%s (%s)".format(artist.name.is,artist.city.is),
    artist.name.is.split(' ').toList,
    artist.name.is,
    artist.city.is,
    artist.province.is,
    artist.id.is.toString
  )

}
case class PublisherDatum(
  value: String, 
  tokens: List[String],
  name: String,
  city: String,
  province: String,
  id: String
)
object PublisherDatum {
  implicit def PublisherDatumCodec = 
    casecodec6(PublisherDatum.apply, PublisherDatum.unapply)("value", "tokens", "name", "city", "province", "id")

  def publisherToDatum(publisher: Publisher): PublisherDatum = PublisherDatum(
    "%s (%s)".format(publisher.name.is,publisher.city.is),
    publisher.name.is.split(' ').toList,
    publisher.name.is,
    publisher.city.is,
    publisher.province.is,
    publisher.id.is.toString
  )
}

case class AlbumDatum(
  value: String,
  tokens: List[String],
  title: String,
  id: String
)
object AlbumDatum {
  implicit def AlbumDatumCodec = 
    casecodec4(AlbumDatum.apply, AlbumDatum.unapply)("value","tokens","title", "id")

  def albumToDatum(album: Album): AlbumDatum = AlbumDatum(
    value = "%s - %s".format(album.artists.toList.map { _.name.is }.mkString(" // "), album.title.is),
    tokens = album.title.is :: album.artists.toList.map { _.name.is } ,
    title = album.title.is,
    id = album.id.is.toString
  )

}

case class ConsignedItemDatum(
  value: String,
  tokens: List[String],
  id: String
)
object ConsignedItemDatum {
  implicit def ConsignedItemCodec = 
    casecodec3(ConsignedItemDatum.apply, ConsignedItemDatum.unapply)("value","tokens", "id")

  def consignedItemToDatum(item: ConsignedItem): ConsignedItemDatum = {
    val album = Album.findByKey(item.album.is).openOrThrowException("DB Should manage consignedItem<->album reference")
    val artistsString = album.artists.toList.map { _.name.is }.mkString(" // ")
    val titleString = album.title.is
    val consignorString =
      Account.findByKey(item.consignor.is).openOrThrowException("DB handle consignedItem<->Account relation").displayName
    
    ConsignedItemDatum(
      value = "%s - %s (%s)".format(artistsString, titleString,consignorString),
      tokens = titleString.split(' ').toList.filterNot {_.isEmpty} ++ 
                consignorString.split(' ').toList.filterNot {_.isEmpty} ++ 
                artistsString.replace("//","").split(' ').toList.filterNot {_.isEmpty},
      id = item.id.is.toString
    )
  }
}

class RestAPI(service: Service) extends RestHelper {

  import AlbumDatum._
  import ArtistDatum._
  import ConsignedItemDatum._
  import PublisherDatum._

  serve {
    case "api" :: "artist" :: artistName :: _ JsonGet _ => {
      val datums = Artist.findByPartialName(artistName.toLowerCase).map { artistToDatum }.asJson
      //JsonResponse(datums.nospaces)
      PlainTextResponse(datums.nospaces)
    }
    case "api" :: "publisher" :: publisherName :: _ JsonGet _ => {
      val datums = Publisher.findByPartialName(publisherName.toLowerCase).map { publisherToDatum }.asJson
      PlainTextResponse(datums.nospaces)
    }
    case "api" :: "album" :: albumTitle :: _ JsonGet _ => {
      val datums = Album.findByPartialTitle(albumTitle.toLowerCase).map { albumToDatum }.asJson
      PlainTextResponse(datums.nospaces)
    }
    case "api" :: "consigned-item" :: itemString :: _ JsonGet _ => {
      val matchedByArtist = Artist.findByPartialName(itemString.toLowerCase).flatMap { _.albums.toList }.flatMap { _.consignedItems.toList }
      val matchedByAlbum = Album.findByPartialTitle(itemString.toLowerCase).flatMap { _.consignedItems.toList }
      val matchedByPublisher =Publisher.findByPartialName(itemString.toLowerCase).flatMap { _.albums.toList }.flatMap { _.consignedItems.toList }

      PlainTextResponse( (matchedByArtist ++ matchedByAlbum ++ matchedByPublisher).map { consignedItemToDatum }.asJson.nospaces )

    }

  }

}
