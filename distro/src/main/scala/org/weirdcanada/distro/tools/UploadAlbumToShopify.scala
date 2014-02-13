package org.weirdcanada.distro.tools

import net.liftweb.json._
import net.liftweb.common.Loggable
import org.weirdcanada.distro.Config
import org.weirdcanada.distro.api.shopify.{Shopify, Product}
import org.weirdcanada.distro.service.DatabaseManager
import org.weirdcanada.distro.data.{Album, Artist, Track}
import net.liftweb.common.Full
import scala.io.Source
import org.weirdcanada.distro.util.AnyExtensions._
import org.weirdcanada.distro.util.IdList
import org.weirdcanada.distro.api.shopify.PersistentProduct
import org.weirdcanada.distro.data.Publisher
import org.weirdcanada.distro.api.shopify.Metafield

object UploadAlbumToShopify extends App with Loggable {
  val config = Config.fromLiftProps
  val dbManager = new DatabaseManager(config)
  val shopify = new Shopify(config)

  dbManager.connect
  //dbManager.createSchema

  
  val ids =
    args.toList match {
      case "-i" :: "-" :: Nil => // Read ids from stdin
        Source.stdin.getLines.map(_.toLong)
        
      case "-i" :: IdList(ids) :: Nil =>
        Iterator(ids : _*)
        
      case _ =>
        println("Required parameters: -i <-|id1[,id2,...]>")
        println("  -i -              Reads ids from stdin, one line at a time")
        println("  -i <id1[,idN]*>   Reads ids from a comma-separated string ")
        Iterator.empty
    }

  ids.map(id => (id, Album.findByKey(id)))
    .foreach{
      _ match {
        case (_, Full(album)) =>
          apply(album)
          
        case (id, _) =>
          println("Failed to find album with id %s".format(id))
      }
    }
  
  def apply(album: Album) = (new UploadAlbumToShopify(album, shopify)).upload
}

class UploadAlbumToShopify(album: Album, shopify: Shopify) {
  val DEFAULT_VENDOR = "Weird Canada"

  def shopifyProductFromAlbum(album: Album): Product = {
    def mkBody =
      "<p><b>%s</b> (<b>%d</b>) by </b>%s</b>.</p><p>%s</p><p>Published by %s</p>".format(
        album.title.is,
        album.releaseYear.is,
        album.artists.toList.map(_.name.is) match {
          case oneArtist :: Nil =>
            oneArtist
          case firstArtist :: etAl =>
            firstArtist + " et al"
          case _ =>
            "???"
        },
        album.description.is,
        album.publishers.toList.map(_.name.is).mkString(", ")
      )


    def tags = (
      album.formatTypeString :: 
      album.artists.toList.map { _.province.is }.distinct ::: 
      album.artists.toList.map { _.city.is }.distinct
    ).toSet
      
    new Product(
      mkBody,
      album.format.is.toString,
      tags,
      album.title.is,
      album.publishers.headOption.map(_.name.is).getOrElse(DEFAULT_VENDOR) // Vendor -- arbitrarily choose the first publisher
    )
  }

  def setPublisherMetafield(publishers: List[Publisher]) =
    (product: PersistentProduct) => {
      shopify.addProductMetafield(
        product.id,
        Metafield(
          "publishers",
          publishers.map(_.name.is).mkString(","),
          "weirdcanada"
        )
      )
      
      product
    }

  def setArtistMetaField(artists: List[Artist]) =
    (product: PersistentProduct) =>
      shopify.addProductMetafield(
        product.id,
        Metafield(
          "artists",
          artists.map { _.name.is }.mkString(","),
          "weirdcanada"
        )
      )

  def publisherMetafield(publishers: List[Publisher]) = 
    Metafield(
      "publishers",
      publishers.map(_.name.is).mkString(";"),
      "weirdcanada"
    )

  def artistMetafield(artists: List[Artist]) = 
    Metafield(
      "artists",
      artists.map { _.name.is }.mkString(";"),
      "weirdcanada"
    )

  def tracksMetafield(tracks: List[Track]) =
    Metafield(
      "tracks",
      tracks.map { _.s3Url.is }.mkString(";"),
      "weirdcanada"
    )

  def addProductMetafields(metafields: Seq[Metafield]) = 
    (product: PersistentProduct) => {
      shopify.addProductMetafields(product.id, metafields)
    }

  def upload = {
    val product = shopifyProductFromAlbum(album)
    
    (album.shopifyId.is match {
      case 0 =>
        shopify.addProduct(product) |>
          (pp => println("Created Shopify product #%s from album #%s (%s)".format(pp.id, album.id.is, album.title.is))) |>
          (pp => album.shopifyId(pp.id).save)
      
      case existingShopifyId =>
        shopify.updateProduct(existingShopifyId, product) |>
          (pp => println("Updated Shopify product #%s from album #%s (%s)".format(pp.id, album.id.is, album.title.is)))
    }) |>
      addProductMetafields(Seq(
        publisherMetafield(album.publishers.toList),
        artistMetafield(album.artists.toList),
        tracksMetafield(album.tracks.toList),
        Metafield("format", album.formatTypeString, "weirdcanada"),
        Metafield("title", album.title.is, "weirdcanada"),
        Metafield("geo", album.artists.toList.map { _.geoString }.mkString(" // "), "weirdcanada"),
        Metafield("albumUrl", album.url.is, "weirdcanada")

      ))
  }
}
