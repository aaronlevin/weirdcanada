package org.weirdcanada.distro.data

import net.liftweb.mapper._
import org.weirdcanada.common.util.StringParsingUtil
import StringParsingUtil.safeParse
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasFields, HasEmpty, ManyRecordField, ManyTypeaheadField}
import scalaz.Lens
import Lens.lensId

class Album extends LongKeyedMapper[Album] with IdPK with ManyToMany with OneToMany[Long, Album] {
  def getSingleton = Album

  /**
   * Convert the `Type` enum to a string
   *
   * @returns a string representation of the enum
   */
  def formatTypeString: String = {
    import Album.Type._
    val formatType = format.is
    if( formatType == CompactDisc)
      "compact disc"
    else if (formatType == Vinyl)
      "vinyl"
    else if (formatType == TwelveInchVinyl)
      "12\""
    else if (formatType == SevenInchVinyl)
      "7\""
    else if (formatType == Cassette)
      "tape"
    else if (formatType == Digital)
      "digital"
    else if (formatType == Lathe)
      "lathe"
    else 
      "cool"
  }

  /**
   * Test if this album has the format of the right type
   *
   * @returns a boolean indicating whether the test was true or not.
   */
  def isOfType(formatString: String): Boolean = {
    import Album.Type._
    lazy val formatType = format.is

    formatString match {
      case "all" => true
      case "lp" => {
        (formatType == Vinyl) ||
        (formatType == TwelveInchVinyl) ||
        (formatType == SevenInchVinyl) ||
        (formatType == Lathe)
      }
      case "cd" => (formatType == CompactDisc)
      case "tape" => (formatType == Cassette)
      case "digital" => (formatType == Digital)
      case _ => false
    }
  }

  object title extends MappedString(this, 256)
  object url extends MappedString(this, 256)
  object description extends MappedText(this)
  object sku extends MappedString(this, 32)
  object shopifyId extends MappedLong(this)

  object format extends MappedEnum(this, Album.Type)
  object isFirstPressing extends MappedBoolean(this)
  object releaseYear extends MappedInt(this)
  object catalogNumber extends MappedString(this, 32)
  object imageUrl extends MappedString(this, 256)
  object additionalImageUrls extends MappedText(this) // Stored as a comma separated list or JSON // TODO: maybe extract the logic into a class?
  
  // it could be a "split label" release (i.e. more than one label collaborating), or it could be a re-release by a new label
  object publishers extends MappedManyToMany(PublishersAlbums, PublishersAlbums.album, PublishersAlbums.publisher, Publisher)
  
  object artists extends MappedManyToMany(ArtistsAlbums, ArtistsAlbums.album, ArtistsAlbums.artist, Artist)
  object tracks extends MappedOneToMany(Track, Track.album, OrderBy(Track.number, Ascending))

  object consignedItems extends MappedManyToMany(AlbumsConsignedItems, AlbumsConsignedItems.album, AlbumsConsignedItems.consignedItem, ConsignedItem)

  override def toString =
    "Album(id=%s, title=%s, sku=%s, format=%s, releaseYear=%s, catalogNumber=%s, shopifyId=%s)"
      .format(
        id.is,
        title.is,
        sku.is,
        format.is.toString,
        releaseYear,
        catalogNumber,
        shopifyId
      )
}

/**
 * ADT for dynamic fields
 */
case class AlbumData(
  title: String,
  url: String,
  description: String,
  sku: String,
  shopifyId: Long,
  format: String,
  isFirstPressing: Boolean,
  releaseYear: Int,
  catalogNumber: String,
  imageUrl: String,
  additionalImageUrls: List[String],
  artistIds: Map[Int, String],
  publisherIds: Map[Int, String],
  tracks: Map[Int, TrackData]
)

// The companion object to the above Class
object Album extends Album with LongKeyedMetaMapper[Album] {

  object Type extends Enumeration {
    type Type = Value
    val CompactDisc, Vinyl, TwelveInchVinyl, SevenInchVinyl, Cassette, Digital, Lathe = Value
  }

  def findByTitle(title: String): List[Album] = Album.findAll(By(Album.title, title))

  /**
   * Lenses for dynamic fields
   */
  private val albumTitleLens: Lens[AlbumData, String] = Lens.lensu( (a, t) => a.copy(title = t), (a) => a.title )
  private val albumUrlLens: Lens[AlbumData, String] = Lens.lensu( (a, u) => a.copy(url = u), (a) => a.url )
  private val albumDescriptionLens: Lens[AlbumData, String] = Lens.lensu( (a, d) => a.copy(description = d), (a) => a.description )
  private val albumSkuLens: Lens[AlbumData, String] = Lens.lensu( (a, s) => a.copy(sku = s), (a) => a.sku )
  private val albumShopifyIdLens: Lens[AlbumData, String] = 
    Lens.lensu( (a, s) => a.copy(shopifyId = safeParse[Long](s).getOrElse { 0L }), (a) => a.shopifyId.toString )
  private val albumFormatLens: Lens[AlbumData, String] = Lens.lensu( (a,f) => a.copy(format = f), (a) => a.format )
  private val albumPressingLens: Lens[AlbumData, String] = 
    Lens.lensu( (a,p) => a.copy(isFirstPressing = safeParse[Boolean](p).getOrElse { true }), (a) => a.isFirstPressing.toString)
  private val albumReleaseYearLens: Lens[AlbumData, String] = 
    Lens.lensu( (a,r) => a.copy(releaseYear = safeParse[Int](r).getOrElse { 0 }), (a) => a.releaseYear.toString )
  private val albumCatalogNumberLens: Lens[AlbumData, String] = Lens.lensu( (a,c) => a.copy(catalogNumber = c), (a) => a.catalogNumber)
  private val albumImageUrlLens: Lens[AlbumData, String] = Lens.lensu( (a,i) => a.copy(imageUrl = i), (a) => a.imageUrl )
  private val albumAdditionalImageUrlsLens: Lens[AlbumData, String] =
    Lens.lensu( (a,is) => a.copy(additionalImageUrls = is.split(',').toList.map { _.trim }), (a) => a.additionalImageUrls.mkString(","))
  private val albumArtistIdsLens: Lens[AlbumData, Map[Int, String]] = Lens.lensu(
    (a, mis) => a.copy(artistIds = mis),
    (a) => a.artistIds
  )
  private val albumPublisherIdsLens: Lens[AlbumData, Map[Int, String]] = Lens.lensu( 
    (a, mis) => a.copy(publisherIds = mis),
    (a) => a.publisherIds
  )

  private val albumTracksLens: Lens[AlbumData, Map[Int, TrackData]] = Lens.lensu((a,t) => a.copy(tracks = t), (a) => a.tracks)

  import Track._
  import Artist._
  //import Publisher._
  import Publisher.{insertPublisherSideEffect, PublisherDataFields, PublisherDataEmpty}

  /**
   * Witness to the `HasFields` type class
   */
  implicit object AlbumDataFields extends HasFields[AlbumData] {
    val fields: List[DynamicField[AlbumData]] = List(
      BasicField[AlbumData]("album-title", albumTitleLens),
      BasicField[AlbumData]("album-url", albumUrlLens),
      BasicField[AlbumData]("album-description", albumDescriptionLens),
      BasicField[AlbumData]("album-sku", albumSkuLens),
      BasicField[AlbumData]("album-shopifyid", albumShopifyIdLens),
      BasicField[AlbumData]("album-format", albumFormatLens),
      BasicField[AlbumData]("album-pressing", albumPressingLens),
      BasicField[AlbumData]("album-releaseyear", albumReleaseYearLens),
      BasicField[AlbumData]("album-catalognumber", albumCatalogNumberLens),
      BasicField[AlbumData]("album-imageurl", albumImageUrlLens),
      BasicField[AlbumData]("album-additionalimageurls", albumAdditionalImageUrlsLens),
      ManyTypeaheadField[AlbumData, ArtistData](
        name = "album-artist", 
        typeaheadLabel = "Add Artist", 
        apiEndpoint = "/api/artist/%Query",
        template = "templates-hidden" :: "_add_artist" :: Nil, 
        sideEffectB = insertArtistSideEffect,
        manyLens = albumArtistIdsLens
      ),
      ManyTypeaheadField[AlbumData, PublisherData](
        name = "album-publisher", 
        typeaheadLabel = "Add Publisher", 
        apiEndpoint = "/api/publisher/%Query",
        template = "templates-hidden" :: "_add_publisher" :: Nil, 
        sideEffectB = insertPublisherSideEffect,
        manyLens = albumPublisherIdsLens
      ),
     ManyRecordField[AlbumData, TrackData]("album-track", albumTracksLens)
    )
  }

  /**
   * Witness to the HasEmpty typeclass
   */
  implicit object AlbumDataEmpty extends HasEmpty[AlbumData] {
    val empty = AlbumData(
      title = "",
      url = "",
      description = "",
      sku = "",
      shopifyId = 0,
      format = "",
      isFirstPressing = true,
      releaseYear = 0,
      catalogNumber = "",
      imageUrl = "",
      additionalImageUrls = Nil,
      artistIds = Map.empty[Int, String],
      publisherIds = Map.empty[Int, String],
      tracks = Map.empty[Int, TrackData]
    )
  }

}
