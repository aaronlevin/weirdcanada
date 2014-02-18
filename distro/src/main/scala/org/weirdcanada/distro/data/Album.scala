package org.weirdcanada.distro.data

import net.liftweb.common.Full
import net.liftweb.db.DefaultConnectionIdentifier
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.mapper._
import org.joda.time.DateTime
import org.weirdcanada.common.util.{StringParsingUtil, StringUtils}
import StringParsingUtil.safeParse
import org.weirdcanada.dynamicform.{BasicField, DynamicField, DynamicFormFieldRenderHelpers, HasFields, HasEmpty, ManyRecordField, ManyTypeaheadField, S3Image, S3Resource}
import scalaz.Lens
import Lens.lensId
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

class Album extends LongKeyedMapper[Album] with IdPK with ManyToMany with OneToMany[Long, Album] {
  def getSingleton = Album


  object title extends MappedString(this, 256)
  object url extends MappedString(this, 256)
  object description extends MappedText(this)
  object sku extends MappedString(this, 32)
  object shopifyId extends MappedLong(this)
  object barcode extends MappedString(this, 32) // UPC, ISBN, etc

  object format extends MappedEnum(this, Album.Type)
  object isFirstPressing extends MappedBoolean(this)
  object isCompilation extends MappedBoolean(this)
  object releaseYear extends MappedInt(this)
  object catalogNumber extends MappedString(this, 32)
  object imageUrl extends MappedString(this, 256)
  object additionalImageUrls extends MappedText(this) // Stored as a comma separated list or JSON // TODO: maybe extract the logic into a class?
  object weirdCanadaUrl extends MappedString(this, 256)
  object weirdCanadaSays extends MappedText(this)
  
  // it could be a "split label" release (i.e. more than one label collaborating), or it could be a re-release by a new label
  object publishers extends MappedManyToMany(PublishersAlbums, PublishersAlbums.album, PublishersAlbums.publisher, Publisher)
  
  object artists extends MappedManyToMany(ArtistsAlbums, ArtistsAlbums.album, ArtistsAlbums.artist, Artist)
  object tracks extends MappedOneToMany(Track, Track.album, OrderBy(Track.number, Ascending))

  object consignedItems extends MappedOneToMany(ConsignedItem, ConsignedItem.album, OrderBy(ConsignedItem.createdAt, Ascending))

  /**
   * Convert the `Type` enum to a string
   *
   * @returns a string representation of the enum
   */
  def formatTypeString: String = {
    import Album.Type._
    val formatType = format.is
    import Album.formatPretty
    formatPretty(formatType)
  }

  /**
   * Return a sluggable version of the format string
   */
  def sluggedFormatTypeString: String = StringUtils.formatSlug(formatTypeString)

  /**
   * Convert the `Type` enum to a char (for use in the SKU)
   *
   * @returns a char representation of the enum
   */
  def formatCodeString: String = {
    import Album.Type._
    format.is match {
      case CompactDisc     => "CD"
      case Vinyl           => "VINL"
      case TwelveInchVinyl => "12IN"
      case SevenInchVinyl  => "7IN"
      case TenInchVinyl    => "10IN"
      case Cassette        => "CASS"
      case Digital         => "DIGI"
      case Lathe           => "LATH"
      case Book            => "BOOK"
      case TShirt          => "SHRT"
      case DVD             => "DVD"
      case Other           => "OTHR"
      case _               => "UNKN"
    }
  }

  /**
   * calculate an items weight in grams
   */
  def weight: Int = {
    import Album.Type._
    format.is match {
      case CompactDisc => 50
      case Vinyl => 350
      case TwelveInchVinyl => 350
      case SevenInchVinyl => 65
      case TenInchVinyl => 125
      case Cassette => 65
      case Digital => 0
      case Lathe => 150
      case Book => 100
      case TShirt => 100
      case DVD => 65
      case Other => 150
      case _ => 350
    }
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
        (formatType == TenInchVinyl) ||
        (formatType == Lathe)
      }
      case "cd" => (formatType == CompactDisc)
      case "cassette" => (formatType == Cassette)
      case "digital" => (formatType == Digital)
      case "other" => {
        (formatType == Book) ||
        (formatType == TShirt) ||
        (formatType == DVD) ||
        (formatType == Other)
      }
      case _ => false
    }
  }

  def titleWithArtist = "%s - %s".format(artists.toList.map { _.name.is }.mkString(" // "), title.is)

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
  id: Long,
  title: String,
  url: String,
  description: String,
  sku: String,
  shopifyId: Long,
  format: String,
  isFirstPressing: Boolean,
  isCompilation: Boolean,
  releaseYear: Int,
  catalogNumber: String,
  imageUrl: String,
  additionalImageUrls: Map[Int, S3Image],
  weirdCanadaUrl: Option[String],
  weirdCanadaSays: Option[String],
  artistIds: Map[Int, String],
  publisherIds: Map[Int, String],
  tracks: Map[Int, TrackData]
) {

  lazy val artistIdsList: List[Long] = 
    artistIds.values.toList.flatMap { safeParse[Long](_).toList }

  lazy val publisherIdsList: List[Long] = 
    publisherIds.values.toList.flatMap { safeParse[Long](_).toList }
}

// The companion object to the above Class
object Album extends Album with LongKeyedMetaMapper[Album] with MapperObjectUtils[Album] {

  object Type extends Enumeration {
    type Type = Value
    val CompactDisc, TwelveInchVinyl, SevenInchVinyl, TenInchVinyl, Cassette, Digital, Lathe, Vinyl, Book, TShirt, DVD, Other = Value
  }

  def findByTitle(title: String): List[Album] = Album.findAll(By(Album.title, title))

  def findByPartialTitle(title: String): List[Album] = 
    Album.findAll(
      Cmp(Album.title, OprEnum.Like, Full("%" + title + "%"), None, Full("LOWER") )
    )

  def formatPretty(formatType: Album.Type.Type): String = {
    import Type._
    if( formatType == CompactDisc)
      "compact disc"
    else if (formatType == Vinyl)
      "vinyl"
    else if (formatType == TwelveInchVinyl)
      "12\""
    else if (formatType == SevenInchVinyl)
      "7\""
    else if (formatType == TenInchVinyl)
      "10\""
    else if (formatType == Cassette)
      "cassette"
    else if (formatType == Digital)
      "digital"
    else if (formatType == Lathe)
      "lathe"
    else if (formatType == Book)
      "book"
    else if (formatType == TShirt)
      "t-shirt"
    else if (formatType == DVD)
      "dvd"
    else if (formatType == Other)
      "other"
    else 
      "other"
  }


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

  private val albumCompilationLens: Lens[AlbumData, String] = 
    Lens.lensu( (a,p) => a.copy(isCompilation = safeParse[Boolean](p).getOrElse { true }), (a) => a.isCompilation.toString)

  private val albumReleaseYearLens: Lens[AlbumData, String] = 
    Lens.lensu( (a,r) => a.copy(releaseYear = safeParse[Int](r).getOrElse { 0 }), (a) => a.releaseYear.toString )
  private val albumCatalogNumberLens: Lens[AlbumData, String] = Lens.lensu( (a,c) => a.copy(catalogNumber = c), (a) => a.catalogNumber)
  private val albumImageUrlLens: Lens[AlbumData, String] = Lens.lensu( (a,i) => a.copy(imageUrl = i), (a) => a.imageUrl )

  private val weirdCanadaUrlLens: Lens[AlbumData, String] = Lens.lensu( 
    (a,i) => if( i.isEmpty ) a else a.copy(weirdCanadaUrl = Some(i)),
    (a) => a.weirdCanadaUrl.getOrElse { "" }
  )
  private val weirdCanadaSaysLens: Lens[AlbumData, String] = Lens.lensu( 
    (a,i) => if( i.isEmpty ) a else a.copy(weirdCanadaSays = Some(i)),
    (a) => a.weirdCanadaSays.getOrElse { "" }
  )

  private val albumAdditionalImageUrlsLens: Lens[AlbumData, Map[Int, S3Image]] = Lens.lensu(
    (a,map) => a.copy(additionalImageUrls = map),
    (a) => a.additionalImageUrls
  )
  private val albumArtistIdsLens: Lens[AlbumData, Map[Int, String]] = Lens.lensu(
    (a, mis) => a.copy(artistIds = mis),
    (a) => a.artistIds
  )
  private val albumPublisherIdsLens: Lens[AlbumData, Map[Int, String]] = Lens.lensu( 
    (a, mis) => a.copy(publisherIds = mis),
    (a) => a.publisherIds
  )
  private val albumTracksLens: Lens[AlbumData, Map[Int, TrackData]] = Lens.lensu((a,t) => a.copy(tracks = t), (a) => a.tracks)

  import DynamicFormFieldRenderHelpers.{checkboxRender, datePickerRender, selectRender, s3SignedUploadRender, textAreaRender}

  private val descriptionTextArea = textAreaRender(albumDescriptionLens.get)("name=album-description-input")("Description") _
  private val weirdCanadaSaysTextArea = textAreaRender(weirdCanadaSaysLens.get)("name=album-weirdcanadasays-input")("Description") _

  private val albumFirstPressingCheckbox = 
    checkboxRender(albumPressingLens.get)("@album-pressing-input") _

  private val albumCompilationCheckbox = 
    checkboxRender(albumCompilationLens.get)("@album-compilation-input") _

  private val formatSequence: Seq[(String, String)] = 
    Album.Type.values.toSeq.map { s =>(s.toString, formatPretty(s)) }

  private val albumFormatSelect =
    selectRender(albumFormatLens.get)("name=album-format-input")(formatSequence) _

  private val imageUrlField =
    s3SignedUploadRender(albumImageUrlLens.get)("@album-imageurl")("/sign_s3/wc-img", "name", "type") _

  private val yearDateSelect = 
    datePickerRender(albumReleaseYearLens.get)("@album-releaseyear-input")("yyyy") _


  import Track._
  import Artist._
  //import Publisher._
  import Publisher.{insertPublisherSideEffect, PublisherDataFields, PublisherDataEmpty}

  /**
   * Witness to the HasEmpty typeclass
   */
  implicit object AlbumDataEmpty extends HasEmpty[AlbumData] {
    val empty = AlbumData(
      id = -1L,
      title = "",
      url = "",
      description = "",
      sku = "",
      shopifyId = 0,
      format = "Cassette",
      isFirstPressing = true,
      isCompilation = false,
      releaseYear = (new DateTime).toString("YYYY").toInt,
      catalogNumber = "",
      imageUrl = "",
      weirdCanadaUrl = None,
      weirdCanadaSays = None,
      additionalImageUrls = Map.empty[Int, S3Image],
      artistIds = Map.empty[Int, String],
      publisherIds = Map.empty[Int, String],
      tracks = Map.empty[Int, TrackData]
    )
  }

  /**
   * Witness to the `HasFields` type class
   */
  implicit object AlbumDataFields extends HasFields[AlbumData] {
    val fields: List[DynamicField[AlbumData]] = List(
      BasicField[AlbumData]("album-title", albumTitleLens),
      BasicField[AlbumData]("album-url", albumUrlLens),
      BasicField[AlbumData]("album-description", albumDescriptionLens, Some(descriptionTextArea)),
      BasicField[AlbumData]("album-sku", albumSkuLens),
      BasicField[AlbumData]("album-shopifyid", albumShopifyIdLens),
      BasicField[AlbumData]("album-format", albumFormatLens, Some(albumFormatSelect)),
      BasicField[AlbumData]("album-pressing", albumPressingLens, Some(albumFirstPressingCheckbox)),
      BasicField[AlbumData]("album-compilation", albumCompilationLens, Some(albumCompilationCheckbox)),
      BasicField[AlbumData]("album-releaseyear", albumReleaseYearLens,Some(yearDateSelect)),
      BasicField[AlbumData]("album-catalognumber", albumCatalogNumberLens),
      BasicField[AlbumData]("album-imageurl", albumImageUrlLens, Some(imageUrlField)),
      ManyRecordField[AlbumData, S3Image]("album-additionalimageurls",albumAdditionalImageUrlsLens),
      BasicField[AlbumData]("album-weirdcanadaurl", weirdCanadaUrlLens),
      BasicField[AlbumData]("album-weirdcanadasays", weirdCanadaSaysLens, Some(weirdCanadaSaysTextArea)),
      ManyTypeaheadField[AlbumData, ArtistData](
        name = "album-artist", 
        typeaheadLabel = "Add Artist", 
        apiEndpoint = "/api/artist/%QUERY",
        template = "templates-hidden" :: "_add_artist" :: Nil, 
        sideEffectB = insertArtistSideEffect,
        bStateValue = (s: String) => Artist.findByStringId(s).map { _.name.is}.toOption,
        manyLens = albumArtistIdsLens
      ),
      ManyTypeaheadField[AlbumData, PublisherData](
        name = "album-publisher", 
        typeaheadLabel = "Add Publisher", 
        apiEndpoint = "/api/publisher/%QUERY",
        template = "templates-hidden" :: "_add_publisher" :: Nil, 
        sideEffectB = insertPublisherSideEffect,
        bStateValue = (s: String) =>  Publisher.findByStringId(s).map { _.name.is}.toOption,
        manyLens = albumPublisherIdsLens
      ),
     ManyRecordField[AlbumData, TrackData]("album-track", albumTracksLens)
    )
  }

  private def setAlbumParamsFromData(data: AlbumData, album: Album): \/[String, Album] = try {
    val format = Album.Type.withName(data.format)

    data.weirdCanadaUrl.map { u => album.weirdCanadaUrl(u) }
    data.weirdCanadaSays.map { s => album.weirdCanadaSays(s) }

    \/-(
      album
        .title(data.title)
        .url(data.url)
        .description(data.description)
        .sku(data.sku)
        .shopifyId(data.shopifyId)
        .format(format)
        .isFirstPressing(data.isFirstPressing)
        .isCompilation(data.isCompilation)
        .releaseYear(data.releaseYear)
        .catalogNumber(data.catalogNumber)
        .imageUrl(data.imageUrl)
        .additionalImageUrls(data.additionalImageUrls.values.map { _.url.toString }.mkString(","))
      )
  } catch {
    case _: java.util.NoSuchElementException => -\/("%s is not a valid format".format(data.format))
  }

  def fromData(data: AlbumData): Option[Album] = {
    DB.use(DefaultConnectionIdentifier) {connection =>

      val newAlbum = Album.create
      
      setAlbumParamsFromData(data, newAlbum) match {
        case \/-(album) =>
          val artists = 
            data
              .artistIds
              .values
              .flatMap { safeParse[Long] }
              .flatMap { Artist.findByKey(_).toOption }

          val publishers = 
            data
              .publisherIds
              .values
              .flatMap { safeParse[Long] }.flatMap { Publisher.findByKey(_).toOption }

          /** side effects! **/
          artists.foreach { album.artists += _ }
          publishers.foreach { album.publishers += _ }
          album.saveMe
          data.tracks.values.map { Track.fromData(_)(album) } // (track adds itself to an album)

          Some(album.saveMe)

        case -\/(_) => None
      }
    }

  }

  // TODO: less queries
  def updateFromData(data: AlbumData, album: Album): \/[String, Album] = 
    DB.use(DefaultConnectionIdentifier) { connection =>

      setAlbumParamsFromData(data, album).map { album =>
        val currentArtistIds: Set[String] = album.artists.map { _.id.is.toString }.toSet
        val currentPublisherIds: Set[String] = album.publishers.map { _.id.is.toString }.toSet
        val currentTrackNames: Set[String] = album.tracks.map { _.name.is }.toSet
        val dataTrackNames: Set[String] = data.tracks.values.map { _.name }.toSet

        val newArtistIds = 
          (data.artistIds.values.toSet &~ currentArtistIds).flatMap { safeParse[Long] }
        val removeableArtistIds = 
          (currentArtistIds &~ data.artistIds.values.toSet).flatMap { safeParse[Long] }

        val newPublisherIds = 
          (data.publisherIds.values.toSet &~ currentPublisherIds).flatMap { safeParse[Long] }
        val removeablePublisherIds = 
          (currentPublisherIds &~ data.publisherIds.values.toSet).flatMap { safeParse[Long] }

        val removeableTrackNames = (currentTrackNames &~ dataTrackNames)
        val newTrackDatas = data.tracks.values.filter { t => !currentTrackNames.contains(t.name) }
        val removeableTracks = album.tracks.filter { t => removeableTrackNames.contains(t.name.is) }

        val newArtists = newArtistIds.flatMap { Artist.findByKey(_).toOption }
        val removeableArtists = removeableArtistIds.flatMap { Artist.findByKey(_).toOption}

        val newPublishers = newPublisherIds.flatMap { Publisher.findByKey(_).toOption }
        val removeablePublishers = removeablePublisherIds.flatMap { Publisher.findByKey(_).toOption }

        newArtists.foreach { album.artists += _ }
        removeableArtists.foreach { album.artists -= _ }

        newPublishers.foreach { album.publishers += _ }
        removeablePublishers.foreach { album.publishers -= _ }

        newTrackDatas.foreach { Track.fromData(_)(album) }
        removeableTracks.foreach { album.tracks -= _ }
        removeableTracks.foreach { _.delete_! }

        album.saveMe
      }
    }

  /**
   * to data
   */
  def toData(album: Album): AlbumData =  DB.use(DefaultConnectionIdentifier) { connection =>
    val additionalImageUrls = 
      album
        .additionalImageUrls
        .is
        .split(',')
        .flatMap { S3Resource.urlFromString }
        .map { S3Image.apply }
        .toList
        .zipWithIndex
        .map { _.swap }
        .toMap

    AlbumData(
      id = album.id.is,
      title = album.title.is,
      url = album.url.is,
      description = album.description.is,
      sku = album.sku.is,
      shopifyId = album.shopifyId.is,
      format = album.format.is.toString,
      isFirstPressing = album.isFirstPressing.is,
      isCompilation = album.isCompilation.is,
      releaseYear = album.releaseYear.is,
      catalogNumber = album.catalogNumber.is,
      imageUrl = album.imageUrl.is,
      additionalImageUrls = additionalImageUrls,
      weirdCanadaUrl = Option(album.weirdCanadaUrl.is).flatMap { s => if(s.isEmpty) None else Some(s) },
      weirdCanadaSays = Option(album.weirdCanadaSays.is).flatMap { s => if(s.isEmpty) None else Some(s) },
      artistIds = album.artists.map { _.id.is.toString }.zipWithIndex.map { _.swap }.toMap,
      publisherIds = album.publishers.map { _.id.is.toString }.zipWithIndex.map { _.swap }.toMap,
      tracks = album.tracks.map { Track.toData }.zipWithIndex.map { _.swap }.toMap
    )
  }

  /**
   * For use in Typeahead forms. cool
   */
  def insertAlbumSideEffect(uid: String)(data: AlbumData): JsCmd = fromData(data) match {
    case None => JsCmds.Alert("failed to insert album")
    case Some(a) => JsCmds.Alert("Succesfully inserted album with Id: %s".format(a.id.is))
  }



}
