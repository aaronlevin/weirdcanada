package org.weirdcanada.distro.data

import net.liftweb.common.{Box, Failure, Full}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.mapper._
import org.weirdcanada.dynamicform.{BasicField, DynamicField, DynamicFormFieldRenderHelpers, HasEmpty,HasFields}
import org.weirdcanada.common.util.{Country, Province, StringParsingUtil}
import StringParsingUtil.safeParse
import scala.xml.NodeSeq
import scalaz.Lens
import scalaz.{\/-,-\/} // Zoidberg
import scalaz.\/

class Artist extends LongKeyedMapper[Artist] with IdPK with Geography with ManyToMany {
  def getSingleton = Artist


  object name extends MappedString(this, 64) with DBIndexed
  object url extends MappedString(this, 256)
  object description extends MappedText(this)
  object imageUrl extends MappedString(this, 256)
  object artistType extends MappedEnum(this, Artist.Type)
  object social extends MappedText(this)
  
  object albums extends MappedManyToMany(ArtistsAlbums, ArtistsAlbums.artist, ArtistsAlbums.album, Album)
  object publishers extends MappedManyToMany(ArtistsPublishers, ArtistsPublishers.publisher, ArtistsPublishers.artist, Artist)
}

/**
 * ADT for Artist data
 */
case class ArtistData(
  id: Long,
  name: String, 
  url: String, 
  description: String,
  imageUrl: String,
  artistType: String,
  social: String,
  city: String,
  province: String,
  country: String
)

// The companion object to the above Class
object Artist extends Artist with LongKeyedMetaMapper[Artist] with MapperObjectUtils[Artist] {

  def findByName(name: String): List[Artist] =
    Artist.findAll(By(Artist.name, name))

  def findByPartialName(name: String): List[Artist] = 
    Artist.findAll(Cmp(Artist.name, OprEnum.Like, Full("%" + name + "%"), None, Full("LOWER")) )

  def findByGeography(city: String, province: String) =
    Artist.findAll(By(Artist.city, city), By(Artist.province, province))

  object Type extends Enumeration {
    type Type = Value
    val Musician, Band, Author, Illustrator = Value // TODO: more types?
  }

  implicit object ArtistEmpty extends HasEmpty[ArtistData] {
    val empty: ArtistData = ArtistData(-1L, "","","","","Band","","","alberta","canada")
  }

  /**
   * Setup lenses for the fields on `ArtistData`
   */
  val artistNameLens: Lens[ArtistData, String] = Lens.lensu( (a, n) => a.copy(name = n), (a) => a.name )
  val artistUrlLens: Lens[ArtistData, String] = Lens.lensu( (a, u) => a.copy(url = u), (a) => a.url )
  val artistDescriptionLens: Lens[ArtistData, String] = Lens.lensu( (a,d) =>
      a.copy(description = d), (a) => a.description)
  val artistTypeLens: Lens[ArtistData, String] = Lens.lensu( (a, t) =>
      a.copy(artistType = t), (a) => a.artistType)
  val artistImageUrlLens: Lens[ArtistData, String] = Lens.lensu( (a,i) =>
      a.copy(imageUrl = i), (a) => a.imageUrl)
  val artistSocialLens: Lens[ArtistData, String] = Lens.lensu( (a, s) =>
      a.copy(social = s), (a) => a.social)
  val artistCityLens: Lens[ArtistData, String] = Lens.lensu( (a, c) => a.copy(city = c), (a) => a.city )
  val artistProvinceLens: Lens[ArtistData, String] = Lens.lensu( (a, p) => a.copy(province = p), (a) => a.province )
  val artistCountryLens: Lens[ArtistData, String] = Lens.lensu( (a,c) =>
      a.copy(country = c), (a) => a.country)

  /**
   * Setup custom form-fields (text areas, selects)
   */
  import DynamicFormFieldRenderHelpers.{textAreaRender, s3SignedUploadRender, selectRender}

  private val provinceSelectOptions: Seq[(String, String)] = Province.provinceNameTuples
  private val countrySelectOptions: Seq[(String, String)] = Country.countryTuples
  private val artistTypeOptions: Seq[(String, String)] =
    Seq(("Band", "Band"),("Author", "Author"), ("Illustrator", "Illustrator"))

  private val provinceSelect: String => ArtistData => (String => JsCmd) => (NodeSeq => NodeSeq) =
    selectRender(artistProvinceLens.get)("name=artist-province-input")(provinceSelectOptions) _
  private val countrySelect: String => ArtistData => (String => JsCmd) => (NodeSeq => NodeSeq) =
    selectRender(artistCountryLens.get)("name=artist-country-input")(countrySelectOptions) _
  private val artistTypeSelect: String => ArtistData => (String => JsCmd) => (NodeSeq => NodeSeq) =
    selectRender(artistTypeLens.get)("name=artist-type-input")(artistTypeOptions) _

  private val descriptionTextArea = textAreaRender(artistDescriptionLens.get)("name=artist-description-input")("Description") _

  private val imageUrlField =
    s3SignedUploadRender(artistImageUrlLens.get)("@artist-image-url")("/sign_s3", "name", "type") _

  implicit object ArtistDataFields extends HasFields[ArtistData] {
    val fields: List[DynamicField[ArtistData]] = List(
      BasicField[ArtistData]("artist-name", artistNameLens),
      BasicField[ArtistData]("artist-url", artistUrlLens),
      BasicField[ArtistData]("artist-description", artistDescriptionLens, Some(descriptionTextArea)),
      BasicField[ArtistData]("artist-image-url", artistImageUrlLens, Some(imageUrlField)),
      BasicField[ArtistData]("artist-type", artistTypeLens, Some(artistTypeSelect)),
      BasicField[ArtistData]("artist-social", artistSocialLens),
      BasicField[ArtistData]("artist-city", artistCityLens),
      BasicField[ArtistData]("artist-province", artistProvinceLens, Some(provinceSelect)),
      BasicField[ArtistData]("artist-country", artistCountryLens, Some(countrySelect))
    )
  }

  private def setArtistParamsFromData(data: ArtistData, artist: Artist): \/[String, Artist] = {
    try {
      val t = Type.withName(data.artistType)
      \/-(
        artist
          .name(data.name)
          .url(data.url)
          .description(data.description)
          .imageUrl(data.imageUrl)
          .artistType(t)
          .social(data.social)
          .city(data.city)
          .province(data.province)
          .country(data.country)
      )
    } catch {
      case e: java.util.NoSuchElementException =>
        -\/("Cannot find Artist Type for %s".format(data.artistType))
    }

  }

  /**
   * TODO: propagate option throughout program
   */
  def fromData(data: ArtistData): Option[Artist] = {
    val newArtist = Artist.create
    setArtistParamsFromData(data, newArtist) match {
      case \/-(artist) => Some(artist.saveMe)
      case -\/(_) => None
    }
  }

  def updateFromData(data: ArtistData, artist: Artist): \/[String, Artist] = {
    setArtistParamsFromData(data, artist).map { _.saveMe }
  }

  def toData(artist: Artist): ArtistData = ArtistData(
    id = artist.id.is, 
    name = artist.name.is,
    url = artist.url.is, 
    description = artist.description.is,
    imageUrl = artist.imageUrl.is,
    artistType = artist.artistType.is.toString,
    social = artist.social.is,
    city = artist.city.is,
    province = artist.province.is,
    country = artist.country.is
  )


  /**
   * For use in Typeahead forms
   */
  def insertArtistSideEffect(uid: String)(data: ArtistData): JsCmd = fromData(data) match {
    case None => JsCmds.Alert("failed to insert artist")
    case Some(a) => JsCmds.Alert("Succesfully inserted artist with Id: %s".format(a.id.is))
  }
}
