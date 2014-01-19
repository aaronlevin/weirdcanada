package org.weirdcanada.distro.data

import net.liftweb.mapper._
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty,HasFields}
import scalaz.Lens

class Artist extends LongKeyedMapper[Artist] with IdPK with Geography with ManyToMany {
  def getSingleton = Artist

  object Type extends Enumeration {
    type Type = Value
    val Musician, Band, Author, Illustrator = Value // TODO: more types?
  }

  object name extends MappedString(this, 64) with DBIndexed
  object url extends MappedString(this, 256)
  object description extends MappedText(this)
  object imageUrl extends MappedString(this, 256)
  object artistType extends MappedEnum(this, Type)
  object social extends MappedText(this)
  
  object albums extends MappedManyToMany(ArtistsAlbums, ArtistsAlbums.album, ArtistsAlbums.artist, Artist)
  object publishers extends MappedManyToMany(ArtistsPublishers, ArtistsPublishers.publisher, ArtistsPublishers.artist, Artist)
}

case class ArtistData(
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
object Artist extends Artist with LongKeyedMetaMapper[Artist] {
  def findByName(name: String): List[Artist] =
    Artist.findAll(By(Artist.name, name))

  def findByGeography(city: String, province: String) =
    Artist.findAll(By(Artist.city, city), By(Artist.province, province))

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

  implicit object ArtistDataFields extends HasFields[ArtistData] {
    val fields: List[DynamicField[ArtistData]] = List(
      BasicField[ArtistData]("artist-name", artistNameLens),
      BasicField[ArtistData]("artist-url", artistUrlLens),
      BasicField[ArtistData]("artist-description", artistDescriptionLens),
      BasicField[ArtistData]("artist-image-url", artistImageUrlLens),
      BasicField[ArtistData]("artist-type", artistTypeLens),
      BasicField[ArtistData]("artist-social", artistSocialLens),
      BasicField[ArtistData]("artist-city", artistCityLens),
      BasicField[ArtistData]("artist-province", artistProvinceLens),
      BasicField[ArtistData]("artist-country", artistCountryLens)
    )
  }

  implicit object ArtistEmpty extends HasEmpty[ArtistData] {
    val empty: ArtistData = ArtistData("","","","","","","","","")
  }

  def fromData(data: ArtistData): Option[Artist] = {
    val artistType = try {
      Some(Type.withName(data.artistType))
    } catch {
      case e: java.util.NoSuchElementException =>
        None
    }

    artistType.map { t =>

    Artist
      .create
      .name(data.name)
      .url(data.url)
      .description(data.description)
      .imageUrl(data.imageUrl)
      //.artistType(t.value)
      .social(data.social)
      .city(data.city)
      .province(data.province)
      .country(data.country)
      .saveMe
    }
  }
}
