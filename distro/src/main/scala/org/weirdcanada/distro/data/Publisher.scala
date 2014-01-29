package org.weirdcanada.distro.data

import net.liftweb.common.{Box, Failure, Full}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.mapper._
import org.weirdcanada.common.util.{Country, Province}
import org.weirdcanada.dynamicform.{BasicField, DynamicField, DynamicFormFieldRenderHelpers, HasEmpty,HasFields}
import scala.xml.NodeSeq
import scalaz.Lens
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

class Publisher extends LongKeyedMapper[Publisher] with IdPK with Address with ManyToMany {
  def getSingleton = Publisher

  object name extends MappedString(this, 64) with DBIndexed
  object url extends MappedString(this, 256)
  object description extends MappedText(this)
  object imageUrl extends MappedString(this, 256)
  object social extends MappedText(this)
  
  object albums extends MappedManyToMany(PublishersAlbums, PublishersAlbums.publisher, PublishersAlbums.album, Album)
  object artists extends MappedManyToMany(ArtistsPublishers, ArtistsPublishers.artist, ArtistsPublishers.publisher, Publisher)
}

case class PublisherData(
  id: Long,
  name: String,
  url: String,
  description: String,
  imageUrl: String,
  social: String,
  city: String,
  province: String,
  country: String
)

// The companion object to the above Class
object Publisher extends Publisher with LongKeyedMetaMapper[Publisher] with MapperObjectUtils[Publisher] {
  def findByName(name: String): List[Publisher] =
    Publisher.findAll(By(Publisher.name, name))
  
  def findByGeography(city: String, province: String) =
    Publisher.findAll(By(Publisher.city, city), By(Publisher.province, province))

  def findByPartialName(name: String): List[Publisher] = 
    Publisher.findAll(Cmp(Publisher.name, OprEnum.Like, Full("%" + name + "%"), None, Full("LOWER")))
  
  private def setPublisherParamsFromData(data: PublisherData, publisher: Publisher): \/[String, Publisher] = try {
    \/-(
      publisher
        .name(data.name)
        .url(data.url)
        .description(data.description)
        .imageUrl(data.imageUrl)
        .social(data.social)
        .city(data.city)
        .province(data.province)
        .country(data.country)
      )
  } catch {
    case e: Throwable => -\/("Something terrible happened.\n%s".format(e))
  }

  def fromData(data: PublisherData): Option[Publisher] = {
    val newPublisher = Publisher.create
    setPublisherParamsFromData(data, newPublisher) match {
      case \/-(publisher) => Some(publisher.saveMe)
      case -\/(_) => None
    }
  }

  def updateFromData(data: PublisherData, publisher: Publisher): \/[String, Publisher] = 
    setPublisherParamsFromData(data, publisher).map { _.saveMe }

  def toData(publisher: Publisher): PublisherData = PublisherData(
    id = publisher.id.is,
    name = publisher.name.is,
    url = publisher.url.is, 
    description = publisher.description.is,
    imageUrl = publisher.imageUrl.is,
    social = publisher.social.is,
    city = publisher.city.is,
    province = publisher.province.is,
    country = publisher.country.is
  )

  /**
   * Setup lenses for the fields on `PublisherData`.
   */
  val publisherNameLens: Lens[PublisherData, String] = Lens.lensu( (p, n) => p.copy(name = n), (p) => p.name )
  val publisherUrlLens: Lens[PublisherData, String] = Lens.lensu( (p, u) => p.copy(url = u), (p) => p.url )
  val publisherDescriptionLens: Lens[PublisherData, String] = Lens.lensu( (p,d) =>
      p.copy(description = d), (p) => p.description)
  val publisherImageUrlLens: Lens[PublisherData, String] = Lens.lensu( (p,i) =>
      p.copy(imageUrl = i), (p) => p.imageUrl)
  val publisherSocialLens: Lens[PublisherData, String] = Lens.lensu( (p, s) =>
      p.copy(social = s), (p) => p.social)
  val publisherCityLens: Lens[PublisherData, String] = Lens.lensu( (p, c) => p.copy(city = c), (p) => p.city )
  val publisherProvinceLens: Lens[PublisherData, String] = Lens.lensu( (p, pr) => p.copy(province = pr), (p) => p.province )
  val publisherCountryLens: Lens[PublisherData, String] = Lens.lensu( (p,c) =>
      p.copy(country = c), (p) => p.country)

  implicit object PublisherDataEmpty extends HasEmpty[PublisherData] {
    val empty: PublisherData = PublisherData(-1L, "","","","","","","","")
  }

  import DynamicFormFieldRenderHelpers.{textAreaRender, selectRender, s3SignedUploadRender}

  private val provinceSelectOptions: Seq[(String, String)] = Province.provinceNameTuples
  private val countrySelectOptions: Seq[(String, String)] = Country.countryTuples

  private val provinceSelect: String => PublisherData => (String => JsCmd) => (NodeSeq => NodeSeq) =
    selectRender(publisherProvinceLens.get)("name=publisher-province-input")(provinceSelectOptions) _
  private val countrySelect: String => PublisherData => (String => JsCmd) => (NodeSeq => NodeSeq) =
    selectRender(publisherCountryLens.get)("name=publisher-country-input")(countrySelectOptions) _

  private val descriptionTextArea = textAreaRender(publisherDescriptionLens.get)("name=publisher-description-input")("Description") _

  private val imageUrlField =
    s3SignedUploadRender(publisherImageUrlLens.get)("@publisher-image-url")("/sign_s3/wc-img", "name", "type") _


  implicit object PublisherDataFields extends HasFields[PublisherData] {
    val fields: List[DynamicField[PublisherData]] = List(
      BasicField[PublisherData]("publisher-name", publisherNameLens),
      BasicField[PublisherData]("publisher-url", publisherUrlLens),
      BasicField[PublisherData]("publisher-description", publisherDescriptionLens, Some(descriptionTextArea)),
      BasicField[PublisherData]("publisher-image-url", publisherImageUrlLens, Some(imageUrlField)),
      BasicField[PublisherData]("publisher-social", publisherSocialLens),
      BasicField[PublisherData]("publisher-city", publisherCityLens),
      BasicField[PublisherData]("publisher-province", publisherProvinceLens, Some(provinceSelect)),
      BasicField[PublisherData]("publisher-country", publisherCountryLens, Some(countrySelect))
    )
  }

  /**
   * For use in Typeahead forms
   */
  def insertPublisherSideEffect(uid: String)(data: PublisherData): JsCmd = fromData(data) match {
    case None => JsCmds.Alert("failed to insert publisher")
    case Some(a) => JsCmds.Alert("Succesfully inserted publisher with Id: %s".format(a.id.is))
  }

}


