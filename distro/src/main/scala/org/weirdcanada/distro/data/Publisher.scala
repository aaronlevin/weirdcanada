package org.weirdcanada.distro.data

import net.liftweb.common.{Full}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.mapper._
import org.weirdcanada.common.util.{Country, Province}
import org.weirdcanada.dynamicform.{BasicField, DynamicField, DynamicFormFieldRenderHelpers, HasEmpty,HasFields}
import scala.xml.NodeSeq
import scalaz.Lens

class Publisher extends LongKeyedMapper[Publisher] with IdPK with Address with ManyToMany {
  def getSingleton = Publisher

  object name extends MappedString(this, 64) with DBIndexed
  object url extends MappedString(this, 256)
  object description extends MappedText(this)
  object imageUrl extends MappedString(this, 256)
  object social extends MappedText(this)
  
  object albums extends MappedManyToMany(PublishersAlbums, PublishersAlbums.album, PublishersAlbums.publisher, Publisher)
  object artists extends MappedManyToMany(ArtistsPublishers, ArtistsPublishers.artist, ArtistsPublishers.publisher, Publisher)
}

case class PublisherData(
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
object Publisher extends Publisher with LongKeyedMetaMapper[Publisher] {
  def findByName(name: String): List[Publisher] =
    Publisher.findAll(By(Publisher.name, name))
  
  def findByGeography(city: String, province: String) =
    Publisher.findAll(By(Publisher.city, city), By(Publisher.province, province))

  def findByPartialName(name: String): List[Publisher] = 
    Publisher.findAll(Cmp(Publisher.name, OprEnum.Like, Full("%" + name + "%"), None, Full("LOWER")))

  def fromData(data: PublisherData): Option[Publisher] = {
    try { 
      Some(Publisher
        .create
        .name(data.name)
        .url(data.url)
        .description(data.description)
        .imageUrl(data.imageUrl)
        .social(data.social)
        .city(data.city)
        .province(data.province)
        .country(data.country)
        .saveMe)
    } catch {
      case _ : Throwable => None
    }
  }
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
    val empty: PublisherData = PublisherData("","","","","","","","")
  }

  import DynamicFormFieldRenderHelpers.{textAreaRender, selectRender}

  private val provinceSelectOptions: Seq[(String, String)] = Province.provinceNameTuples
  private val countrySelectOptions: Seq[(String, String)] = Country.countryTuples

  private val provinceSelect: String => PublisherData => (String => JsCmd) => (NodeSeq => NodeSeq) =
    selectRender(publisherProvinceLens.get)("name=publisher-province-input")(provinceSelectOptions) _
  private val countrySelect: String => PublisherData => (String => JsCmd) => (NodeSeq => NodeSeq) =
    selectRender(publisherCountryLens.get)("name=publisher-country-input")(countrySelectOptions) _

  private val descriptionTextArea = textAreaRender(publisherDescriptionLens.get)("name=publisher-description-input")("Description") _

  implicit object PublisherDataFields extends HasFields[PublisherData] {
    val fields: List[DynamicField[PublisherData]] = List(
      BasicField[PublisherData]("publisher-name", publisherNameLens),
      BasicField[PublisherData]("publisher-url", publisherUrlLens),
      BasicField[PublisherData]("publisher-description", publisherDescriptionLens, Some(descriptionTextArea)),
      BasicField[PublisherData]("publisher-image-url", publisherImageUrlLens),
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


