package org.weirdcanada.site.model

// scala
import scala.xml.{NodeSeq, Text}

// weirdcanada
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty, HasFields}

// scalaz
import scalaz.Lens

case class Publisher(name: String, url: String, city: String, province: String)

object Publisher {
  private val publisherNameLens: Lens[Publisher, String] = Lens.lensu( (p, n) => p.copy(name = n), (p) => p.name )
  private val publisherUrlLens: Lens[Publisher, String] = Lens.lensu( (p, u) => p.copy(url = u), (p) => p.url )
  private val publisherCityLens: Lens[Publisher, String] = Lens.lensu( (p, c) => p.copy(city = c), (p) => p.city )
  private val publisherProvinceLens: Lens[Publisher, String] = Lens.lensu( (p, pr) => p.copy(province = pr), (p) => p.province )

  implicit object PublisherRecord extends HasFields[Publisher] {
    val fields: List[DynamicField[Publisher]] = List(
      BasicField[Publisher]("publisher-name", publisherNameLens)
    , BasicField[Publisher]("publisher-url", publisherUrlLens)
    , BasicField[Publisher]("publisher-city", publisherCityLens)
    , BasicField[Publisher]("publisher-province", publisherProvinceLens)
    )
  }

  implicit object PublisherEmpty extends HasEmpty[Publisher] {
    val empty: Publisher = Publisher("","","","")
  }

  def renderAsXml(publisher: Publisher): NodeSeq =
    if(publisher.url.isEmpty) Text(publisher.name) else <a href={publisher.url} target="_blank">{publisher.name}</a>

}
