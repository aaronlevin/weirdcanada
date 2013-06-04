package org.weirdcanada.site.model

// scala
import scala.xml.{Text, NodeSeq}

// weirdcanada
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty, HasFields}

// scalaz
import scalaz.Lens

case class Artist(name: String, url: String, city: String, province: String)

object Artist {
  val artistNameLens: Lens[Artist, String] = Lens.lensu( (a, n) => a.copy(name = n), (a) => a.name )
  val artistUrlLens: Lens[Artist, String] = Lens.lensu( (a, u) => a.copy(url = u), (a) => a.url )
  val artistCityLens: Lens[Artist, String] = Lens.lensu( (a, c) => a.copy(city = c), (a) => a.city )
  val artistProvinceLens: Lens[Artist, String] = Lens.lensu( (a, p) => a.copy(province = p), (a) => a.province )

  implicit object ArtistRecord extends HasFields[Artist] {
    val fields: List[DynamicField[Artist]] = List(
      BasicField[Artist]("artist-name", artistNameLens)
    , BasicField[Artist]("artist-url", artistUrlLens)
    , BasicField[Artist]("artist-city", artistCityLens)
    , BasicField[Artist]("artist-province", artistProvinceLens)
    )
  }

  implicit object ArtistEmpty extends HasEmpty[Artist] {
    val empty: Artist = Artist("","","","")
  }

  def renderAsXml(artist: Artist): NodeSeq =
    if(artist.url.isEmpty) Text(artist.name) else <a href={artist.url} target="_blank">{artist.name}</a>

}
