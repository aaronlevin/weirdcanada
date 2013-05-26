package org.weirdcanada.site.model

// weirdcanada
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty, HasFields}

// scalaz
import scalaz.Lens

case class Artist(name: String, url: String, city: String, province: String)
object Artist {
  private val artistNameLens: Lens[Artist, String] = Lens.lensu( (a, n) => a.copy(name = n), (a) => a.name )
  private val artistUrlLens: Lens[Artist, String] = Lens.lensu( (a, u) => a.copy(url = u), (a) => a.url )
  private val artistCityLens: Lens[Artist, String] = Lens.lensu( (a, c) => a.copy(city = c), (a) => a.city )
  private val artistProvinceLens: Lens[Artist, String] = Lens.lensu( (a, p) => a.copy(province = p), (a) => a.province )

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
}
