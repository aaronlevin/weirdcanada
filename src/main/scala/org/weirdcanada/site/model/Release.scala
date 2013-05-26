package org.weirdcanada.site.model

// weirdcanada
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty, HasFields, ManyRecordField}

// scalaz
import scalaz.Lens

case class Release(title: String, artists: List[Artist], publishers: List[Publisher], format: String, releaseDate: String)

object Release {
  val releaseTitleLens: Lens[Release, String] = Lens.lensu( (r,t) => r.copy(title = t), (r) => r.title)
  val releaseArtistsMapLens: Lens[Release, Map[Int,Artist]] = Lens.lensu(
    set = (r: Release, am: Map[Int,Artist]) => r.copy(artists = am.toList.sortBy { _._1 }.map { _._2})
  , get = (r: Release) => r.artists.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  val releaseArtistsLens: Lens[Release, List[Artist]] = Lens.lensu(
    set = (r: Release, as: List[Artist]) => r.copy(artists = as)
  , get = (r: Release) => r.artists
  )
  val releasePublishersMapLens: Lens[Release, Map[Int,Publisher]] = Lens.lensu(
    set = (r: Release, pm: Map[Int,Publisher]) => r.copy(publishers = pm.toList.sortBy { _._1 }.map { _._2})
  , get = (r: Release) => r.publishers.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  val releasePublishersLens: Lens[Release, List[Publisher]] = Lens.lensu(
    set = (r: Release, ps: List[Publisher]) => r.copy(publishers = ps)
  , get = (r: Release) => r.publishers
  )
  val releaseFormatLens: Lens[Release, String] = Lens.lensu( (r,f) => r.copy(format = f), (r) => r.format)
  val releaseReleaseDateLens: Lens[Release, String] = Lens.lensu( (r,d) => r.copy(releaseDate = d), (r) => r.releaseDate)

  implicit object ReleaseRecord extends HasFields[Release] {
    val fields: List[DynamicField[Release]] = List(
      BasicField[Release]("release-title", releaseTitleLens)
    , ManyRecordField[Release, Artist]("artist", releaseArtistsMapLens)
    , ManyRecordField[Release, Publisher]("publisher", releasePublishersMapLens)
    , BasicField[Release]("release-format", releaseFormatLens)
    , BasicField[Release]("release-release-date", releaseReleaseDateLens)
    )
  }
}
