package org.weirdcanada.site.model

// weirdcanada
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty, HasFields, ManyRecordField}

// scalaz
import scalaz.Lens

case class Release(title: String, artists: List[Artist], publishers: List[Publisher], format: String, releaseDate: String)

object Release {
  private val releaseTitleLens: Lens[Release, String] = Lens.lensu( (r,t) => r.copy(title = t), (r) => r.title)
  private val releaseArtistsLens: Lens[Release, Map[Int,Artist]] = Lens.lensu(
    set = (r: Release, am: Map[Int,Artist]) => r.copy(artists = am.toList.sortBy { _._1 }.map { _._2})
  , get = (r: Release) => r.artists.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  private val releasePublishersLens: Lens[Release, Map[Int,Publisher]] = Lens.lensu(
    set = (r: Release, pm: Map[Int,Publisher]) => r.copy(publishers = pm.toList.sortBy { _._1 }.map { _._2})
  , get = (r: Release) => r.publishers.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  private val releaseFormatLens: Lens[Release, String] = Lens.lensu( (r,f) => r.copy(format = f), (r) => r.format)
  private val releaseReleaseDateLens: Lens[Release, String] = Lens.lensu( (r,d) => r.copy(releaseDate = d), (r) => r.releaseDate)

  implicit object ReleaseRecord extends HasFields[Release] {
    val fields: List[DynamicField[Release]] = List(
      BasicField[Release]("release-title", releaseTitleLens)
    , ManyRecordField[Release, Artist]("artist", releaseArtistsLens)
    , ManyRecordField[Release, Publisher]("publisher", releasePublishersLens)
    , BasicField[Release]("release-format", releaseFormatLens)
    , BasicField[Release]("release-release-date", releaseReleaseDateLens)
    )
  }
}
