package org.weirdcanada.distro.data

import java.math.MathContext
import net.liftweb.mapper._
import org.weirdcanada.common.util.StringParsingUtil
import StringParsingUtil.safeParse
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasFields, HasEmpty}
import scalaz.Lens


class Track extends LongKeyedMapper[Track] with IdPK {
  def getSingleton = Track
  
  object name extends MappedString(this, 256) with DBIndexed
  object number extends MappedInt(this)
  object url extends MappedString(this, 256)
  object price extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  object s3Url extends MappedString(this, 256)
  
  object album extends MappedLongForeignKey(this, Album)
}

/**
 * ADT for track data
 */
case class TrackData(id: Long, name: String, number: Int, url: String, price: BigDecimal, s3Url: String)

// The companion object to the above Class
object Track extends Track with LongKeyedMetaMapper[Track] {


  /**
   * Lenses for Dynamic Fields
   */
  val trackNameLens: Lens[TrackData, String] = Lens.lensu( (t,n) => t.copy(name = n), (t) => t.name )
  val trackNumberLens: Lens[TrackData, String] = 
    Lens.lensu( (t,n) => t.copy(number = safeParse[Int](n).getOrElse { 0 }), (t) => t.number.toString )
  val trackUrlLens: Lens[TrackData, String] = Lens.lensu( (t,u) => t.copy(url = u), (t) => t.url )
  val trackPriceLens: Lens[TrackData, String] = Lens.lensu(
    (t,d) => t.copy(price = safeParse[BigDecimal](d).getOrElse { 0.0 }),
    (t) => t.price.toString
  )
  val trackS3UrlLens: Lens[TrackData, String] = Lens.lensu( (t,u) => t.copy(s3Url = u), (t) => t.s3Url )

  /**
   * Witness to the `HasFields` Type Class
   */
  implicit object TrackDataFields extends HasFields[TrackData] {
    val fields: List[DynamicField[TrackData]] = List(
      BasicField[TrackData]("track-name", trackNameLens),
      BasicField[TrackData]("track-number", trackNumberLens),
      BasicField[TrackData]("track-url", trackUrlLens),
      BasicField[TrackData]("track-price", trackPriceLens),
      BasicField[TrackData]("track-s3url", trackS3UrlLens)
    )
  }

  /**
   * Witness to the `HasEmpty` type class
   */
  implicit object TrackDataEmpty extends HasEmpty[TrackData] {
    val empty = TrackData(-1L, "", 0, "", 0.0, "")
  }

  def fromData(data: TrackData)(album: Album): Track =
    Track
      .create
      .name(data.name)
      .number(data.number)
      .url(data.url)
      .price(data.price)
      .s3Url(data.s3Url)
      .album(album)
      .saveMe

  def toData(track: Track): TrackData = TrackData(
    id = track.id.is,
    name = track.name.is,
    number = track.number.is,
    url = track.url.is,
    price = track.price.is,
    s3Url = track.s3Url.is
  )

}

