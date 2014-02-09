package org.weirdcanada.distro.data

import java.math.MathContext
import net.liftweb.mapper._
import org.weirdcanada.common.util.StringParsingUtil
import StringParsingUtil.safeParse
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasFields, HasEmpty, RecordField, S3Audio}
import scalaz.Lens
import java.net.{MalformedURLException, URL}


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
case class TrackData(id: Option[Long], name: String, number: Int, url: String, price: BigDecimal, s3Url: Option[S3Audio])

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
  val trackS3UrlLens: Lens[TrackData, S3Audio] = Lens.lensu( 
    (t, s3) => t.copy(s3Url = Some(s3)),
    (t) => t.s3Url.getOrElse { S3Audio(new URL("https://s3.amazon.com")) }
  )

  /**
   * Witness to the `HasFields` Type Class
   */
  implicit object TrackDataFields extends HasFields[TrackData] {
    val fields: List[DynamicField[TrackData]] = List(
      BasicField[TrackData]("track-name", trackNameLens),
      BasicField[TrackData]("track-number", trackNumberLens),
      BasicField[TrackData]("track-url", trackUrlLens),
      BasicField[TrackData]("track-price", trackPriceLens),
      RecordField[TrackData, S3Audio]("track-s3url", trackS3UrlLens)
      //BasicField[TrackData]("track-s3url", trackS3UrlLens)
    )
  }

  /**
   * Witness to the `HasEmpty` type class
   */
  implicit object TrackDataEmpty extends HasEmpty[TrackData] {
    val empty = TrackData(None, "", 0, "", 0.0, None)
  }

  def fromData(data: TrackData)(album: Album): Track = {
    val track = 
      Track
        .create
        .name(data.name)
        .number(data.number)
        .url(data.url)
        .price(data.price)
        .album(album)

      data.s3Url.map { url => track.s3Url(url.url.toString) }

      track.saveMe
  }

  def toData(track: Track): TrackData = TrackData(
    id = Some(track.id.is),
    name = track.name.is,
    number = track.number.is,
    url = track.url.is,
    price = track.price.is,
    s3Url = try{ Some(S3Audio( new URL(track.s3Url.is) )) } catch { case _:MalformedURLException => None }
  )

}

