package org.weirdcanada.distro.data

import net.liftweb.mapper._
import java.math.MathContext

class Track extends LongKeyedMapper[Track] with IdPK {
  def getSingleton = Track
  
  object name extends MappedString(this, 256) with DBIndexed
  object number extends MappedInt(this)
  object url extends MappedString(this, 256)
  object price extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  object s3Url extends MappedString(this, 256)
  
  object album extends MappedLongForeignKey(this, Album)
}

// The companion object to the above Class
object Track extends Track with LongKeyedMetaMapper[Track]
