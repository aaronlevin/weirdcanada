package org.weirdcanada.ulli.model

// Lift
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._


class UlliItem extends LongKeyedMapper[UlliItem] {

  def getSingleton = UlliItem

  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object text extends MappedText(this)
  object url extends MappedString(this, 2083)
  object rank extends MappedInt(this)

  object list extends MappedLongForeignKey(this, UlliList)
}

object UlliItem extends UlliItem with LongKeyedMetaMapper[UlliItem]
