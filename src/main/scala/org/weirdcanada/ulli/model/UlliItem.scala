package org.weirdcanada.ulli.model

// Lift
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._


class UlliElement extends LongKeyedMapper[UlliElement] {

  def getSingleton = UlliElement

  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object text extends MappedText(this)
  object url extends MappedString(this, 2083)
  object rank extends MappedInt(this)

  object list extends MappedLongForeignKey(this, UlliList)
}

object UlliElement extends UlliElement with LongKeyedMetaMapper[UlliElement]

case class UlliElementStruct(text: String, url: String, rank: Int)
