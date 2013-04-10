package org.weirdcanada.ulli.model

// Lift
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.db.DefaultConnectionIdentifier

class UlliElement extends LongKeyedMapper[UlliElement] {

  def getSingleton = UlliElement

  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object text extends MappedText(this)
  object url extends MappedString(this, 2083)
  object rank extends MappedInt(this)

  object list extends MappedLongForeignKey(this, UlliList)
}

object UlliElement extends UlliElement with LongKeyedMetaMapper[UlliElement] {

  def insertStruct(struct: UlliElementStruct, list: UlliList): UlliElement = {

    DB.use(DefaultConnectionIdentifier) { connection => {
      val element = 
        UlliElement
          .create
          .text(struct.text)
          .url(struct.url)
          .rank(struct.rank)
          .list(list)

      element.save()
      element
    }}
  }

  def squishRank(elements: List[UlliElementStruct]): List[UlliElementStruct] = 
    elements.zipWithIndex.map { case (elem, i) => elem.copy(rank = i + 1) }

}

case class UlliElementStruct(text: String, url: String, rank: Int)
