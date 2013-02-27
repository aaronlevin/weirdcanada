package org.weirdcanada.ulli.model

// Lift
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

/**
 * Fundamental part of Ulli: the list. Contains a number of items
 */

class UlliList extends LongKeyedMapper[UlliList] with OneToMany[Long,UlliList] {

  def getSingleton = UlliList

  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object title extends MappedString(this, 140) {
    override def dbIndexed_? = true
  }
  object description extends MappedText(this)
  object public extends MappedBoolean(this)
  object datetime extends MappedDateTime(this)

  // Many-to-One
  object user extends MappedLongForeignKey(this, User)

  // One-to-Many
  object items extends MappedOneToMany(UlliItem, UlliItem.list, OrderBy(UlliItem.rank, Ascending))

}

object UlliList extends UlliList with LongKeyedMetaMapper[UlliList]
