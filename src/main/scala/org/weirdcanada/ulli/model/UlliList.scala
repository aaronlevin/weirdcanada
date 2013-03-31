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
  object elements extends MappedOneToMany(UlliElement, UlliElement.list, OrderBy(UlliElement.rank, Ascending))

}

object UlliList extends UlliList with LongKeyedMetaMapper[UlliList] {

  def insertStruct(struct: UlliListStruct, user: User): UlliList = {

    DB.use(DefaultConnectionIdentifier) { connection => {

      val list: UlliList = 
        UlliList
          .create
          .title(struct.title)
          .description(struct.description)
          .public(struct.privacy)
          .user(user)
      list.save()

      // Create UlliElements with correct id
      struct
        .elements
        .filterNot { element => element.text.isEmpty && element.url.isEmpty }
        .map { element => UlliElement.insertStruct(element, list) } 
      list
    }}
  }

}

// A convenience struct used in forms
case class UlliListStruct(title: String, description: String, privacy: Boolean, elements: List[UlliElementStruct])
