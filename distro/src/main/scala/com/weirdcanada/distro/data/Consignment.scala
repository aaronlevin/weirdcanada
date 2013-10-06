package com.weirdcanada.distro.data

import net.liftweb.mapper._

class Consignment extends LongKeyedMapper[Consignment] with IdPK {
  def getSingleton = Consignment
  
  object dateReceived extends MappedDate(this)
  object consignedItem extends MappedLongForeignKey(this, ConsignedItem)
  object consignor extends MappedLongForeignKey(this, Account)
  object quantity extends MappedInt(this)
  object notes extends MappedText(this)
}

// The companion object to the above Class
object Consignment extends Consignment with LongKeyedMetaMapper[Consignment]