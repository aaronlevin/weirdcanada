package org.weirdcanada.distro.data

import net.liftweb.mapper._
import java.math.MathContext
import net.liftweb.common.Box

class ConsignedItem extends LongKeyedMapper[ConsignedItem] with IdPK with OneToMany[Long, ConsignedItem] with CreatedTrait {
  def getSingleton = ConsignedItem

  override def createdAtIndexed_? = true

  object Condition extends Enumeration {
    type Type = Value
    val StillSealed, MintMinus, Excellent, VeryGoodPlus, VeryGood, Good, Poor = Value
  }
  
  object Age extends Enumeration {
    type Type = Value
    val New, Vintage = Value // TODO: anything between new and vintage? perhaps just used/preowned
  }

  object consignor extends MappedLongForeignKey(this, Account)
  object album extends MappedLongForeignKey(this, Album)

  object coverCondition extends MappedEnum(this, Condition)
  object mediaCondition extends MappedEnum(this, Condition)
  object additionalNotes extends MappedText(this)
  // TODO: object age 
  object consignedDate extends MappedDateTime(this)
  object quantity extends MappedInt(this)
  object customerCost extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  object wholesaleCost extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  object markUp extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  
  object guid extends MappedUniqueId(this, 32) with DBIndexed // Use this ID to track an item in the store
}

// The companion object to the above Class
object ConsignedItem extends ConsignedItem with LongKeyedMetaMapper[ConsignedItem] {
  def findByGuid(guid: String): Box[ConsignedItem] = {
    find(By(ConsignedItem.guid, guid), PreCache(ConsignedItem.consignor))
  }
}
