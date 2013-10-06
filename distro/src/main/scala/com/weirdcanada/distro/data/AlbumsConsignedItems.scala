package com.weirdcanada.distro.data

import net.liftweb.mapper._

class AlbumsConsignedItems extends LongKeyedMapper[AlbumsConsignedItems] with IdPK {
  def getSingleton = AlbumsConsignedItems
  object consignedItem extends MappedLongForeignKey(this, ConsignedItem)
  object album extends MappedLongForeignKey(this, Album)
}

object AlbumsConsignedItems extends AlbumsConsignedItems with LongKeyedMetaMapper[AlbumsConsignedItems]
