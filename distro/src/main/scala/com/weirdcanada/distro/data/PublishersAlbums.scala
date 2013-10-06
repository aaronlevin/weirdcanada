package com.weirdcanada.distro.data

import net.liftweb.mapper._

class PublishersAlbums extends LongKeyedMapper[PublishersAlbums] with IdPK {
  def getSingleton = PublishersAlbums
  
  object album extends MappedLongForeignKey(this, Album)
  object publisher extends MappedLongForeignKey(this, Publisher)
}

object PublishersAlbums extends PublishersAlbums with LongKeyedMetaMapper[PublishersAlbums]
