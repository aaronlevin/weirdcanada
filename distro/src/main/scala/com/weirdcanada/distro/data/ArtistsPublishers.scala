package com.weirdcanada.distro.data

import net.liftweb.mapper._

class ArtistsPublishers extends LongKeyedMapper[ArtistsPublishers] with IdPK {
  def getSingleton = ArtistsPublishers
  object artist extends MappedLongForeignKey(this, Artist)
  object publisher extends MappedLongForeignKey(this, Publisher)
}

object ArtistsPublishers extends ArtistsPublishers with LongKeyedMetaMapper[ArtistsPublishers]
