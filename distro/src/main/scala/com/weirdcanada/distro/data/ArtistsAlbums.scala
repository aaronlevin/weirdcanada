package com.weirdcanada.distro.data

import net.liftweb.mapper._

class ArtistsAlbums extends LongKeyedMapper[ArtistsAlbums] with IdPK {
  def getSingleton = ArtistsAlbums
  object artist extends MappedLongForeignKey(this, Artist)
  object album extends MappedLongForeignKey(this, Album)
}

object ArtistsAlbums extends ArtistsAlbums with LongKeyedMetaMapper[ArtistsAlbums]