package org.weirdcanada.distro.data

import net.liftweb.mapper._

class Album extends LongKeyedMapper[Album] with IdPK with ManyToMany with OneToMany[Long, Album] {
  def getSingleton = Album
  
  object title extends MappedString(this, 256)
  object url extends MappedString(this, 256)
  object description extends MappedText(this)
  object sku extends MappedString(this, 32)
  object shopifyId extends MappedLong(this)

  object format extends MappedEnum(this, Album.Type)
  object isFirstPressing extends MappedBoolean(this)
  object releaseYear extends MappedInt(this)
  object catalogNumber extends MappedString(this, 32)
  object imageUrl extends MappedString(this, 256)
  object additionalImageUrls extends MappedText(this) // Stored as a comma separated list or JSON // TODO: maybe extract the logic into a class?
  
  // it could be a "split label" release (i.e. more than one label collaborating), or it could be a re-release by a new label
  object publishers extends MappedManyToMany(PublishersAlbums, PublishersAlbums.album, PublishersAlbums.publisher, Publisher)
  
  object artists extends MappedManyToMany(ArtistsAlbums, ArtistsAlbums.album, ArtistsAlbums.artist, Artist)
  object tracks extends MappedOneToMany(Track, Track.album, OrderBy(Track.number, Ascending))

  object consignedItems extends MappedManyToMany(AlbumsConsignedItems, AlbumsConsignedItems.album, AlbumsConsignedItems.consignedItem, ConsignedItem)
}

// The companion object to the above Class
object Album extends Album with LongKeyedMetaMapper[Album] {
  object Type extends Enumeration {
    type Type = Value
    val CompactDisc, Vinyl, Cassete, Digital = Value
  }

  def findByTitle(title: String): List[Album] = Album.findAll(By(Album.title, title))
}