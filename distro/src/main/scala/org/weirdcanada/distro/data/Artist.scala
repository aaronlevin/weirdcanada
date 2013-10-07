package org.weirdcanada.distro.data

import net.liftweb.mapper._

class Artist extends LongKeyedMapper[Artist] with IdPK with Geography with ManyToMany {
  def getSingleton = Artist
  
  object Type extends Enumeration {
    type Type = Value
    val Musician, Band, Author, Illustrator = Value // TODO: more types?
  }
  
  object name extends MappedString(this, 64) with DBIndexed
  object url extends MappedString(this, 256)
  object description extends MappedText(this)
  object imageUrl extends MappedString(this, 256)
  object artistType extends MappedEnum(this, Type)
  object social extends MappedText(this)
  
  object albums extends MappedManyToMany(ArtistsAlbums, ArtistsAlbums.album, ArtistsAlbums.artist, Artist)
  object publishers extends MappedManyToMany(ArtistsPublishers, ArtistsPublishers.publisher, ArtistsPublishers.artist, Artist)
}

// The companion object to the above Class
object Artist extends Artist with LongKeyedMetaMapper[Artist] {
  def findByName(name: String): List[Artist] =
    Artist.findAll(By(Artist.name, name))
  
  def findByGeography(city: String, province: String) =
    Artist.findAll(By(Artist.city, city), By(Artist.province, province))
}
