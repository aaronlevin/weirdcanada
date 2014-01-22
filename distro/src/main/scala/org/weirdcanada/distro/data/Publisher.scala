package org.weirdcanada.distro.data

import net.liftweb.common.{Full}
import net.liftweb.mapper._

class Publisher extends LongKeyedMapper[Publisher] with IdPK with Address with ManyToMany {
  def getSingleton = Publisher

  object name extends MappedString(this, 64) with DBIndexed
  object url extends MappedString(this, 256)
  object description extends MappedText(this)
  object imageUrl extends MappedString(this, 256)
  
  object albums extends MappedManyToMany(PublishersAlbums, PublishersAlbums.album, PublishersAlbums.publisher, Publisher)
  object artists extends MappedManyToMany(ArtistsPublishers, ArtistsPublishers.artist, ArtistsPublishers.publisher, Publisher)
}

// The companion object to the above Class
object Publisher extends Publisher with LongKeyedMetaMapper[Publisher] {
  def findByName(name: String): List[Publisher] =
    Publisher.findAll(By(Publisher.name, name))
  
  def findByGeography(city: String, province: String) =
    Publisher.findAll(By(Publisher.city, city), By(Publisher.province, province))

  def findByPartialName(name: String): List[Publisher] = 
    Publisher.findAll(Cmp(Publisher.name, OprEnum.Like, Full("%" + name + "%"), None, Full("LOWER")))
}
