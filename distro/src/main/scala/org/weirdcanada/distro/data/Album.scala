package org.weirdcanada.distro.data

import net.liftweb.mapper._

class Album extends LongKeyedMapper[Album] with IdPK with ManyToMany with OneToMany[Long, Album] {
  def getSingleton = Album

  /**
   * Convert the `Type` enum to a string
   *
   * @returns a string representation of the enum
   */
  def formatTypeString: String = {
    import Album.Type._
    val formatType = format.is
    if( formatType == CompactDisc)
      "compact disc"
    else if (formatType == Vinyl)
      "vinyl"
    else if (formatType == TwelveInchVinyl)
      "12\""
    else if (formatType == SevenInchVinyl)
      "7\""
    else if (formatType == Cassette)
      "tape"
    else if (formatType == Digital)
      "digital"
    else if (formatType == Lathe)
      "lathe"
    else 
      "cool"
  }

  /**
   * Test if this album has the format of the right type
   *
   * @returns a boolean indicating whether the test was true or not.
   */
  def isOfType(formatString: String): Boolean = {
    import Album.Type._
    lazy val formatType = format.is

    formatString match {
      case "all" => true
      case "lp" => {
        (formatType == Vinyl) ||
        (formatType == TwelveInchVinyl) ||
        (formatType == SevenInchVinyl) ||
        (formatType == Lathe)
      }
      case "cd" => (formatType == CompactDisc)
      case "tape" => (formatType == Cassette)
      case "digital" => (formatType == Digital)
      case _ => false
    }
  }

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

  override def toString =
    "Album(id=%s, title=%s, sku=%s, format=%s, releaseYear=%s, catalogNumber=%s, shopifyId=%s)"
      .format(
        id.is,
        title.is,
        sku.is,
        format.is.toString,
        releaseYear,
        catalogNumber,
        shopifyId
      )
}

// The companion object to the above Class
object Album extends Album with LongKeyedMetaMapper[Album] {

  object Type extends Enumeration {
    type Type = Value
    val CompactDisc, Vinyl, TwelveInchVinyl, SevenInchVinyl, Cassette, Digital, Lathe = Value
  }

  def findByTitle(title: String): List[Album] = Album.findAll(By(Album.title, title))
}
