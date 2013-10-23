package org.weirdcanada.distro.service

import net.liftweb.db.DB1.db1ToDb
import net.liftweb.http.LiftRules
import net.liftweb.util.Props
import net.liftweb.common.{Box, Full}
import net.liftweb.db.{DB, DefaultConnectionIdentifier}
import net.liftweb.mapper.{StandardDBVendor, Schemifier}
import org.weirdcanada.distro.util.{RollingWindow, EmailFactory}
import org.weirdcanada.distro.data._
import org.weirdcanada.distro.Config
import org.weirdcanada.distro.DistroSession

class DatabaseManager(config: Config) {
  def connect = {    
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
        new StandardDBVendor(
          config.dbDriver,
          config.dbUrl,
          Full(config.dbUsername),
          Full(config.dbPassword)
        )

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
  }
  
  def createSchema = {
    // Use Lift's Mapper ORM to populate the database
    Schemifier.schemify(true, Schemifier.infoF _,
        Account, Artist, Publisher, Album, ArtistsPublishers, PublishersAlbums, ArtistsAlbums, Track,
        ConsignedItem, AlbumsConsignedItems, Sale, Consignment, Payment)

    // Dev scaffolding
    if (Props.devMode) {
      if (Publisher.findByName("EMI").isEmpty) {
        val emi = Publisher.create.name("EMI").saveMe
        val bmg = Publisher.create.name("BMG").saveMe
        
        val albert = Artist.create.name("Albert Albertson").saveMe
        val bruce = Artist.create.name("Bruce Bollinger").saveMe
        val chloe = Artist.create.name("Chloe Clearwater").saveMe
        val dianna = Artist.create.name("Dianna Dearborne").saveMe
        
        ArtistsPublishers.create.artist(albert).publisher(bmg).save
        ArtistsPublishers.create.artist(bruce).publisher(emi).save
        ArtistsPublishers.create.artist(chloe).publisher(bmg).save
        ArtistsPublishers.create.artist(dianna).publisher(emi).save
      }
    }
  }
}
