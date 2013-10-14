package org.weirdcanada.distro.tools

import net.liftweb.json._
import net.liftweb.common.Loggable
import net.liftweb.json.{pretty, render}
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import org.weirdcanada.distro.Config
import org.weirdcanada.distro.service.DatabaseManager
import org.weirdcanada.distro.util.AnyExtensions._
import scala.annotation.tailrec
import org.weirdcanada.distro.util.Parse._
import org.weirdcanada.distro.data.Album
import net.liftweb.common.Box.box2Option

object AlbumApp extends App with Loggable {
  val config = Config.fromLiftProps
  val dbManager = new DatabaseManager(config)

  dbManager.connect
  //dbManager.createSchema

  sealed trait Mode
  case object Add extends Mode
  case object Show extends Mode
  case object Update extends Mode
  case object Delete extends Mode
  
  case class Args(
    mode: Mode = Show,
    id: Option[Long] = None,
    title: Option[String] = None,
    url: Option[String] = None,
    description: Option[String] = None,
    sku: Option[String] = None,
    format: Option[String] = None,
    releaseYear: Option[Int] = None
  )
  
  object Args {
    private object Default extends Args()
          
    def apply() = parseArgs(args.toList, Default)

    @tailrec
    private def parseArgs(remainingArgs: List[String], args: Args): Args = {
      remainingArgs match {
        case Nil =>
          args
          
        case "-a" :: tail =>
          parseArgs(tail, args.copy(mode = Add))
        case "-u" :: id :: tail =>
          parseArgs(tail, args.copy(mode = Update, id = Some(id.toLong)))
        case "-d" :: id :: tail =>
          parseArgs(tail, args.copy(mode = Delete, id = Some(id.toLong)))
        case "-s" :: id :: tail =>
          parseArgs(tail, args.copy(mode = Show, id = Some(id.toLong)))
        
        //case "-v" :: tail =>
        //  parseArgs(tail, args.copy(verbose = true))
        
        case "-title" :: title :: tail =>
          parseArgs(tail, args.copy(title = Some(title)))
          
        case "-sku" :: sku :: tail =>
          parseArgs(tail, args.copy(sku = Some(sku)))
          
        case "-url" :: url :: tail =>
          parseArgs(tail, args.copy(url = Some(url)))
          
        case "-desc" :: description :: tail =>
          parseArgs(tail, args.copy(description = Some(description)))
          
        case "-format" :: format :: tail =>
          parseArgs(tail, args.copy(format = Some(format)))
          
        case "-year" :: year :: tail =>
          parseArgs(tail, args.copy(releaseYear = Some(year.toInt)))
          
        case unexpectedToken :: tail =>
          println("Unexpected token: " + unexpectedToken)
          sys.exit(1)
      }
    }
  }
  
  (new AlbumApp(Args())).apply
}

import AlbumApp._

class AlbumApp(args: Args) {
  // Helper method to display an error message and exit (or continue if the user chose to ignore errors)
  private def exit(code: Int, msg: => String): Nothing = {
    println(msg)
    sys.exit(code)
  }
  
  def apply = {
    // Helper method to make the for-comprehension below a little cleaner
    def require(bool: => Boolean) = if (bool) Some(true) else None

    val album: Album =
      (args.mode, args.id) match {
        case (Add, None) =>
          Album.create

        case (Show | Update, Some(id)) =>
          Album.findByKey(id)
            .getOrElse(exit(1, "Album with id %s does not exist".format(id)))
        
        case (Delete, Some(id)) =>
          // TODO: disallow this in production -- especially if other rows depend on this.
          //       Use an ON DELETE RESTRICT type constraint on the foreign key relation
          Album.findByKey(id).map(Album.delete_!)
          exit(0, "Deleted album %d".format(id))
        
        case _ =>
          exit(1, "Invalid options")
      }
    
    if (args.mode == Add || args.mode == Update) {
      args.title map album.title.set
      args.description map album.description.set
      args.url map album.url.set
      args.format map Album.Type.withName map album.format.set
      args.releaseYear map album.releaseYear.set
      args.sku map album.sku.set
      album.save
    }

    val json =
      "album" -> (
        ("id" -> album.id.is) ~ 
        ("title" -> album.title.is) ~
        ("sku" -> album.sku.is) ~
        ("description" -> album.description.is) ~
        ("url" -> album.url.is) ~
        ("releaseYear" -> album.releaseYear.is) ~
        ("format" -> album.format.is.toString) ~
        ("shopifyId" -> album.shopifyId.is) ~
        ("catalogNumber" -> album.catalogNumber.is) ~
        ("firstPressing" -> album.isFirstPressing.is) ~
        ("imageUrl" -> album.imageUrl.is) ~
        ("additionalImageUrls" -> album.additionalImageUrls.is)
        // TODO: publishers, tracks, artists, consigned items?
      )
      
    println(pretty(render(json)))
    
  } // def apply
}
