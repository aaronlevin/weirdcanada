package org.weirdcanada.ulli.snippet

// Lift
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

// Scala
import scala.xml.{NodeSeq, Text}

// Java
import java.util.Date

// Ulli
import org.weirdcanada.ulli.lib._


/**
 * simple trait to abstract the rendering of Ulli Lists
 */
trait UlliRenderMethods {

  def renderList(list: UlliList): NodeSeq => NodeSeq = {
    "name=title" #> list.is.title &
    "name=description" #> list.is.description &
    "name=list" #> ulli.is.items.map { item => 
      "name=url [href]" #> item.is.url &
      "name=text *" #> item.is.text
    }
  } 
}
