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
import org.weirdcanada.ulli.model.{UlliList, UlliElement}

/**
 * simple trait to abstract the rendering of Ulli Lists
 */
trait UlliRenderMethods {

  def renderList(list: UlliList): NodeSeq => NodeSeq = {
    "name=title" #> list.title &
    "name=description" #> list.description &
    "name=list" #> list.elements.map { element => 
      renderElement(element)
    }
  } 

  def renderElement(element: UlliElement): NodeSeq => NodeSeq = {
    "name=url [href]" #> element.url &
    "name=text *" #> element.text
  }

}
