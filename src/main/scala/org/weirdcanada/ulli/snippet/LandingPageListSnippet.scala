package org.weirdcanada.ulli.snippet

// Lift
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

// Scala
import scala.xml.{NodeSeq, Text}

object LandingPageListSnippet {

  def render = 
    "name=description *" #> "I am a transformed description" &
    "name=text *" #> "JERRY HELLO"

}
