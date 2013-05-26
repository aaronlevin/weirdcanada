package org.weirdcanada.site.snippet
import scala.reflect.runtime.universe._

// Scala
import scalaz.Lens
import Lens.{mapVLens, lensId}

// Weird Canada
import org.weirdcanada.site.lib.{ApplyOnce}
import org.weirdcanada.dynamicform.DynamicFormCreator
import org.weirdcanada.site.model.{Post, Release}

// Lift
import net.liftweb._
import http._
import js.{JsCmd, JsCmds}
import JsCmds.{After, Alert, Replace, Run, Noop}
import common._
import util.Helpers._

// Scala
import scala.xml.{NodeSeq, Text, Elem}
import scala.annotation.tailrec

object TestSnippet extends DynamicFormCreator {

  import Post._

  private object postState extends RequestVar[Post](Post(Release("", Nil, Nil, "",""), Nil, Nil, "", "", "", "", ""))

  // Add and save function
  def updateState = getUpdateAndSaveFuncForField[Post](postState)

  val renderFunction = renderField(postState)

  def render = renderFunction andThen
    "name=do-something" #> SHtml.ajaxButton("Done", () => JsCmds.SetValById("result-input", Post.renderAsXml(postState.is).toString)) 

}
