package org.weirdcanada.site.snippet
import scala.reflect.runtime.universe._

// Scala
import scalaz.Lens
import Lens.{mapVLens, lensId}

// Weird Canada
import org.weirdcanada.site.lib.{ApplyOnce}
import org.weirdcanada.dynamicform.DynamicFormCreator
import org.weirdcanada.site.model.{
  Post => NewPost
, Release => NewRelease
, Author => NewAuthor
, Translator => NewTranslator
, Artist => NewArtist
, Publisher => NewPublisher}

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

  private object postState extends RequestVar[NewPost](NewPost(NewRelease("", Nil, Nil, "",""), Nil, Nil, "", "", "", "", ""))

  // Add and save function
  def updateState = getUpdateAndSaveFuncForField[NewPost](postState)

  val renderFunction = renderField(postState)

  def render = renderFunction
}
