package org.weirdcanada.ulli.snippet

// Ulli
import org.weirdcanada.ulli.lib.{ApplyOnce}
import org.weirdcanada.ulli.model.{UlliElement, UlliElementStruct, UlliList, UlliListStruct, User}

// Lift
import net.liftweb._
import http._
import js.{JsCmd, JsCmds}
import JsCmds.{After, Alert, Run}
import common._
import util.Helpers._

// Scala
import scala.xml.{NodeSeq, Text, Elem}
import scala.annotation._
 
object AddUlliListSnippet extends DynamicFormHelpers {

  val urlClickName = "url-click"

  // UlliListStruct: title, description, public, elements: List[UlliElementStruct]
  // UlliElementStruct: text, url, rank
  private object listState extends RequestVar[UlliListStruct](UlliListStruct("","",true,Nil))

  private def getUlliText(rank: Int) = if( listState.is.elements.length > rank ) listState.is.elements(rank).text else ""
  private def getUlliUrl(rank: Int) = if( listState.is.elements.length > rank ) listState.is.elements(rank).url else ""


  // Add and save function without validators thus far. 
  // Signature: addAndSave(UlliListStruct => UlliListStruct)(successCmd: () => JsCmd): JsCmd
  def addAndSaveForField = getUpdateAndSaveFuncForField[UlliListStruct](listState)

  // Validation functions
  // def validateList(list: UlliListStruct): ValidationResponse = {}

 /**
   * Method used when we create a new Text Row for a given rank
   */
  @tailrec
  def textRowUpdateFunc(rank: Int)(state: UlliListStruct)(inputText: String): UlliListStruct = {
    state.elements.length match {
      case stateLength if stateLength <= rank => textRowUpdateFunc(rank)(state.copy(elements = state.elements ::: UlliElementStruct("","",stateLength) :: Nil ))(inputText)
      case _ => 
        val oldElement = state.elements(rank)
        state.copy(elements = state.elements.updated(rank,oldElement.copy(text = inputText)))
    }
  }
  /**
   * Method used when we create a new URL Row for a given rank
   */
  @tailrec
  def urlRowUpdateFunc(rank: Int)(state: UlliListStruct)(inputText: String): UlliListStruct = {
    state.elements.length match {
      case stateLength if stateLength <= rank => urlRowUpdateFunc(rank)(state.copy(elements = state.elements ::: UlliElementStruct("","",stateLength) :: Nil))(inputText)
      case _ => 
        val oldElement = state.elements(rank)
        state.copy(elements = state.elements.updated(rank,oldElement.copy(url = inputText)))
    }
  }
 
  /**
   * Functions used to update the state in response to a client-side ajax request
   */
  def addUlliTextElement(rank: Int) = addAndSaveForField( textRowUpdateFunc(rank) )( () => JsCmds.Noop )
  def addUlliUrlElement(rank: Int) = addAndSaveForField( urlRowUpdateFunc(rank) )( () => Run("""$('#%s-%s').popover('hide');""".format(urlClickName,rank,urlClickName, rank, getUlliUrl(rank))) ) 

  /**
   * Simple mutually-recursive `NodeSeq => NodeSeq` function that will append rows below it
   */
  def textElementTransform(rank: Int) = 
    "name=ulli-element-text [onblur]" #> SHtml.onEvent( addUlliTextElement(rank) ) &
    "name=ulli-element-text [onfocus]" #> SHtml.onEvent( ApplyOnce[String,JsCmd]( (s: String) => After(300,addTextRow(rank+1)()), JsCmds.Noop).apply _ ).toString & 
    "name=ulli-element-url-clicker [data-content]" #> SHtml.ajaxText("", addUlliUrlElement(rank), "placeholder" -> "url", "id" -> "popover-%s-%s".format(urlClickName, rank)).toString &
    "name=ulli-element-url-clicker [id]" #> "%s-%s".format(urlClickName, rank) &
    "name=ulli-element-url-clicker [onclick]" #> SHtml.onEvent( (s: String) => JsCmds.SetValById("popover-%s-%s".format(urlClickName, rank), getUlliUrl(rank)))

  /**
   * Function to replace an element with id "elements" with a Ajax form field bound to a UlliElement of rank `rank`
   */
  def addTextRow(rank: Int): () => JsCmd = { () => 
    JsCmds.Replace("elements", textElementTransform(rank)( textElementMemoize.applyAgain() ) ) & Run("""$("#%s-%s").popover();""".format(urlClickName,rank))
  }

  /**
   * Memoize the text Element field so we can tranform against it in the future
   * (this helps us keep all the HTML in the template)
   */
  val textElementMemoize = SHtml.memoize( textElementTransform(0) )

  /**
   * Save to database
   */
  def saveList = { () =>
    User.currentUser match {
      case Full(user) => 
        val currentState = listState.is
        val newState = currentState.copy(
          title = listTitle,
          description = listDescription,
          privacy = listPrivacy
        )
        listState.set(newState)
        UlliList.insertStruct(newState, user)
        Alert("saved")
      case Empty => 
        Alert("no user")
      case _: Failure =>
        Alert("failure")
    }
  }

  // Form variables
  var listTitle = ""
  var listDescription = ""
  var listPrivacy = true

  def render = 
    "name=ulli-title" #> SHtml.ajaxText(listTitle, listTitle = _, "placeholder" -> "title", "class" -> "input-xlarge") &
    "name=ulli-description" #> SHtml.ajaxTextarea(listDescription, listDescription = _, "placeholder" -> "description", "class" -> "input-xlarge", "rows" -> "4") &
    "#ulli-text-field-wrapper *" #> textElementMemoize &
    "name=ulli-privacy" #> SHtml.ajaxCheckbox(listPrivacy, listPrivacy = _) &
    "name=submit-button" #> SHtml.ajaxButton("add list",  saveList , "class" -> "btn btn-inverse")

}
