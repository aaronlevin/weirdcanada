package org.weirdcanada.dynamicform

// Lift
import net.liftweb.util.ClearNodes
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common.{Empty,Full,Box}
import js.{JsCmd, JsCmds}
import net.liftweb.http.js.jquery.{JqJsCmds}
import js.JE
import JE._

// scala
import scala.xml.{NodeSeq, Unparsed}

/**
 * This trait provides functions to help create dynamic forms using Lift's
 * SHtml.memoize method. See AddCuratedListSnippet for example usage.
 */
trait DynamicFormHelpers {

  private def foldValidators[T](newData: T)(validators: List[T => ValidationResponse]) =
    validators
      .map { validator => validator(newData) }
      .foldLeft(ValidationResponse(true,JsCmds.Noop))( (b,a) => ValidationResponse(b.validity && a.validity, b.response & a.response) )

  private def validationLogic[T](currentData: T, newData: T, validation: ValidationResponse, successCmd: () => JsCmd, stateChanger: T => T): JsCmd = {

    // Ensure validations pass before updating new data
    if( validation.validity ) {
      // Only update global state if newData is present
      if( currentData != newData ) {
        stateChanger(newData)
        validation.response & successCmd() 
      } else {
        validation.response
      }
    } else {
      // Validation did not succeed; execute JsCmds
      validation.response
    }
  }

  // Similar method to the above, except this includes rules for validation
  // across multiple fields
  def getUpdateAndSaveFunc[T](data: RequestVar[T], validators: List[T => ValidationResponse] = Nil) = {

    def updateAndSave(f: T => T)(successCmd: () => JsCmd): JsCmd = {
      try {

        // Get current data & transformed data
        val currentData = data.is
        val newData = f(currentData)

        // validation of new data
        val validationRollUp = foldValidators(newData)(validators)
    
        // Apply logic to determine if state is updated & Successs Command executed
        validationLogic(currentData, newData, validationRollUp, successCmd, (t:T) => data.set(t))
      
      } catch {
        case e: Throwable =>
          JsCmds.Replace("saved", <div id="saved">Error saving form: {e.toString}</div>)
      }
    }
    updateAndSave _
  }

  def getUpdateAndSaveFuncForField[T](data: RequestVar[T], validators: List[T => ValidationResponse] = Nil) = {
  
    def updateAndSave(f:  T => String => T)(successCmd: () => JsCmd): String => JsCmd = (inputString: String) =>
      try {

        // Get current data & transformed data
        val currentData = data.is
        val newData = f(currentData)(inputString)

        // validation of new data
        val validationRollUp = foldValidators(newData)(validators)

        // Apply logic to determine if state is updated & Sccess command executed
        validationLogic(currentData, newData, validationRollUp, successCmd, (t:T) => data.set(t))
      } catch {
        case e: Throwable =>
          JsCmds.Replace("saved", <div id="saved">Error saving form: {e.toString}</div>)
      }

    updateAndSave _
  }
}

object DynamicFormFieldRenderHelpers {

  def textAreaRender[A](accessor: A => String)(selector: String)(filler: String)(uid: String)(current: A)(updateFunc: String => JsCmd): NodeSeq => NodeSeq =
    selector #> SHtml.ajaxTextarea(accessor(current), updateFunc, "placeholder" -> filler, "class" -> "form-control")

  def selectRender[A](accessor: A => String)(selector: String)(selectOptions: Seq[(String,String)])(uid: String)(current: A)(updateFunc: String => JsCmd): NodeSeq => NodeSeq = {
    val currentValue: Box[String] = accessor(current) match {
      case "" => Empty
      case value @ _ => Full(value)
    }
    selector #> SHtml.ajaxSelect(selectOptions, currentValue, updateFunc)
  }

  def checkboxRender[A](accessor: A => String)(selector: String)(uid: String)(current: A)(updateFunc: String => JsCmd): NodeSeq => NodeSeq = {
    val currentValue: Boolean = try {
      accessor(current).toBoolean
    } catch {
      case e: java.lang.IllegalArgumentException => false 
    }

    val booleanUpdateFunc: Boolean => JsCmd = (b) => updateFunc(b.toString)

    selector #> SHtml.ajaxCheckbox(currentValue, booleanUpdateFunc)
  }

  def s3SignedUploadRender[A](accessor: A => String)(selector: String)(
    signUrl: String,
    nameParam: String,
    mimeParam: String
  )(uid: String)(current: A)(updateFunc: String => JsCmd): NodeSeq => NodeSeq = {
    val progressBarId = "%s-progress-bar".format(uid)
    val progressStatusId = "%s-progress-status".format(uid)
    val progressPercentId = "%s-progress-percent".format(uid)
    val handleFileSelectJs: String =
      """wc.s3Upload('%s','%s','%s','%s','%s', '%s', '%s', wc.setProgress)""".format(signUrl, nameParam, mimeParam,progressBarId, progressStatusId, progressPercentId, uid)
    val s3FilesInputId = "%s-s3-files".format(uid)

    val currentUrl = accessor(current)

    selector #> {
      "@s3-files [id]" #> s3FilesInputId &
      "@s3-upload-progress [id]" #> progressBarId &
      "@s3-upload-progress-percent [id]" #> progressPercentId &
      "@s3-upload-status [id]" #> progressStatusId &
      "@s3-url-input" #> SHtml.ajaxText(currentUrl, updateFunc, "id" -> uid) &
      "@s3-javascript" #> JsCmds.Script( JsCmds.Run("""$( document ).ready(function() { document.getElementById('%s').addEventListener('change', %s, false); wc.setProgress(0, 'Waiting for upload.', '%s', '%s', '%s'); });""".format(s3FilesInputId, handleFileSelectJs, progressBarId, progressStatusId, progressPercentId))) &
      "@s3-uploaded-image" #> {
        if( accessor(current).isEmpty )
          ClearNodes
        else
          "@s3-uploaded-image-src [src]" #> currentUrl
      }
    }
  }

  def singleTypeahead[A](accessor: A => String)(selector: String)(
    apiEndpoint: String,
    typeaheadLabel: String,
    currentTransformedValue: String => String
  )(uid: String)(current: A)(updateFunc: String => JsCmd): NodeSeq => NodeSeq =
    selector #> {
      "@typeahead-label *" #> typeaheadLabel &
      "@typeahead-input [id]" #> uid &
      "@typeahead-input [value]" #> currentTransformedValue(accessor(current)) &
      "@typeahead-modal" #> List.empty &
      "@typeahead-hidden-input" #> SHtml.ajaxText("", updateFunc, "id" -> (uid + "-hidden"), "value" -> accessor(current)) &
      "@typeahead-script-handler *" #> Unparsed(
        """wc.typeaheadWrapper('#%s',function(datum) { $('#%s-hidden').val(datum.id); $('#%s').blur(); }, '%s');""".format(uid, uid, uid, apiEndpoint)
      )
    }
}
