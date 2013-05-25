package org.weirdcanada.site.snippet

// Lift
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common.{Full,Box}
import js.{JsCmd, JsCmds}

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

  def getUpdateAndSaveFuncForDynamicForm[T](data: RequestVar[T], validators: List[T => ValidationResponse] = Nil) = {

    def updateAndSave(f: Option[T] => String => Option[T])(successCmd: () => JsCmd): String => JsCmd = (inputString: String) => 
      try {

        // Get current data & transformed data
        val currentData = data.is
        val newData = f(Some(currentData))(inputString)
        println("newData: %s".format(newData))
        newData match {
          case None => JsCmds.Noop
          case Some(newStateData) => 
            // validation of new data
            val validationRollUp = foldValidators(newStateData)(validators)

            // Apply logic to determine if state is updated and Success command executed
            validationLogic(currentData, newStateData, validationRollUp, successCmd, (t:T) => data.set(t))
        }
      } catch {
        case e: Throwable =>
          JsCmds.Replace("saved", <div id="saved">Error saving form: {e.toString}</div>)
      }

      updateAndSave _
  }



  
  def validInt(in: String): Boolean = {
    in.forall(_.isDigit)
  }

}

case class ValidationResponse(validity: Boolean, response: JsCmd)
