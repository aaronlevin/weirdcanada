package org.weirdcanada.dynamicform

// scala
import scala.xml.NodeSeq

// Lift
import net.liftweb.http.RequestVar

// scalaz
import scalaz.Lens.lensId

/*
 * Trait that can be mixed in to provide a method creating `NodeSeq => NodeSeq`
 * methods that will create dynamic forms. 
 */
trait DynamicFormCreator extends DynamicFormHelpers { 

  /*
   * Method to fetch the `NodeSeq => NodeSeq` method for a type `A` that implements
   * the `HasFields` typeclass.
   *
   * @param formState a `RequestVar` object holding the form state.
   * @return a method to transform `NodeSeq`s
   */
  def renderField[A : HasFields](formState: RequestVar[A]): NodeSeq => NodeSeq = {
    val record = implicitly[HasFields[A]]
    val updateStateFunction = getUpdateAndSaveFuncForField[A](formState)
    val initialState = formState.is
    record
      .fields
      .foldLeft( (ns: NodeSeq) => ns){ (acc, field) => 
        acc andThen field.render(updateStateFunction, initialState)(lensId[A], None)
      }
  }
}

