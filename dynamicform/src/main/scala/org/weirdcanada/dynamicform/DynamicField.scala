package org.weirdcanada.dynamicform

// Scala
import scala.xml.{NodeSeq, Text, Unparsed}

// Lift
import net.liftweb._
import http._
import js.{JsCmd, JsCmds}
import JsCmds.{After, Alert, Replace, Run, Noop}
import common._
import util.Helpers._

// 3rd party
import scalaz.Lens
import scalaz.Lens.{mapVLens, lensId}


/*
 * Trait to represent a Dynamic Fields. Classes mixing in this trait must provide
 * a render method that takes some state and a lens and renders the field (or subfields)
 * in addition to a `String` reprsenting the name of the field (does not need to be unique).
 */
sealed trait DynamicField[A] {
  import DynamicField.{makeName,FormStateUpdate}
  val name: String

  /*
   * Method that renders a form.
   *
   * @param formStateUpdater  a method used to construct a function that updates the form state and returns some JsCmds for UI 
   * @param outerLens         Lens from the parent of the current field.
   * @param outerName         Concatenated name from the parent field(s)
   * @return Return a method to transform `NodeSeq`
   */
  def render[B](formStateUpdater: FormStateUpdate[B], state: B)(outerLens: Lens[B,A], outerName: Option[String]): NodeSeq => NodeSeq
}

/*
 * The most basic field: a field with no SubFields. Will correspond to a basic "<input>" on a form.
 *
 * @constructor create a new `BasicField` of type `A`
 * @param name The name of the field
 * @param lens a lens from the field type `A` to String (for updating the field from string input)
 */
case class BasicField[A](name: String, lens: Lens[A,String], transformer: Option[String => A => (String => JsCmd) => (NodeSeq => NodeSeq)] = None) extends DynamicField[A] {
  import DynamicField.{makeName,makeNameAdd,makeInput, FormStateUpdate, label}

  /*
   * Method to render a Basic field.
   *
   * @param formStateUpdater  a method to construct a function that updates the form state and returns some JsCmds for UI
   * @param outerLens         Lens from the parent of this field
   * @param outerName         Concatenated name from the parent field
   * @return a method to transform `NodeSeq`
   */
  def render[B](formStateUpdater: FormStateUpdate[B], state: B)(outerLens: Lens[B,A], outerName: Option[String] = None): NodeSeq => NodeSeq = {

    /*
     * A method composing lenses for the ability to update this field with the input string.
     *
     * @param state       state data of type `B` reprsenting the parent form state to be updated.
     * @param inputString inputString from a user (coming via ajax on a form)
     * @return an updated version of the state. 
     */
    def updateFunc(state: B)(inputString: String): B = 
      (outerLens >=> lens).set(state, inputString)

    /*
     * A method composing lenses for the ability to "get" this field
     *
     * @param state     state data of type `B` representing the parent form state to be getted.
     * @return the value
     */
    def getFunc(state: B): String = (outerLens >=> lens).get(state)

    val jsCmd = () => Noop
    val fieldUpdateFunc: String => JsCmd = formStateUpdater(updateFunc)(jsCmd)
    transformer match {
      case None => BasicField.defaultTransformer(outerName, name, fieldUpdateFunc, getFunc(state))
      case Some(transformer) => transformer(label(outerName, name))(outerLens.get(state))(fieldUpdateFunc)
    }
  }
}

/*
 * Companion object for BasicField
 */
object BasicField {

  import DynamicField.{makeInput, label}

  def defaultTransformer(outerName: Option[String], name: String, updateFunc: String => JsCmd, value: => String): NodeSeq => NodeSeq = {
    val inputName = makeInput(None, name)
    val uid = label(outerName, name)
    inputName #> SHtml.ajaxText("", updateFunc, "value" -> value, "id" -> uid) 
  }

}



/* 
 * A Field of type `A` that encompasses a Structure of cardinality 1 for type `B`. 
 * Type `B` must implement the `HasFields` typeclass (ensuring that `B` has fields to render)
 *
 * @constructor create a `RecordField` from type `A` to type `B`
 * @param name the name of the field
 * @param lens a lens from `A` to `B`
 */
case class RecordField[A, B : HasFields](name: String, lens: Lens[A,B]) extends DynamicField[A] {
  import DynamicField.{label,makeNameAdd,FormStateUpdate}
  val bRecord = implicitly[HasFields[B]]

  /*
   * Render a structured field by calling the `render` method on its subfields and passing a composition
   * of the outer and current lense.
   *
   * @param formStateUpdater  a method to construct a function that updates the form state and returns some JsCmds for UI
   * @param outerLens         Lens from the parent of this field
   * @param outerName         Concatenated name from the parent field
   * @return a method to transform `NodeSeq`
   */
  def render[C](formStateUpdater: FormStateUpdate[C], state: C)(outerLens: Lens[C,A], outerName: Option[String]): NodeSeq => NodeSeq = {
    makeNameAdd(None, name) #>
      bRecord
        .fields
        .foldLeft( identity[NodeSeq]_ ){ (acc, field) => acc andThen field.render(formStateUpdater, state)(outerLens >=> lens, Some(label(outerName,name))) }
  }
}

/*
 * A field of type `A` which contains a structure (`B`) of cardinality larger than 1. `B` must implement
 * the `HasEmpty` typeclass in addition to the `HasFields` typeclass. `B` must implement `HasFields` so we
 * can insert an "empty" `B` when updating a subfield of `B` when one doesn't already exist (i.e. updating the
 * `name` on a `Comment`). An example is a `Post` has many `Comments`
 *
 * @constructor create a `ManyRecordField` from type `A` to type `B`
 * @param name the name of the field
 * @param lens a lens from `A` to `Map[Int,B]`
 * @param indexedChromeRendering a function to render the visual data surrounding the many record field (updates for each entry)
 */
case class ManyRecordField[A, B : HasFields : HasEmpty](name: String, lens: Lens[A, Map[Int,B]], indexedChromeRendering: Option[Int => (NodeSeq => NodeSeq)] = None) extends DynamicField[A] {
  import DynamicField.{makeAdd, makeName, makeNameAdd, indexedLabel, label, FormStateUpdate,optionLens}
  val bRecord = implicitly[HasFields[B]]

  /*
   * Render a `ManyRecordField` by creating some chrome to allow us to, recursively, add additional copies of this field,
   * and then pass rendering off to the subfields.
   *
   * @param formStateUpdater  a method to construct a function that updates the form state and returns some JsCmds for UI
   * @param outerLens         Lens from the parent of this field
   * @param outerName         Concatenated name from the parent field
   * @return a method to transform `NodeSeq`
   */
  def render[C](formStateUpdater: FormStateUpdate[C], state: C)(outerLens: Lens[C,A], outerName: Option[String]): NodeSeq => NodeSeq = {

    /*
     * Convenience method to return a lens for a given index of the `Map[Int,B]`
     *
     * @param index the index of the map we're lensing over.
     * @return a lens from `C` to `B`
     */
    def lensAtIndex(index: Int): Lens[C, B] = outerLens >=> lens >=> mapVLens(index) >=> optionLens[B]

    /*
     * Convenience method that creates the `JsCmd`s necessary to add a copy of a `B` form closed over the index
     *
     * @param index the index of the map we're currently lensing over
     * @return a unital method returning `JsCmd`
     */
    def addNewRecordForm(index: Int): () => JsCmd = { () =>
      Replace(
        "%s-elements".format(label(outerName, name)),
        renderAtIndex(index)( addRecordMemoize.applyAgain() ) ++ <div id={"%s-elements".format(label(outerName,name))}></div>
      )
    }

    /*
     * Remove a record at an index from the state object
     *
     * @param index the index of the map we're lensing over
     * @param state the state object to which we're updating
     * @param inputString a string from the user
     * @return an amended state object with the entry at `index` removed from the map under lens
     */
    def removeRecordFromState(index: Int)(state: C)(inputString: String): C = (outerLens >=> lens >=> mapVLens(index)).set(state, None)

    /*
     * Remove the record fields from the form
     *
     * @param index the index of the record we want to remove
     * @return a method that when called removes the desired form fields
     */
    def removeRecordFromForm(index: Int): String => JsCmd = { 
      val jsCmd = () => Replace("%s-%s".format(makeAdd(outerName, name), index), Nil)
      formStateUpdater( removeRecordFromState(index) )( jsCmd )
    }


    /*
     * Render the Record at a certain index. The surrounding chrome is intended to keep track of 
     * the index of the form we're updating and provide methods to add or remove copies. At the 
     * bottom of the form will be a "+" button to recursively add another form (index + 1).
     *
     * @param index the index of the record we want to remove
     * @return a method to transform `NodeSeq`
     */
    def renderAtIndex(index: Int): NodeSeq => NodeSeq = {
      "%s [id]".format(makeNameAdd(None, name)) #> "%s-%s".format(makeAdd(outerName, name), index) andThen
      indexedChromeRendering.map { _(index+1) }.getOrElse { makeName(None, "%s-number *".format(name)) #> (index+1) } andThen
      bRecord
        .fields
        .foldLeft( identity[NodeSeq]_ ){ (acc, field) =>
           val newLens: Lens[C, B] = lensAtIndex(index)
           acc andThen field.render(formStateUpdater, state)(newLens, Some(indexedLabel(outerName,index, name)))
         } andThen
      makeName(None, "%s-add [onclick]".format(name)) #> SHtml.onEvent( (s: String) => addNewRecordForm(index+1)() ) &
      makeName(None, "%s-remove [onclick]".format(name)) #> SHtml.onEvent( (s: String) => removeRecordFromForm(index)(s) )
    }

    // made lazy to avoid: 
    // http://stackoverflow.com/questions/13328502/what-does-forward-reference-extends-over-definition-of-value-mean-in-scala
    lazy val addRecordMemoize = SHtml.memoize( renderAtIndex(0) )

    val items: List[(Int, B)] = ((outerLens >=> lens).get(state)).toList

    items match {
      case Nil => 
        makeNameAdd(None, name) #> addRecordMemoize &
        "#%s-elements [id]".format(label(None, name)) #> "%s-elements".format(label(outerName, name))
      case _ => 
        makeNameAdd(None, name) #> addRecordMemoize &
        makeNameAdd(None, name) #> items.map { case (i, b) => renderAtIndex(i) } &
        "#%s-elements [id]".format(label(None, name)) #> "%s-elements".format(label(outerName, name))
    }

  }
}

/**
 * Typeahead field with Modal-add support (whoop!). If the item doesn't appear in the Typeahead results, add it!
 *
 * @param name The name of the field
 * @param typeaheadLabel the label that will appear at the typeahead
 * @param template For type `B`, what to do with B once we've added it.
 * @param lens Quite often you'll need to send an ID from the result of the Typeahead into a hidden form field. This 
 * lens should 'lens' over that field.
 * @param sideEffectB what do we do with B after we've added a new one? We're passed a function that can curry over the `uid` in-case we want to update a template. 
 */
case class TypeaheadField[A, B : HasFields : HasEmpty](
  name: String, 
  typeaheadLabel: String, 
  template: List[String], 
  sideEffectB: String => B => JsCmd,
  lens: Lens[A,String]
) extends DynamicField[A] with DynamicFormCreator {

  import DynamicField.{FormStateUpdate, label, makeName}

  def render[C](formStateUpdater: FormStateUpdate[C], state: C)(outerLens: Lens[C,A], outerName: Option[String]): NodeSeq => NodeSeq = {

    val fullLens = outerLens >=> lens

    def updateFunc(state: C)(string: String): C = fullLens.set(state,string) 
    def getFunc(state: C): String = fullLens.get(state)

    val jsCmd = () => Noop
    val fieldUpdateFunc = formStateUpdater(updateFunc)(jsCmd)
    val uid = label(outerName, name)

    /**
     * For the B form we need to pretend like this is a new Snippet
     */
    object bState extends RequestVar[B](implicitly[HasEmpty[B]].empty)
    def bUpdateState = getUpdateAndSaveFuncForField[B](bState)
    val bRenderFunction = renderField(bState)

    "@typeahead-label *" #> typeaheadLabel &
    "@typeahead-input [id]" #> uid &
    "@typeahead-modal-button [data-target]" #> "#%s-modal".format(uid) &
    "@typeahead-modal [id]" #> "%s-modal".format(uid) &
    "@typeahead-modal-save" #> SHtml.ajaxButton("Save", () => sideEffectB(uid)(bState.is)) &
    "@typeahead-hidden-input" #>  SHtml.ajaxText("", false, fieldUpdateFunc, "id" -> (uid + "-hidden")) &
    "@typeahead-modal-form" #> Templates(template).map { bRenderFunction } &
    "@typeahead-script-handler *" #> Unparsed("""wc.typeaheadWrapper('#%s', function(datum) { $('#%s-hidden').val(datum.id); $('#%s-hidden').blur(); }, '/api/artist/%%QUERY');""".format(uid, uid, uid))

  }
}

case class ManyTypeaheadField[A, B : HasFields : HasEmpty](
  name: String,
  typeaheadLabel: String,
  template: List[String],
  sideEffectB: String => B => JsCmd,
  manyLens: Lens[A, Map[Int, String]]
) extends DynamicField[A] with DynamicFormCreator {

  import DynamicField.{FormStateUpdate, label, makeName, optionLens}

  /*
   * Not sure why I have to do this, but it won't compile. the import doesn't work.
   * TODO: FIX
   */
  implicit object StringPrimitive extends HasFields[String] {
    val fields: List[DynamicField[String]] = List(
      BasicField[String]("primitive-string", lensId[String])
    )
  }

    import DynamicFieldPrimitives.{StringPrimitiveEmpty}
  def render[C](formStateUpdater: FormStateUpdate[C], state: C)(outerLens: Lens[C,A], outerName: Option[String]): NodeSeq => NodeSeq = {

    def lensAtIndex(index: Int): Lens[C,String] = 
      outerLens >=> manyLens >=> mapVLens(index) >=> optionLens[String](StringPrimitiveEmpty)

    def indexedRenderer(index: Int):NodeSeq => NodeSeq = {
      println("xxx Indexed renderer - index = %s , name = %s , outerName = %s".format(index, name, outerName))

     "@many-%s-number".format(name) #> index &
      makeName(outerName, name) #> TypeaheadField[C,B](label(outerName, name) + "-" + index.toString, typeaheadLabel, template, sideEffectB, lensAtIndex(index)).render(formStateUpdater, state)(lensId[C], outerName)     
    }

    ManyRecordField[A, String]("many-%s".format(name), manyLens, Some(indexedRenderer _)).render(formStateUpdater, state)(outerLens, outerName)
  }


}

/*
 * Companion object to the `DynamicField` trait. Supplies many helper methods
 */   
object DynamicField {

  /**
   * DEAD CODE?
   *
   * Auto-box `DynamicFields` to automagically turn them into `ManyRecordField`s
   */
  case class Wrapper[A](elem: A)
  def liftMany[A : HasFields : HasEmpty, B : HasFields : HasEmpty](field: DynamicField[A], lens: Lens[A, Map[Int, B]]): ManyRecordField[A,Wrapper[B]] = {
    /**
     * Create new getters and setters that wrap and unpack the values
     */
    val wrappedSetter: (A, Map[Int, Wrapper[B]]) => A = (a, wrappedMap) => 
      lens.set(a, wrappedMap.map { case (i,wb) => (i, wb.elem) })
    val wrappedGetter: A => Map[Int, Wrapper[B]] = (a: A) => 
      lens.get(a).map { case (i,b) => (i, Wrapper[B](b)) }

    val newLens: Lens[A, Map[Int, Wrapper[B]]] = Lens.lensu(wrappedSetter, wrappedGetter)

    implicit val wrappedFields = new HasFields[Wrapper[B]] {
      val fields: List[DynamicField[Wrapper[B]]] = List(
        RecordField[Wrapper[B],B]("wrapped", Lens.lensu( (w,b) => Wrapper[B](b), (w) => w.elem ) )
      )
    }
    implicit val wrappedEmpty = new HasEmpty[Wrapper[B]] {
      val empty: Wrapper[B] = Wrapper[B](implicitly[HasEmpty[B]].empty)
    }

    ManyRecordField[A, Wrapper[B]]("many", newLens)
  }

       
  type FormStateUpdate[A] = (A => String => A) => (() => JsCmd) => (String => JsCmd)
        
  /**
   * This should be unique for the entire form
   */
  def label(outerName: Option[String], name: String): String =
    "%s%s".format(outerName.map{ _ + "-"}.getOrElse(""), name)

  def indexedLabel(outerName: Option[String], index: Int, name: String): String = 
    "%s%s-%s".format(outerName.map { _ + "-" }.getOrElse(""),name, index)
        
  def makeName(outerName: Option[String], name: String): String =
    "name=%s".format(label(outerName, name))

  def makeNameAdd(outerName: Option[String], name: String): String =
    "name=add-%s".format(label(outerName, name))

  def makeAdd(outerName: Option[String], name: String): String =
    "add-%s".format(label(outerName, name))

  def makeInput(outerName: Option[String], name: String): String =
    "%s-input".format(makeName(outerName, name))

  /** TODO:
   * This lens is needed for the case that we want to update a field in a map or list
   * but it does not exist yet. In these cases, we need to create a `blank` instance of
   * the field that can be added to the map/list and then the field we're lensin' on can
   * be updated. E.g. you can't update the name of an `Artist` when no artist exists at 
   * index 0 in `Map[Int,Artist]`. 
   *
   * Alas, this Lens is not a true lens. It breaks the second lens law:
   *   2. forall a. lens.set(a, lens.get(a)) = a
   *   consider the case where a = None you will get:
   *   lens.set(None, lens.get(None))
   *   = lens.set(None, a.empty)
   *   = Some(a.empty)
   *   != None
   * Possible different approaches:
   *   1. Partial lenses
   *   2. State actions 
   *
   *   @return a "lens" from Option[A] to A
   */
  def optionLens[A : HasEmpty]: Lens[Option[A],A] = Lens.lensu(
    get = (oa) => oa.getOrElse( implicitly[HasEmpty[A]].empty )
  , set = (oa,a) => Some(a)
  )
}

/**
 * In case you need a multi field of a primitive
 */
object DynamicFieldPrimitives {

  lazy val stringLens: Lens[String,String] = Lens.lensu( (str,s) => s, (s) => s)

  implicit object StringPrimitive extends HasFields[String] {
    val fields: List[DynamicField[String]] = List(
      BasicField[String]("primitive-string", stringLens)
    )
  }

  implicit object StringPrimitiveEmpty extends HasEmpty[String] {
    val empty: String = ""
  }
}
