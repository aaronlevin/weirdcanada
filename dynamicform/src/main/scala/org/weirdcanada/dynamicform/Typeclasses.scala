package org.weirdcanada.dynamicform

import scalaz.Lens

/*
 * Typeclass to encompass types that have a list of dynamic fields.
 */     
trait HasFields[A] {
  val fields: List[DynamicField[A]]
}       
      
/*    
 * Typeclass that encompasses types that have the notion of an "empty". For example,
 * consider: `case class Artist(name: String, url: String)`
 * an "empty" artist may be: `Artist("", "")`
 */ 
trait HasEmpty[A] {
  val empty: A
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

