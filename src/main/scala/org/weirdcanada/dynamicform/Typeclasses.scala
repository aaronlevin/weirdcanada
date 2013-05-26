package org.weirdcanada.dynamicform

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
