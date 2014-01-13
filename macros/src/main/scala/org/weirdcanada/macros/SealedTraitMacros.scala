package org.weirdcanada.macros

import language.experimental.macros
import scala.reflect.macros.Context

/**
 * Macros for dealing with sealed traits
 */
object SealedTraitMacros {

  def values[A]: Set[A] = macro values_impl[A]

  /**
   * Will a set of extensions of sealed traits
   * See: http://stackoverflow.com/questions/13671734/iteration-over-a-sealed-trait-in-scala
   */
  def values_impl[A: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val symbol = weakTypeOf[A].typeSymbol

    if (!symbol.isClass) c.abort(
      c.enclosingPosition,
      "Can only enumerate values of a sealed trait or class."
    ) else if (!symbol.asClass.isSealed) c.abort(
      c.enclosingPosition,
      "Can only enumerate values of a sealed trait or class."
    ) else {
      val children = symbol.asClass.knownDirectSubclasses.toList

      if (!children.forall(_.isModuleClass)) c.abort(
        c.enclosingPosition,
        "All children must be objects."
      ) else c.Expr[Set[A]] {
        def sourceModuleRef(sym: Symbol) = Ident(sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sourceModule.asInstanceOf[Symbol])
        Apply(Select(reify(Set).tree, newTermName("apply")), children.map(sourceModuleRef(_)))
      }


    }
  }
}

