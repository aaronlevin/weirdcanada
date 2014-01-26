package org.weirdcanada.common.util

import org.weirdcanada.macros.SealedTraitMacros



/**
 * Various helper methods relating to provinces
 */
object PhysicalCondition {

  /**
   * Goldmine scale of grading whooo hee.
   */
  sealed trait PhysicalCondition {
    val name: String
    val longName: String
    val slug: String
    val description: String
    val order: Int
  }

  case object StillSealed extends PhysicalCondition { 
    val name = "SS"
    val longName = "Still Sealed"
    val slug = "ss"
    val description = ""
    val order = 0
  }

  case object NearMint extends PhysicalCondition { 
    val name = "NM"
    val longName = "Near Mint"
    val slug = "nm"
    val description = ""
    val order = 1
  }

  case object Excellent extends PhysicalCondition { 
    val name = "EX"
    val longName = "Excellent"
    val slug = "ex"
    val description = ""
    val order = 2
  }

  case object VeryGoodPlus extends PhysicalCondition { 
    val name = "VG+"
    val longName = "Very Good +"
    val slug = "vg-plus"
    val description = ""
    val order = 3
  }

  case object VeryGood extends PhysicalCondition { 
    val name = "VG"
    val longName = "Very Good"
    val slug = ""
    val description = ""
    val order = 4
  }

  case object VeryGoodMinus extends PhysicalCondition { 
    val name = "VG-"
    val longName = "Very Good -"
    val slug = "vg-minus"
    val description = ""
    val order = 5
  }

  case object Good extends PhysicalCondition { 
    val name = "Good"
    val longName = "Good"
    val slug = "good"
    val description = ""
    val order = 6
  }

  case object Poor extends PhysicalCondition { 
    val name = "Poor"
    val longName = "Poor"
    val slug = "poor"
    val description = ""
    val order = 7
  }
  /**
   * Set of all conditions
   */
  val physicalConditions: Set[PhysicalCondition] = SealedTraitMacros.values[PhysicalCondition]

  /**
   * Tuples of province names and slug values
   * (used in forms)
   */
  val conditionNameTuples: Seq[(String, String)] = 
    physicalConditions
      .toSeq
      .sortBy { _.order}
      .map { p => (p.slug, p.longName) }

  /**
   * Given a slug, find a `PhysicalCondition`.
   *
   * @param slug a slug
   * @return an optional province (if it cannot be found)
   */
  def fromSlug(slug: String): Option[PhysicalCondition] = 
    physicalConditions.find { _.slug == slug }

}
