package org.weirdcanada.common.util

import org.weirdcanada.macros.SealedTraitMacros

/**
 * Ye olde provinces of Canada
 */
sealed trait Province {
  val name: String
  val slug: String
  val postalCode: String
}
case object BritishColumbia extends Province { val name = "British Columbia"; val slug = "british-columbia" ; val postalCode = "BC" }
case object Alberta extends Province { val name = "Alberta"; val slug = "alberta"; val postalCode = "AB" }
case object Saskatchewan extends Province { val name = "Saskatchewan"; val slug = "saskatchewan"; val postalCode = "SK" }
case object Manitoba extends Province { val name = "Manitoba"; val slug = "manitoba"; val postalCode = "MB" }
case object Ontario extends Province { val name = "Ontario"; val slug = "ontario"; val postalCode = "ON" }
case object Quebec extends Province { val name = "Québec"; val slug = "quebec"; val postalCode = "QC" }
case object NewBrunswick extends Province { val name = "New Brunswick"; val slug = "new-brunswick"; val postalCode = "NB" }
case object NewfoundlandAndLabrador extends Province { val name = "Newfoundland and Labrador"; val slug = "newfoundland-and-labrador"; val postalCode = "NL" }
case object PrinceEdwardIsland extends Province { val name = "Prince Edward Island"; val slug = "prince-edward-island"; val postalCode = "PE" }
case object Yukon extends Province { val name = "Yukon"; val slug = "yukon"; val postalCode = "YK" }
case object NorthwestTerritories extends Province { val name = "Northwest Territories"; val slug = "northwest-territories"; val postalCode = "NT" }
case object Nunavut extends Province { val name = "Nunavut"; val slug = "nunavut"; val postalCode = "NU" }
case object NovaScotia extends Province { val name = "Nova Scotia"; val slug = "nova-scotia"; val postalCode = "NS" }
case object OtherState extends Province { val name = "Other"; val slug = "other"; val postalCode = "xx" }

/**
 * Various helper methods relating to provinces
 */
object Province {

  /**
   * Set of all provinces
   */
  val provinces: Set[Province] = SealedTraitMacros.values[Province]

  /**
   * Tuples of province names and slug values
   * (used in forms)
   */
  val provinceNameTuples: Seq[(String, String)] = 
    provinces
      .toSeq
      .sortBy { _.slug }
      .map { p => (p.slug, p.name) }

  /**
   * Given a slug, find a province.
   *
   * @param func a function from `Province` to `Boolean`
   * @return an optional province (if it cannot be found)
   */
  def getProvinceForSlug(slug: String): Option[Province] = 
    provinces.find { _.slug == slug }

  /**
   * Given a name, find a province
   *
   * @param func a function from `Province` to `Boolean`
   * @return an optional province (if it cannot be found)
   */
  def getProvinceForName(name: String): Option[Province] = 
    provinces.find { _.name == name }

  /**
   * Given a postal id, find province
   *
   * @param func a function from `Province` to `Boolean`
   * @return an optional province (if it cannot be found)
   */
  def getProvinceForPostalCode(postalCode: String): Option[Province] = 
    provinces.find { _.postalCode == postalCode }
}
