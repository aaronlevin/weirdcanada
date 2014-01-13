package org.weirdcanada.common.util

import org.weirdcanada.macros.SealedTraitMacros

/**
 * Thee countries of the world
 */

sealed trait Country {
  val name: String
  val slug: String
}
case object Canada extends Country { val name = "Canada"; val slug = "canada" }
case object USA extends Country { val name = "United State of America"; val slug = "usa" }
case object Japan extends Country { val name = "Japan"; val slug = "japan" }

object Country {

  /**
   * Set of all countries
   */
  val countries: Set[Country] = SealedTraitMacros.values[Country]

  /**
   * Tuples of country names and slug values
   */
  val countryTuples: Seq[(String, String)] =
    countries
      .toSeq
      .sortBy { _.slug }
      .map { c => (c.slug, c.name) }

  /**
   * Given a slug, find a country
   *
   * @param slug a slug (usually a cleansed version of the country
   * @returns an optional country
   */
  def getCountryBySlug(slug: String): Option[Country] =
    countries.find { _.slug == slug }

}
