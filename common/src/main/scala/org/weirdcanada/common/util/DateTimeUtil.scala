package org.weirdcanada.common.util

import org.joda.time.DateTime

object DateTimeUtil {

  val DoubleDateParse = """([\d]{4}\-[\d]{2}\-[\d]{2})\-([\d]{4}\-[\d]{2}\-[\d]{2})""".r

  /**
   * Method to get the start and end dates for "This week". Weeks begin on
   * Monday.
   */
  def thisWeek: (DateTime, DateTime) = {

    val now = new DateTime
    val weekStart = now.withDayOfWeek(1).withTimeAtStartOfDay()
    val weekEnd = weekStart.plusWeeks(1).minusMillis(2)

    (weekStart, weekEnd)
  }

  /**
   * Method to get the start and end dates for "This Month". 
   */
  def thisMonth: (DateTime, DateTime) = {

    val now = new DateTime
    val monthStart = now.withDayOfMonth(1).withTimeAtStartOfDay
    val monthEnd = monthStart.plusMonths(1).minusMillis(2)

    (monthStart, monthEnd)
  }

  /**
   * Method to get the start and end dates for "All Time". In this case, the end
   * date is right now and the beginning is 1970.
   */
  def allTime: (DateTime, DateTime) = {
    val now = new DateTime
    val start = now.withYear(1970)
    (start, now)
  }

  /**
   * A method to receive a string and return a pair of dates. The format of the
   * string is assumed to be:
   *
   * `YYYY-MM-DD-YYYY-MM-DD`
   *
   * @params dateString the string to be parsed
   * @returns an optional tuple
   */
  def parseDoubleDate(dateString: String): Option[(DateTime, DateTime)] =
    dateString match {
      case DoubleDateParse(startDateString, endDateString) => 
        Some((new DateTime(startDateString), (new DateTime(endDateString)).plusDays(1).minusMillis(2) ))
      case _ => None
    }

}

