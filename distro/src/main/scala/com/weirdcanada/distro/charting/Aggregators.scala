package com.weirdcanada.distro.charting

import com.weirdcanada.distro.charting.ChartBuilder._
import org.joda.time.DateTime
import com.weirdcanada.distro.data.Sale
import com.weirdcanada.distro.data.Album
import org.joda.time.format.DateTimeFormat

object Aggregators {
  // aggregate is a helper method that walks through an ordered data set to collect
  // similar entries by key and calculate an aggregate value of the grouped rows.
  def aggregate[D, K, V](rows: Seq[D], fKey: D => K, fValue: D => V, fAggregate: (V, V) => V): Seq[DataPoint[K,V]] = {
    // Use helper class to accumulate values as we iterate through the dataset
    case class Helper(seq: Seq[DataPoint[K,V]], workingGroup: Option[DataPoint[K,V]])
    
    // Process rows using foldLeft instead of groupBy so we can run in O(n) time
    // (Note, it's assumed that the incoming data is already sorted by key)
    rows.foldLeft(Helper(Seq.empty, None)) {
      (helper, row) =>
        val currentLabel = fKey(row)
        val currentValue = fValue(row)
        
        helper.workingGroup
          .filter(_.label == currentLabel) // Does the current row belong to the current working group?
          .map( wg =>
            helper.copy(
              // Yes, add the current value to the current working group
              workingGroup = Some(wg.copy(value = fAggregate(wg.value, currentValue)))
            )
          )
          .getOrElse(
            // No, move the old working group into the sequence and start a new working group with current values
            helper.copy(
              seq = helper.seq ++ helper.workingGroup.toSeq,
              workingGroup = Some(DataPoint(currentLabel, currentValue))
            )
          )
    } match {
      case Helper(seq, finalGroup) => seq ++ finalGroup.toSeq
    }
  }

  private def revenueForConsignor(sale: Sale) = sale.amount.is - sale.markUp.is
  
  private val dateFormatter = DateTimeFormat.forPattern("MMM dd, yyyy")

  val groupWCRevenueByDay = groupSalesByDay(sale => sale.markUp.is)
  val groupTotalRevenueByDay = groupSalesByDay(sale => sale.amount.is)
  val groupConsignorRevenueByDay = groupSalesByDay(revenueForConsignor)
  
  private def groupSalesByDay(saleToBD: Sale => BigDecimal): Aggregator[Sale, String, BigDecimal] = hasData => {
    HasGroups(
      hasData.labels,
      aggregate[Sale, String, BigDecimal](
        hasData.rows,
        sale => dateFormatter.print(new DateTime(sale.dateTime.is).withTimeAtStartOfDay),
        saleToBD,
        _ + _
      )
    )
  }

  val groupConsignorRevenueByFormat: Aggregator[Sale, String, BigDecimal] = hasData => {
    HasGroups(
      hasData.labels,
      aggregate[Sale, String, BigDecimal](
        hasData.rows.sortBy(_.format.is),
        sale => sale.format.is.toString,
        sale => sale.amount.is - sale.markUp.is,
        _ + _
      )
    )
  }
  
  val groupConsignorRevenueByCountry = groupSalesByCountry(revenueForConsignor)
  val groupTotalRevenueByCountry = groupSalesByCountry(sale => sale.amount.is)
  
  private def groupSalesByCountry(saleToBD: Sale => BigDecimal): Aggregator[Sale, String, BigDecimal] = hasData => {
    HasGroups(
      hasData.labels,
      aggregate[Sale, String, BigDecimal](
        hasData.rows.sortBy(_.country.is),
        sale => sale.country.is,
        saleToBD,
        _ + _
      )
    )
  }
  

  def groupConsignorRevenueByProvince(country: String) = groupSalesByProvince(country, revenueForConsignor)
  private def groupSalesByProvince(country: String, saleToBD: Sale => BigDecimal): Aggregator[Sale, String, BigDecimal] = hasData => {
    HasGroups(
      hasData.labels,
      aggregate[Sale, String, BigDecimal](
        hasData.rows.filter(_.country.is == country).sortBy(_.province.is),
        sale => "CA-" + sale.province.is, // TODO: remove assumption on Canada
        saleToBD,
        _ + _
      )
    )
  }

  /*
  // TODO
  val groupConsignorRevenueBySku: Aggregator[Sale, String, (BigDecimal, Int)] = hasData => {
    HasGroups(
      hasData.labels,
      aggregate[Sale, String, (BigDecimal, Int)](
        hasData.rows.sortBy(_.sku.is),
        sale => sale.sku.is,
        sale => (sale.amount.is - sale.markUp.is, sale.quantity.is),
        (t1, t2) => (t1._1 + t2._1, t1._2 + t2._2)
      )
    )
  }
  */
  val groupConsignorRevenueBySku: Aggregator[Sale, String, BigDecimal] = hasData => {
    HasGroups(
      hasData.labels,
      aggregate[Sale, String, BigDecimal](
        hasData.rows.sortBy(_.sku.is),
        sale => sale.sku.is,
        sale => sale.amount.is - sale.markUp.is,
        _ + _
      )
    )
  }
}
