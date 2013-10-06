package com.weirdcanada.distro.charting

import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import com.weirdcanada.distro.data.Sale
import com.weirdcanada.distro.charting.ChartBuilder._
import com.weirdcanada.distro.data.Account
import com.weirdcanada.distro.data.Album


object DataSources {
  val getTestSales: DataSource[Sale] = hasInput => {
    def randomFormat = Album.Type.values.drop(randomInt(Album.Type.values.size)).head
    def randomProvince = 
      List("BC", "AB", "SK", "MB", "ON", "QC", "NL", "NB", "NS", "PE", "YT", "NT", "NU")
        .drop(randomInt(13)).head
    def randomSku = 
      List("ACME-100", "ACME-101", "ACME-200", "ACME-300", "ACME-400", "ACME-404")
        .drop(randomInt(6)).head
    
    def randomSale =
      Sale.create
        .sku(randomSku)
        .amount(randomInt(1800) / 100.00 + 2)
        .markUp(1.00)
        .dateTime(DateTime.now.minusDays(randomInt(7)).toDate)
        .format(randomFormat)
        .country("Canada")
        .province(randomProvince)

    HasData[Sale](
      hasInput.labels,
      (0 to 25).map(_ => randomSale).sortBy(_.dateTime.is)
    )
  }
  
  val getSalesFromMapperDB: DataSource[Sale] = hasInput => {
    HasData[Sale](
      hasInput.labels,
      Sale.findAll(
        (hasInput.inputs.collect {
          case (fromParam @ FromDateParameter, fromDate: DateTime) =>
            By_>=(Sale.dateTime, fromDate.toDate)
            
          case (toParam @ ToDateParameter, toDate: DateTime) =>
            By_<=(Sale.dateTime, toDate.toDate)
            
          case (whoParam @ AccountParameter, account: Account) =>
            By(Sale.consignor, account)
        })
        .map(_.asInstanceOf[QueryParam[Sale]]).toSeq : _*
      )
    )
  }
}
