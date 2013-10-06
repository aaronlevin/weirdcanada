package com.weirdcanada.distro.snippet

import net.liftweb.http.DispatchSnippet
import com.weirdcanada.distro.service.Service
import com.weirdcanada.distro.charting.ChartBuilder
import com.weirdcanada.distro.charting.ParamSources._
import com.weirdcanada.distro.charting.DataSources._
import com.weirdcanada.distro.charting.Aggregators._
import com.weirdcanada.distro.charting.Generators._
import scala.xml.NodeSeq
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import com.weirdcanada.distro.charting.ChartBuilder._
import com.weirdcanada.distro.data.Sale
import org.joda.time.DateTime

class ChartSnippet(service: Service) extends DispatchSnippet {
  var fromDate = DateTime.now.minusDays(30)
  var toDate = DateTime.now
  
  def dispatch = {
    case "mySalesByDate" => renderMySalesChart
    case "mySalesByFormat" => renderMySalesByFormatChart
    case "mySalesBySku" => renderMySalesBySkuChart
    case "mySalesByCountry" => renderMySalesByCountryChart
    case "mySalesByProvince" => renderMySalesByProvinceChart
    case "allSalesByDate" => renderAllSalesChart
    case "allSalesByFormat" => renderAllSalesByFormatChart
    case "totalSales" => renderTotalSalesChart
  }

  
  // Specialized helper methods to define the charts we care about
  object Charts {
    private def dataSource = if (Props.devMode) getTestSales else getSalesFromMapperDB
    private def dateRangeForAccount = specifyInputs(fromDate, toDate) _
    private def dateRangeForAll = dateRangeForAccount(None)
    
    
    def getMySalesByDate(divId: String) = {
      val accountOpt = service.SessionManager.current.accountOpt
      ChartBuilder(DateRangeInput(ChartLabels("My Sales", "Date", "Sales")))(dateRangeForAccount(accountOpt))(dataSource)(groupConsignorRevenueByDay)(googleLineChart(divId))
    }
    
    def getMySalesByFormat(divId: String) = {
      val accountOpt = service.SessionManager.current.accountOpt
      ChartBuilder(DateRangeInput(ChartLabels("My Sales by Format", "Format", "Sales")))(dateRangeForAccount(accountOpt))(dataSource)(groupConsignorRevenueByFormat)(googlePieChart(divId))
    }
    
    def getMySalesBySku(divId: String) = {
      val accountOpt = service.SessionManager.current.accountOpt
      ChartBuilder(DateRangeInput(ChartLabels("My Sales by SKU", "SKU", "Sales")))(dateRangeForAccount(accountOpt))(dataSource)(groupConsignorRevenueBySku)(googleTableChart(divId))
    }
    
    def getMySalesByProvince(divId: String) = {
      val accountOpt = service.SessionManager.current.accountOpt
      ChartBuilder(DateRangeInput(ChartLabels("My Sales by Province", "Province", "Sales")))(dateRangeForAccount(accountOpt))(dataSource)(groupConsignorRevenueByProvince("Canada"))(googleGeoChart(divId, "CA"))
    }
    
    def getMySalesByCountry(divId: String) = {
      val accountOpt = service.SessionManager.current.accountOpt
      ChartBuilder(DateRangeInput(ChartLabels("My Sales by Country", "Country", "Sales")))(dateRangeForAccount(accountOpt))(dataSource)(groupConsignorRevenueByCountry)(googleGeoChart(divId))
    }
    
    def getAllSalesByDate(divId: String) = 
      ChartBuilder(DateRangeInput(ChartLabels("All Sales", "Date", "Sales")))(dateRangeForAll)(dataSource)(groupConsignorRevenueByDay)(googleLineChart(divId))
    
    def getAllSalesByFormat(divId: String) = 
      ChartBuilder(DateRangeInput(ChartLabels("All Sales by Format", "Format", "Sales")))(dateRangeForAll)(dataSource)(groupConsignorRevenueByFormat)(googlePieChart(divId))
      
    def getTotalSalesByDate(divId: String) =
      ChartBuilder(DateRangeInput(ChartLabels("Total Sales", "Date", "Sales")))(dateRangeForAll)(dataSource)(groupTotalRevenueByDay)(googleLineChart(divId))
  }

  
  def renderMySalesChart = {
    require(service.SessionManager.current.isLoggedIn)
    
    "*" #> S.attr("divId").map(Charts.getMySalesByDate)
  }

  def renderMySalesByFormatChart = {
    require(service.SessionManager.current.isLoggedIn)
    
    "*" #> S.attr("divId").map(Charts.getMySalesByFormat)
  }

  def renderMySalesBySkuChart = {
    require(service.SessionManager.current.isLoggedIn)
    
    "*" #> S.attr("divId").map(Charts.getMySalesBySku)
  }
  
  def renderMySalesByProvinceChart = {
    require(service.SessionManager.current.isLoggedIn)

    // TODO: allow user to select country instead of hardcoding to Canada
    
    "*" #> S.attr("divId").map(Charts.getMySalesByProvince)
  }
  
  def renderMySalesByCountryChart = {
    require(service.SessionManager.current.isLoggedIn)
    
    "*" #> S.attr("divId").map(Charts.getMySalesByCountry)
  }
  
  def renderAllSalesChart = {
    require(service.SessionManager.current.isAdmin)
    
    "*" #> S.attr("divId").map(Charts.getAllSalesByDate)
  }
  
  def renderAllSalesByFormatChart = {
    require(service.SessionManager.current.isAdmin)
    
    "*" #> S.attr("divId").map(Charts.getAllSalesByFormat)
  }  
  
  def renderTotalSalesChart = {
    require(service.SessionManager.current.isAdmin)
    
    "*" #> S.attr("divId").map(Charts.getTotalSalesByDate)
  }  
}