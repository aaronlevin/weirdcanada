package com.weirdcanada.distro.charting

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import com.weirdcanada.distro.data.Sale
import com.weirdcanada.distro.charting.ChartBuilder._
import scala.xml.{NodeSeq, Unparsed}
import net.liftweb.util.Helpers.{encJs, randomString}

object Generators {
  def googleLineChart[X, Y](divId: String) = googleChart[X, Y](divId, "LineChart", "corechart", false)
  def googlePieChart[X, Y](divId: String) = googleChart[X, Y](divId, "PieChart", "corechart", true)
  def googleTableChart[X, Y](divId: String) = googleChart[X, Y](divId, "Table", "table", false)
  def googleGeoChart[X, Y](divId: String) = googleChart[X, Y](divId, "GeoChart", "geochart", false, "region" -> "021") // "021" -> North America
  def googleGeoChart[X, Y](divId: String, countryCode: String) = googleChart[X, Y](divId, "GeoChart", "geochart", false, "region" -> countryCode)

  private def googleChart[X, Y](divId: String, chartType: String, chartPackage: String, showLegend: Boolean, options: (String,String)*): Generator[X, Y, NodeSeq] = params => {
    val id = randomString(8)
    HasFormatting(
      params.labels,
      <script type="text/javascript">{Unparsed("""
        google.load("visualization", "1", {packages:['""" + chartPackage + """']});
        google.setOnLoadCallback(drawChart_""" + id + """);
        function drawChart_""" + id + """() {
          var data = google.visualization.arrayToDataTable([""" +
            "[%s, %s],".format(encJs(params.labels.xLabel), encJs(params.labels.yLabel)) +
            params.groups.map{ case DataPoint(label, value) =>
              "[%s,%s]".format(encJs(label.toString), value match { case s: String => encJs(s); case x @ _ => x })
            }.mkString(",")
          + """]);
          var options = {
            title: """ + encJs(params.labels.name) + """,
            vAxis: {format:'$#,###'},
            legend: {position: '""" + (if (showLegend) "right" else "none") + """'}
            """ + options.map{ case (label, value) => ",%s: %s".format(label, encJs(value))}.mkString("\n") + """
          };
  
          var chart = new google.visualization.""" + chartType + """(document.getElementById(""" + encJs(divId) + """));
          chart.draw(data, options);
        }
      """)}</script>
    )
  }
}
