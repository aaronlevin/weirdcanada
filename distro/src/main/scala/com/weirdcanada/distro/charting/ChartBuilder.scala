package org.weirdcanada.distro.charting

import org.joda.time.DateTime
import org.weirdcanada.distro.data.Sale
import scala.xml.{NodeSeq, Unparsed}
import org.joda.time.format.DateTimeFormat
import org.weirdcanada.distro.data.Account


class ChartBuilder[A](val state: A) {
  def map[B](f: A => B): ChartBuilder[B] = {
    new ChartBuilder(f(state))
  }
}

object ChartBuilder {
  case class ChartLabels(name: String, xLabel: String, yLabel: String)

  abstract class Parameter[T](val name: String)
  
  trait NeedsInput {
    def labels: ChartLabels
    def getParameters: Seq[Parameter[_]] = Seq.empty
  }
  trait NeedsData {
    def labels: ChartLabels
    def inputs: Map[Parameter[_], _] = Map.empty
  }
  trait NeedsGrouping[T] {
    def labels: ChartLabels
    def rows: Seq[T] = Seq.empty  
  }
  trait NeedsFormatting[K, V] {
    def labels: ChartLabels
    def groups: Seq[DataPoint[K, V]]
  }
  trait NeedsOutput[T] {
    def labels: ChartLabels
    def output: T
  }

  
  type ParamSource = NeedsInput => NeedsData
  type DataSource[T] = NeedsData => NeedsGrouping[T]
  type Aggregator[T,K,V] = NeedsGrouping[T] => NeedsFormatting[K,V]
  type Generator[K,V,T] = NeedsFormatting[K,V] => NeedsOutput[T]
  
  case object AccountParameter extends Parameter[Account]("Account")
  case object FromDateParameter extends Parameter[DateTime]("From Date")
  case object ToDateParameter extends Parameter[DateTime]("To Date")
  
  case class DateRangeInput(labels: ChartLabels) extends NeedsInput {
    override def getParameters: Seq[Parameter[_]] = {
      FromDateParameter +: ToDateParameter +: super.getParameters
    }
  }
  
  case class HasInput(override val labels: ChartLabels, override val inputs: Map[Parameter[_], _]) extends NeedsData
  case class HasData[T](override val labels: ChartLabels, override val rows: Seq[T]) extends NeedsGrouping[T]
  case class HasGroups[K, V](override val labels: ChartLabels, override val groups: Seq[DataPoint[K, V]]) extends NeedsFormatting[K, V]
  case class HasFormatting[T](override val labels: ChartLabels, override val output: T) extends NeedsOutput[T]
  
  
  case class DataPoint[K,V](label: K, value: V)

  
  def apply[T <: NeedsInput, I, X, Y, O](t: T)(fetchParams: ParamSource)(fetchData: DataSource[I])(aggregateData: Aggregator[I,X,Y])(generateOutput: Generator[X,Y,O]) =
    new ChartBuilder[T](t)
      .map(fetchParams)
      .map(fetchData)
      .map(aggregateData)
      .map(generateOutput)
      .state.output
}
