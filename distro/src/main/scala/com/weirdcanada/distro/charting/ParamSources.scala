package com.weirdcanada.distro.charting

import com.weirdcanada.distro.charting.ChartBuilder._
import org.joda.time.DateTime
import com.weirdcanada.distro.data.Account

object ParamSources {
  def trailing30Days(account: Option[Account]): ParamSource = params => {
    HasInput(
      params.labels,
      params.getParameters.collect {
        case fromParam @ FromDateParameter => fromParam -> DateTime.now.minusDays(30)
        case toParam @ ToDateParameter => toParam -> DateTime.now
        case whoParam @ AccountParameter if account.isDefined => whoParam -> account.get
      }.toMap
    )
  }

  def specifyInputs(fromDate: DateTime, toDate: DateTime)(account: Option[Account]): ParamSource = params => {
    require(!fromDate.isAfter(toDate))
    
    HasInput(
      params.labels,
      params.getParameters.collect {
        case fromParam @ FromDateParameter => fromParam -> fromDate
        case toParam @ ToDateParameter => toParam -> toDate
        case whoParam @ AccountParameter if account.isDefined => whoParam -> account.get
      }.toMap
    )
  }
}
