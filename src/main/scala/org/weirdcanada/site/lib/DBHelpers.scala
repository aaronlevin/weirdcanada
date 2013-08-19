package org.weirdcanada.site.lib

import scala.annotation.tailrec
import java.sql.{PreparedStatement, ResultSet}

object DBHelpers {

  @tailrec
  private def iterateOverResultSet[T](f: ResultSet => T, rs: ResultSet, results: List[T]): List[T] = rs.next() match {
    case false => results
    case true => iterateOverResultSet(f, rs, f(rs) :: results)
  }


  def executeQuery[T](st: PreparedStatement)(f: ResultSet => T): List[T] = {
    val rs = st.executeQuery()
    iterateOverResultSet(f, rs, Nil)
  }

}
