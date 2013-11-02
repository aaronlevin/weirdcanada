package org.weirdcanada.common.query

import scalaz.Free
import Free.{Suspend, Return}

import JoinTypeInstances._

import language.implicitConversions

/**
 * SQL TABLE
 */
case class SQLTable(name: String, alias: Option[String]) {

  lazy val render: String = alias.map { a => "%s AS %s".format(name, a) }.getOrElse { name }
  
  import FreeQuery._
  def column(columnName: String): Free[FreeQuery,SQLColumn] = 
    Suspend(Column(SQLColumn(columnName, Some(this)), t => Return(t)))

  /**
   * Alias for "ON". Must use operator here for precedence ordering.
   */
  def |*|(columnPair: (SQLColumn, SQLColumn)): (SQLTable, SQLColumn, SQLColumn) = 
    (this, columnPair._1, columnPair._2)

  def innerJoin(triple: (SQLTable, SQLColumn, SQLColumn)): Free[FreeQuery, Unit] =
    Suspend(JoinedTables(InnerJoin, this, triple._1, Some((triple._2, triple._3)), Return(())))

  def leftJoin(triple: (SQLTable, SQLColumn, SQLColumn)): Free[FreeQuery, Unit] =
    Suspend(JoinedTables(LeftJoin, this, triple._1, Some((triple._2, triple._3)), Return(())))

  def rightJoin(triple: (SQLTable, SQLColumn, SQLColumn)): Free[FreeQuery, Unit] =
    Suspend(JoinedTables(RightJoin, this, triple._1, Some((triple._2, triple._3)), Return(())))

  def outerJoin(triple: (SQLTable, SQLColumn, SQLColumn)): Free[FreeQuery, Unit] =
    Suspend(JoinedTables(OuterJoin, this, triple._1, Some((triple._2, triple._3)), Return(())))

  def crossJoin(table: SQLTable): Free[FreeQuery, Unit] = 
    Suspend(JoinedTables(CrossJoin, this, table, None, Return(())))

}
object SQLTable {
  implicit class SQLTableSyntax(tableName: String) {
    def as(alias: String): SQLTable = SQLTable(tableName, Some(alias))
  }
  implicit def string2Table(tableName: String): SQLTable = SQLTable(tableName, None)
}
