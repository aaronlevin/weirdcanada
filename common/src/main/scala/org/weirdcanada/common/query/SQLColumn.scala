package org.weirdcanada.common.query

import language.implicitConversions


/**
 * SQL COLUMN
 */
case class SQLColumn(name: String, table: Option[SQLTable]) {
  def ===(column: SQLColumn): (SQLColumn, SQLColumn) = (this, column)
  lazy val render: String = (
    for { 
      t <- table
      alias <- t.alias
    } yield "%s.%s".format(alias, name)
    ).getOrElse( name )

}
object SQLColumn {
  implicit class SQLColumnSyntax(columnName: String) {
    def in(table: SQLTable): SQLColumn = SQLColumn(columnName, Some(table))
    def ===(column: SQLColumn): (SQLColumn, SQLColumn) = (SQLColumn(columnName, None), column)
  }
  implicit def string2Column(columnName: String): SQLColumn = SQLColumn(columnName, None)


}
