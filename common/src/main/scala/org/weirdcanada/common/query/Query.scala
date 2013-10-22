package org.weirdcanada.common.query

import scalaz.{Free, FreeInstances, Functor, \/, -\/, \/-}
import Free._

import language.implicitConversions

import java.sql.{PreparedStatement, Types}

sealed trait JDBCValue[A] {
  def set(st: PreparedStatement, i: Int, a: A): Unit
  val sqlType: Int
}
object JDBCValue {

  implicit object jdbcString extends JDBCValue[String] {
    def set(st: PreparedStatement, i: Int, a: String): Unit =
      st.setString(i,a)
    val sqlType: Int = Types.VARCHAR
  }

  implicit object jdbcInt extends JDBCValue[Int] {
    def set(st: PreparedStatement, i: Int, a: Int): Unit = 
      st.setInt(i,a)
    val sqlType: Int = Types.INTEGER
  }

  implicit object jdbcBigInt extends JDBCValue[Long] {
    def set(st: PreparedStatement, i: Int, a: Long): Unit = 
      st.setLong(i,a)
    val sqlType: Int = Types.BIGINT
  }

  implicit class JDBCIterable[A : JDBCValue](as: Iterable[A]) extends JDBCValue[Iterable[A]] {
    val sqlType: Int = Types.BIGINT
    val jdbcVal: JDBCValue[A] = implicitly[JDBCValue[A]]
    def set(st: PreparedStatement, i: Int, as: Iterable[A]): Unit = 
      jdbcVal.set(st, i, as.head)
  }

}
/**
 * SELECT c.id FROM cool AS c
 *
 * I'd like to get the syntax up to this point perhaps. 
 *
 * for {
 *   table1 <- table1
 *   column2 <- table2.column2
 *   table2 <- table2
 *   _ <- select(table1."column1" :: column2 :: Nil)
 *   _ <- from (table1 inner join table2 on table1."column1" === table2."column2")
 *   _ <- where {
 *     for {
 *       _ <- (table1."one" === "two") and (table2."three" === "two")
 *       _ <- or
 *       _ <- table2."one" === "three"
 *     } yield ()
 *   }
 * } yield ()
 */


/**
 * SQL COLUMN
 */
case class SQLColumn(name: String, table: Option[SQLTable])
object SQLColumn {
  implicit class SQLColumnSyntax(columnName: String) {
    def in(table: SQLTable): SQLColumn = SQLColumn(columnName, Some(table))
  }
  implicit def string2Column(columnName: String): SQLColumn = SQLColumn(columnName, None)
}

/**
 * SQL Conditionals
 */
sealed abstract class Conditional[A : JDBCValue] { 
  def getSetter: JDBCValue[A] = implicitly[JDBCValue[A]]
}

case class Eq[A : JDBCValue](column: String, value: A) extends Conditional[A]
case class DoesNotEqual[A : JDBCValue](column: String, value: A) extends Conditional[A]
case class LessThan[A : JDBCValue](column: String, value: A) extends Conditional[A]
case class LessThanOrEqualTo[A : JDBCValue](column: String, value: A) extends Conditional[A]
case class In[A : JDBCValue](column: String, values: Iterable[A]) extends Conditional[A]

object Conditional {
  implicit class ConditionalSyntax(string: String) {  
    def ===[A : JDBCValue](a: A): Conditional[A] = Eq(string, a)
    def =!=[A : JDBCValue](a: A): Conditional[A] = DoesNotEqual(string, a)
    def <[A : JDBCValue](a: A): Conditional[A] = LessThan(string, a)
    def <=[A : JDBCValue](a: A): Conditional[A] = LessThanOrEqualTo(string, a)
    def in[A : JDBCValue](as: Iterable[A]): Conditional[A] = In(string, as)
  }
}

/**
 * The Free Query Free Monad
 */
sealed trait FreeQuery[+A] 

case class Select[A](columns: List[SQLColumn], next: A) extends FreeQuery[A]
case class From[A](table: SQLTable, next: A) extends FreeQuery[A] 
case class FromQ[A](subQuery: Free[FreeQuery,Unit], next: A) extends FreeQuery[A]
case class Where[A](logics: List[Conditional[_]], next: A) extends FreeQuery[A] {
  def and[B](newLogic: Conditional[B]): Where[A] = Where(this.logics ::: newLogic :: Nil, next)
}
case class Table[A](table: SQLTable, func: SQLTable => A) extends FreeQuery[A]
case class Column[A](column: SQLColumn, func: SQLColumn => A) extends FreeQuery[A]
case object Done extends FreeQuery[Nothing]

object FreeQuery {

  implicit object queryFunctor extends Functor[FreeQuery] {

    def map[A, B](fa: FreeQuery[A])(f: A => B): FreeQuery[B] = fa match {
      case Select(columns, next) => Select(columns, f(next))
      case From(tables, next) => From(tables, f(next) )
      case FromQ(subQuery, next) => FromQ(subQuery, f(next))
      case Where(logics, next) => Where(logics, f(next))
      case Table(table, g) => Table(table, f compose g) 
      case Column(column, g) => Column(column, f compose g)
      case Done => Done
    }
  }

  def select(columns: List[SQLColumn]): Free[FreeQuery, Unit] =
    Suspend(Select(columns, Return(())))

  def from(table: SQLTable): Free[FreeQuery, Unit] =
    Suspend(From(table, Return(())))

  def fromQ(subQuery: Free[FreeQuery,Unit]): Free[FreeQuery, Unit] = 
    Suspend(FromQ(subQuery, Return(())))
 
  def done: Free[FreeQuery, Unit] = 
    Return(Done)

  def where[A](logics: List[Conditional[A]]): Free[FreeQuery, Unit] = 
    Suspend(Where(logics, Return(())))

  def table(table: SQLTable): Free[FreeQuery, SQLTable] = 
    Suspend(Table(table, t => Return(t)))

  private def renderConditional(conditional: Conditional[_]): String = conditional match {
    case Eq(column, _) => "%s = ?".format(column)
    case DoesNotEqual(column, _) => "%s <> ?".format(column)
    case LessThan(column, _) => "%s < ?".format(column)
    case LessThanOrEqualTo(column, _) =>  "%s <= ?".format(column)
    case In(column, values) => "%s IN (%s)".format( column, values.map {_ => "?" }.mkString(",") ) 
  }

  def sqlInterpreter[A](query: Free[FreeQuery,A], statements: List[String]): String = query.resume match {
    case -\/(freeValue) => freeValue match {
      case Select(columns, a) => 
        val columnString: List[String] = 
          columns.map { column => 
            (for {
              table <- column.table
              alias <- table.alias
            } yield "%s.%s".format(alias, column.name)).getOrElse { column.name }
          }
        sqlInterpreter(a, statements :::  "select %s".format(columnString.mkString(",")) :: Nil)
      case From(table, a) => 
        val sqlString: String = table.alias match {
          case None => "FROM %s".format(table.name)
          case Some(as) => "FROM %s AS %s".format(table.name, as)
        }
        sqlInterpreter(a, statements ::: sqlString :: Nil)
      case FromQ(subquery, a) => sqlInterpreter(a, statements ::: "from ( %s )".format(sqlInterpreter(subquery,Nil)) :: Nil)
      case Where(logics, a) => 
        val whereStatement: String = logics.map { renderConditional }.mkString(" AND ") match {
          case "" => ""
          case string => "WHERE %s".format(string)
        }
        sqlInterpreter(a, statements ::: whereStatement :: Nil )
      case Table(table, tableFunc) => sqlInterpreter(tableFunc(table), statements)
      case Column(column, columnFunc) => sqlInterpreter(columnFunc(column), statements)
      case Done => statements.mkString("\n")
    }
    case \/-(endValue) => statements.mkString("\n")
  }

  /**
   * TODO: Fix to handle `SQLTable` and `SQLColumn`
   */
  def sqlPrepared[A](query: Free[FreeQuery, A], st: PreparedStatement, counter: Int = 1): (PreparedStatement, Int) = query.resume match {
    case -\/(freeValue) => freeValue match {
      case Select(columns, a) => sqlPrepared(a, st, counter)
      case From(table, a) => sqlPrepared(a, st, counter)
      case FromQ(subquery, a) => sqlPrepared(a, st, counter)
      case Where(logics, a) => 
        val (newStatement, newCounter): (PreparedStatement, Int) = logics.foldLeft( (st, counter) ){ (acc, logic) => logic match {
          case conditional @ Eq(_, value) => conditional.getSetter.set(st, counter, value); (st, counter + 1)
          case conditional @ DoesNotEqual(_, value) => conditional.getSetter.set(st, counter, value); (st, counter + 1)
          case conditional @ LessThan(_, value) => conditional.getSetter.set(st, counter, value); (st, counter + 1)
          case conditional @ LessThanOrEqualTo(_, value) => conditional.getSetter.set(st, counter, value); (st, counter + 1)
          case conditional @ In(_, values) => 
            values.foldLeft( (st, counter) ){ case ( (newSt, newInCounter), value) => 
              conditional.getSetter.set(newSt, newInCounter, value)
              (newSt, newInCounter + 1)
            }
        }}
        sqlPrepared(a, newStatement, newCounter)
      case Table(table, tableFunc) => sqlPrepared(tableFunc(table), st, counter)
      case Column(column, columnFunc) => sqlPrepared(columnFunc(column), st, counter)
      case Done => (st, counter)
    }
    case \/-(endValue) => (st, counter)
  }

  import Conditional._
  import SQLTable._
  import SQLColumn._
 
  val sub: Free[FreeQuery, Unit] = 
    for {
      _ <- select( List("XXXX","YYY"))
      _ <- from("sub-table" as "s")
      _ <- where(("neat" in List(1,2,3,4)) :: Nil)
    } yield ()

  val tmp: Free[FreeQuery, Unit] =
    for {
      t1 <- table("freeTable" as "f")
      column1 <- t1.column("free-column1")
      y <- select(List(column1, "column1", "column2"))
      _ <- from(t1)
      _ <- from("cool" as "c") 
      _ <- fromQ(sub)
      _ <- fromQ { 
        for {
          _ <- select(List("column1","column2"))
        } yield ()
      }
      _ <- where( ("levin" === "cool") :: Nil )
      _ <- done
    } yield ()

}

/**
 * SQL TABLE
 */
case class SQLTable(name: String, alias: Option[String]) {
  import FreeQuery._
  def column(columnName: String): Free[FreeQuery,SQLColumn] = 
    Suspend(Column(SQLColumn(columnName, Some(this)), t => Return(t)))
}
object SQLTable {
  implicit class SQLTableSyntax(tableName: String) {
    def as(alias: String): SQLTable = SQLTable(tableName, Some(alias))
  }
  implicit def string2Table(tableName: String): SQLTable = SQLTable(tableName, None)
}

object QueryMain {

  def main(args: Array[String]) {

    import FreeQuery._

    println("%s".format(sqlInterpreter(tmp,Nil)))

  }

}
