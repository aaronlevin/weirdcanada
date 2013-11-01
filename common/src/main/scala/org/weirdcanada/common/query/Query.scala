package org.weirdcanada.common.query

import scalaz.{Free, FreeInstances, Functor, \/, -\/, \/-, State, syntax}
import scalaz.std.list._
import scalaz.syntax.traverse._
import Free._
import State.{get, init, modify, put, state}

import language.implicitConversions
import java.sql.{PreparedStatement, Types}

/**
 * Goals: 
 *
 * 1. to create composable queries in a monadic form
 * 2. to create PreparedStatements that know how to iterate over containers
 * 3. to create PreparedStatements that know how to handle Options
 * 4. To keep a flexible syntax that allows for type safety whenever, 
 *    or just cow-boy wild west strings.
 * 5. To not need an ORM, but be able to easily fix and debug queries.
 * 6. To hopefully use Shapeless to handle airity constraints.
 * 7. To learn about Free Monads
 * 8. To see the performance impact of crazy Free Monad stuff in scalaz
 *
 * Ultimatley, to produce queries like this:
 *
 *  for {
 *    table1 <- table("table1-name" as "t1") // supply an alias
 *    table2 <- table("table2-name") // do not supply an alias
 *    column1 <- table1.column("column2")  // 'safe way' (will use alias)
 *    column2 <- column("a column") // not tied to table (sql will assume its form table2-name)
 *    _ <- select(column1, column2, "column3") // flexibility with adding columns
 *    _ <- from(table1).innerJoin(table2 on column2 == "randomColumn")
 *    _ <- fromQ { // for fun, subQuery support!
 *      for {
 *        _ <- select("*")
 *        _ <- from("random-table") // can use proper table or just a string
 *      } yield ()
 *    }
 *    _ <- where{
 *      for {
 *        _ <- (table1.column("third-column") === "three") and (column2 === 1)
 *        _ <- or
 *        _ <- column1 NotEqual "levin"
 *      } yield ()
 *    }
 *  } yield ()
 */

/**
 * A TypeClass for types that can be set in `PreparedStatement`s
 */
sealed trait JDBCValue[A] {
  def set(st: PreparedStatement, i: Int, a: A): Unit
  val sqlType: Int
}
/**
 * instances
 */
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
    val jdbcVal: JDBCValue[A] = implicitly[JDBCValue[A]]
    val sqlType: Int = jdbcVal.sqlType
    def set(st: PreparedStatement, i: Int, as: Iterable[A]): Unit = 
      jdbcVal.set(st, i, as.head)
  }

}

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

sealed trait JoinType
case object InnerJoin extends JoinType
case object LeftJoin extends JoinType
case object RightJoin extends JoinType
case object OuterJoin extends JoinType
case object CrossJoin extends JoinType

/**
 * The Free Query Free Monad
 */
sealed trait FreeQuery[+A] 

case class Select[A](columns: List[SQLColumn], next: A) extends FreeQuery[A]
case class From[A](table: SQLTable, next: A) extends FreeQuery[A] 
case class FromQ[A](subQuery: Free[FreeQuery,Unit], next: A) extends FreeQuery[A]
case class Where[A, B](logicQuery: Free[FreeQuery,B], next: A) extends FreeQuery[A]
case class Table[A](table: SQLTable, func: SQLTable => A) extends FreeQuery[A]
case class JoinedTables[A](joinType: JoinType, table1: SQLTable, table2: SQLTable, joinCondition: Option[(SQLColumn, SQLColumn)], next: A) extends FreeQuery[A]
case class Column[A](column: SQLColumn, func: SQLColumn => A) extends FreeQuery[A]
case class And[A,B,C](cond1: Free[FreeQuery, B], cond2: Free[FreeQuery, C], next: A) extends FreeQuery[A]
case class Or[A,B,C](cond1: Free[FreeQuery, B], cond2: Free[FreeQuery, C], next: A) extends FreeQuery[A]
case class Equals[A, B : JDBCValue](column: SQLColumn, value: B, next: A) extends FreeQuery[A] {
  lazy val jdbc: JDBCValue[B] = implicitly[JDBCValue[B]]
  // Stupid type erasure
  def mapF[C](f: A => C): Equals[C, B] = Equals(this.column, this.value, f(next))
  def set: State[(PreparedStatement, Int), Unit] = modify{ s => jdbc.set(s._1, s._2, value); (s._1, s._2 +1) }
}
case class NotEqual[A,B : JDBCValue](column: SQLColumn, value: B, next: A) extends FreeQuery[A] {
  lazy val jdbc: JDBCValue[B] = implicitly[JDBCValue[B]]
  def mapF[C](f: A => C): NotEqual[C, B] = NotEqual(this.column, this.value, f(next))
  def set: State[(PreparedStatement, Int), Unit] = modify{ s => jdbc.set(s._1, s._2, value); (s._1, s._2 +1) }
}
case class LessThanOrEqual[A, B : JDBCValue](column: SQLColumn, value: B, next: A) extends FreeQuery[A] {
  lazy val jdbc: JDBCValue[B] = implicitly[JDBCValue[B]]
  def mapF[C](f: A => C): LessThanOrEqual[C, B] = LessThanOrEqual(this.column, this.value, f(next))
  def set: State[(PreparedStatement, Int), Unit] = modify{ s => jdbc.set(s._1, s._2, value); (s._1, s._2 +1) }
}
case class LessThan[A, B : JDBCValue](column: SQLColumn, value: B, next: A) extends FreeQuery[A] {
  lazy val jdbc: JDBCValue[B] = implicitly[JDBCValue[B]]
  def mapF[C](f: A => C): LessThan[C, B] = LessThan(this.column, this.value, f(next))
  def set: State[(PreparedStatement, Int), Unit] = modify{ s => jdbc.set(s._1, s._2, value); (s._1, s._2 +1) }
}
case class In[A, B : JDBCValue](column: SQLColumn, values: Iterable[B], next: A) extends FreeQuery[A] {
  lazy val jdbc: JDBCValue[B] = implicitly[JDBCValue[B]]
  def mapF[C](f: A => C): In[C, B] = In(this.column, this.values, f(next))
  def set: State[(PreparedStatement, Int), Unit] = 
    for {
      _ <- values
            .toList
            .traverseS { v => 
              modify{ (s: (PreparedStatement, Int)) => jdbc.set(s._1, s._2, v); (s._1, s._2 +1) }
            }
    } yield ()
}
case object Done extends FreeQuery[Nothing]

object FreeQuery {

  implicit object queryFunctor extends Functor[FreeQuery] {

    def map[A, B](fa: FreeQuery[A])(f: A => B): FreeQuery[B] = fa match {
      case Select(columns, next) => Select(columns, f(next))
      case From(tables, next) => From(tables, f(next) )
      case FromQ(subQuery, next) => FromQ(subQuery, f(next))
      case Where(logic, next) => Where(logic, f(next))
      case eq @ Equals(_, _, _) => eq.mapF(f) 
      case neq @ NotEqual(_, _, _) => neq.mapF(f) 
      case lte @ LessThanOrEqual(_, _, _) => lte.mapF(f) 
      case lt @ LessThan(_, _, _) => lt.mapF(f) 
      case inQ @ In(_,_,_) => inQ.mapF(f)
      case Table(table, g) => Table(table, f compose g) 
      case JoinedTables(j, l, r, cond, next) => JoinedTables(j, l, r, cond, f(next))
      case Column(column, g) => Column(column, f compose g)
      case And(cond1, cond2, next) => And(cond1, cond2, f(next))
      case Or(cond1, cond2, next) => Or(cond1, cond2, f(next))
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

  def where[A](logic: Free[FreeQuery, A]): Free[FreeQuery, Unit] = 
    Suspend(Where(logic, Return(())))

  def table(table: SQLTable): Free[FreeQuery, SQLTable] = 
    Suspend(Table(table, t => Return(t)))

  def and[A,B](cond1: Free[FreeQuery, A], cond2: Free[FreeQuery, B]): Free[FreeQuery, Unit] = 
    Suspend(And(cond1, cond2, Return(())))

  def innerJoin(table1: SQLTable, table2: SQLTable): Free[FreeQuery, Unit] =
    Suspend(JoinedTables(InnerJoin, table1, table2, None, Return(())))

  import SQLTable._
  import SQLColumn._
 
  implicit class ConditionalSyntax(string: String) {
    def ===[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend( Equals(string, a, Return(())))
    def =!=[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend( NotEqual(string, a, Return(())))
    def <=[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend( LessThanOrEqual(string, a, Return(())))
    def <[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend( LessThan(string, a, Return(())))
    def in[A : JDBCValue](as: Iterable[A]): Free[FreeQuery, Unit] = 
      Suspend( In(string, as, Return(())))
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
      case Where(logicQuery, a) => 
        val whereStatement: String = "WHERE ( %s )".format(sqlInterpreter(logicQuery,Nil))
        sqlInterpreter(a, statements ::: whereStatement :: Nil)
      case Equals(column, _, a) => sqlInterpreter(a, statements ::: "%s = ?".format(column.name) :: Nil)
      case NotEqual(column, _, a) => sqlInterpreter(a, statements ::: "%s <> ?".format(column.name) :: Nil)
      case LessThanOrEqual(column, _, a) => sqlInterpreter(a, statements ::: "%s LessThanOrEqual ?".format(column.name) :: Nil)
      case LessThan(column, _, a) => sqlInterpreter(a, statements ::: "%s < ?".format(column.name) :: Nil)
      case In(column, values, a) => 
        val inString: String = "%s IN (%s)".format( column.name, values.map {_ => "?" }.mkString(",") )
        sqlInterpreter(a, statements ::: inString :: Nil)
      case And(cond1, cond2, a) => 
        val statement1: String = sqlInterpreter(cond1, Nil)
        val statement2: String = sqlInterpreter(cond2, Nil)
        val andStatement: String = "(%s) AND (%s)".format(statement1, statement2)
        sqlInterpreter(a, statements ::: andStatement :: Nil )
      case Or(cond1, cond2, a) => 
        val statement1: String = sqlInterpreter(cond1, Nil)
        val statement2: String = sqlInterpreter(cond2, Nil)
        val orStatement: String = "(%s) OR (%s)".format(statement1, statement2)
        sqlInterpreter(a, statements ::: orStatement :: Nil )
      case Table(table, tableFunc) => sqlInterpreter(tableFunc(table), statements)
      case JoinedTables(joinType, table1, table2, joinCondition, a) => 
        val joinTypeString = joinType match { 
          case InnerJoin => " INNER JOIN "
          case LeftJoin => " LEFT JOIN "
          case RightJoin => " RIGHT JOIN "
          case OuterJoin => " OUTER JOIN "
          case CrossJoin => " CROSS JOIN "
        }
        val onCondition = joinCondition.map { case (col1, col2) => " ON (%s = %s)".format(col1.render, col2.render) }.getOrElse {""}
        sqlInterpreter(a, statements ::: (table1.render + joinTypeString + table2.render + onCondition) :: Nil)
      case Column(column, columnFunc) => sqlInterpreter(columnFunc(column), statements)
      case Done => statements.mkString("\n")
    }
    case \/-(endValue) => statements.mkString("\n")
  }

  type PState = (PreparedStatement, Int)

  def sqlPrepared[A](query: Free[FreeQuery, A], state: State[PState, Unit]): State[PState, Unit] = query.resume match {
    case -\/(freeValue) => freeValue match {
      case Select(columns, a) => sqlPrepared(a, state)
      case From(table, a) => sqlPrepared(a, state)
      case FromQ(subQuery, a) => 
        val newState: State[PState, Unit] = sqlPrepared(subQuery, state)
        sqlPrepared(a, newState)
      case Where(logic, a) => 
        val newState: State[PState, Unit] = sqlPrepared(logic, state)
        sqlPrepared(a, newState)
      case eq @ Equals(_, value, a) => 
        val newState: State[PState, Unit] = for { _ <- state; _ <- eq.set } yield ()
        sqlPrepared(a, newState)
      case neq @ NotEqual(_, value, a) => 
        val newState: State[PState, Unit] = for { _ <- state; _ <- neq.set } yield ()
        sqlPrepared(a, newState)
      case lte @ LessThanOrEqual(_, value, a) => 
        val newState: State[PState, Unit] = for { _ <- state; _ <- lte.set } yield ()
        sqlPrepared(a, newState)
      case lt @ LessThan(_, value, a) => 
        val newState: State[PState, Unit] = for { _ <- state; _ <- lt.set } yield ()
        sqlPrepared(a, newState)
      case in @ In(_, values, a) => 
        val newState: State[PState, Unit] = for { _ <- state; _ <- in.set } yield ()
        sqlPrepared(a, newState)
      case And(cond1, cond2, a) => 
        val newState: State[PState, Unit] = for { _ <- sqlPrepared(cond1, state); _ <- sqlPrepared(cond2, state) } yield ()
        sqlPrepared(a, newState)
      case Or(cond1, cond2, a) => 
        val newState: State[PState, Unit] = for { _ <- sqlPrepared(cond1, state); _ <- sqlPrepared(cond2, state) } yield ()
        sqlPrepared(a, newState)
      case Table(table, tableFunc) => sqlPrepared(tableFunc(table), state)
      case JoinedTables(_, _, _, _, a) => sqlPrepared(a, state)
      case Column(column, columnFunc) => sqlPrepared(columnFunc(column), state)
      case Done => state
    }
    case \/-(endValue) => state
  }


  val sub: Free[FreeQuery, Unit] = 
    for {
      _ <- select( List("XXXX","YYY"))
      _ <- from("sub-table" as "s")
      _ <- where( "neat" in List(1,2,3,4) )
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
      _ <- fromQ { t1 innerJoin "table2" |*| column1 === "neat" }
      _ <- where( "levin" === "cool" )
      _ <- done
    } yield ()

}

 //    _ <- from(table1).innerJoin(table2 on column2 == "randomColumn")

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

object QueryMain {

  def main(args: Array[String]) {

    import FreeQuery._

    println("%s".format(sqlInterpreter(tmp,Nil)))

  }

}
