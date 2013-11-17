package org.weirdcanada.common.query

import JoinTypeInstances._

import scalaz.{Free, FreeInstances, Functor, \/, -\/, \/-, State, syntax}
import scalaz.std.list._
import scalaz.syntax.traverse._
import Free._
import State.{get, init, modify, put, state}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import language.implicitConversions
import java.sql.{Connection, PreparedStatement, ResultSet, Types}

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
 * The Free Query Free Monad
 */
sealed trait FreeQuery[+A] 

case class Select[A](columns: List[SQLColumn], next: A) extends FreeQuery[A]
case class From[A](table: SQLTable, next: A) extends FreeQuery[A] 
case class FromQ[A](subQuery: Free[FreeQuery,Unit], withBrackets: Boolean, next: A) extends FreeQuery[A]
case class Update[A](table: SQLTable, next: A) extends FreeQuery[A]
case class Set[A](conditionals: List[Free[FreeQuery, Unit]], next: A) extends FreeQuery[A]
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
  def set: State[(PreparedStatement, Int), Unit] = modify{ s => jdbc.set(s._1, s._2, value);  (s._1, s._2 +1) }
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

  import SQLColumn._
  import SQLTable._

  implicit object queryFunctor extends Functor[FreeQuery] {

    def map[A, B](fa: FreeQuery[A])(f: A => B): FreeQuery[B] = fa match {
      case Select(columns, next) => Select(columns, f(next))
      case From(tables, next) => From(tables, f(next) )
      case FromQ(subQuery, b, next) => FromQ(subQuery, b, f(next))
      case Update(subTable, next) => Update(subTable, f(next))
      case Set(conditionals, next) => Set(conditionals, f(next))
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

  def select(columns: SQLColumn*): Free[FreeQuery, Unit] =
    Suspend(Select(columns.toList, Return(())))

  def from(table: SQLTable): Free[FreeQuery, Unit] =
    Suspend(From(table, Return(())))

  def from(subQuery: Free[FreeQuery, Unit]): Free[FreeQuery, Unit] =
    Suspend(FromQ(subQuery, false, Return(())))

  def fromQ(subQuery: Free[FreeQuery,Unit]): Free[FreeQuery, Unit] = 
    Suspend(FromQ(subQuery, true, Return(())))

  def update(table: SQLTable): Free[FreeQuery, Unit] =
    Suspend(Update(table, Return(())))
 
  def done: Free[FreeQuery, Unit] = 
    Return(Done)

  def where[A](logic: Free[FreeQuery, A]): Free[FreeQuery, Unit] = 
    Suspend(Where(logic, Return(())))

  def table(table: SQLTable): Free[FreeQuery, SQLTable] = 
    Suspend(Table(table, t => Return(t)))

  def set(queries: Free[FreeQuery, Unit]*): Free[FreeQuery, Unit] = 
    Suspend(Set(queries.toList, Return(())))

  def and[A,B](cond1: Free[FreeQuery, A], cond2: Free[FreeQuery, B]): Free[FreeQuery, Unit] = 
    Suspend(And(cond1, cond2, Return(())))

  def innerJoin(table1: SQLTable, table2: SQLTable): Free[FreeQuery, Unit] =
    Suspend(JoinedTables(InnerJoin, table1, table2, None, Return(())))

  import SQLTable._
  import SQLColumn._

  implicit class FreeQuerySyntax(query: Free[FreeQuery, Unit]) {
   def and(query2: Free[FreeQuery, Unit]): Free[FreeQuery, Unit] =
    Suspend(And(query, query2, Return()))
  }
 
  implicit class ConditionalStringSyntax(string: String) {
    def ===[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend(Equals(string, a, Return(())))
    def =!=[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend(NotEqual(string, a, Return(())))
    def <=[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend(LessThanOrEqual(string, a, Return(())))
    def <[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend(LessThan(string, a, Return(())))
    def in[A : JDBCValue](as: Iterable[A]): Free[FreeQuery, Unit] = 
      Suspend(In(string, as, Return(())))
  }

  implicit class ConditionalColumnSyntax(column: SQLColumn) {
    def ===[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend(Equals(column, a, Return(())))
    def =!=[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend(NotEqual(column, a, Return(())))
    def <=[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend(LessThanOrEqual(column, a, Return(())))
    def <[A : JDBCValue](a: A): Free[FreeQuery, Unit] = 
      Suspend(LessThan(column, a, Return(())))
    def in[A : JDBCValue](as: Iterable[A]): Free[FreeQuery, Unit] = 
      Suspend(In(column, as, Return(())))
  }

  def sqlInterpreter[A](query: Free[FreeQuery,A], statements: List[String]): String = query.resume match {
    case -\/(freeValue) => freeValue match {
      case Select(columns, a) => 
        val columnString: List[String] = columns.map { _.render }
        sqlInterpreter(a, statements :::  "select %s".format(columnString.mkString(",")) :: Nil)
      case From(table, a) => 
        val sqlString: String = "FROM %s".format(table.render)
        sqlInterpreter(a, statements ::: sqlString :: Nil)
      case FromQ(subquery, false, a) => sqlInterpreter(a, statements ::: "from %s ".format(sqlInterpreter(subquery,Nil)) :: Nil)
      case FromQ(subquery, true, a) => sqlInterpreter(a, statements ::: "from ( %s )".format(sqlInterpreter(subquery,Nil)) :: Nil)
      case Update(table, a) => 
        val sqlString: String = "UPDATE %s".format(table.render)
        sqlInterpreter(a, statements ::: sqlString :: Nil)
      case Set(conditionals, a) => 
        val sqlString: String = "set %s".format(conditionals.map { sqlInterpreter(_,Nil) }.mkString(" , "))
        sqlInterpreter(a, statements ::: sqlString :: Nil)
      case Where(logicQuery, a) => 
        val whereStatement: String = "WHERE  %s ".format(sqlInterpreter(logicQuery,Nil))
        sqlInterpreter(a, statements ::: whereStatement :: Nil)
      case Equals(column, _, a) => sqlInterpreter(a, statements ::: "%s = ?".format(column.render) :: Nil)
      case NotEqual(column, _, a) => sqlInterpreter(a, statements ::: "%s <> ?".format(column.render) :: Nil)
      case LessThanOrEqual(column, _, a) => sqlInterpreter(a, statements ::: "%s LessThanOrEqual ?".format(column.render) :: Nil)
      case LessThan(column, _, a) => sqlInterpreter(a, statements ::: "%s < ?".format(column.render) :: Nil)
      case In(column, values, a) => 
        val inString: String = "%s IN (%s)".format( column.render, values.map {_ => "?" }.mkString(",") )
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
      case FromQ(subQuery, _, a) => 
        val newState: State[PState, Unit] = sqlPrepared(subQuery, state)
        sqlPrepared(a, newState)
      case Where(logic, a) => 
        val newState: State[PState, Unit] = sqlPrepared(logic, state)
        sqlPrepared(a, newState)
      case Update(_, a) => sqlPrepared(a, state)
      case Set(conditionals, a) => 
        val stateAction = 
          conditionals
            .toList
            .foldLeft(state) { (acc, cond) => sqlPrepared(cond, acc) }
        sqlPrepared(a, stateAction)
      case eq @ Equals(_, _, a) => sqlPrepared(a, state.flatMap { _ => eq.set })
      case neq @ NotEqual(_, value, a) => sqlPrepared(a, state.flatMap { _ => neq.set })
      case lte @ LessThanOrEqual(_, value, a) => sqlPrepared(a, state.flatMap { _ => lte.set })
      case lt @ LessThan(_, value, a) => sqlPrepared(a, state.flatMap { _ => lt.set })
      case in @ In(_, values, a) => sqlPrepared(a, state.flatMap { _ => in.set })
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

  def executeQuery[A](c: Connection, query: Free[FreeQuery, Unit])(f: ResultSet => A): List[A] = {
    val ps = c.prepareStatement(sqlInterpreter(query, Nil))
    val init: (PreparedStatement, Int) = (ps, 1)
    val initState = for { _ <- State.init[(PreparedStatement, Int)] } yield ()

    val resultSet = sqlPrepared(query, initState).run(init)._1._1.executeQuery()

    val resultBuffer = new ListBuffer[A]

    while(resultSet.next)
      resultBuffer += f(resultSet)

    resultBuffer.toList
  }

  def execute(c: Connection, query: Free[FreeQuery, Unit]): Boolean = {
    val ps = c.prepareStatement(sqlInterpreter(query, Nil))
    val init: (PreparedStatement, Int) = (ps, 1)
    val initState = for { _ <- State.init[(PreparedStatement, Int)] } yield ()

    sqlPrepared(query, initState).run(init)._1._1.execute()
  }

}
