package org.weirdcanada.common.query

import scalaz.{Free, FreeInstances, Functor}
import Free._

sealed trait Query[+A] 

case class Select[A](columns: Iterable[String], next: A) extends Query[A]
case class From[A](tables: Iterable[String], next: A) extends Query[A]
case class FromQ[A](subQuery: Free[Query,Unit], next: A) extends Query[A]
case object Done extends Query[Nothing]

object Query {

  implicit object queryFunctor extends Functor[Query] {

    def map[A, B](fa: Query[A])(f: A => B): Query[B] = fa match {
      case Select(columns, next) => Select(columns, f(next))
      case From(tables, next) => From(tables, f(next) )
      case FromQ(subQuery, next) => FromQ(subQuery, f(next))
      case Done => Done
    }
  }

  def select(columns: Iterable[String]): Free[Query, Unit] =
    Suspend(Select(columns, Return(())))

  def from(tables: Iterable[String]): Free[Query, Unit] =
    Suspend(From(tables, Return(())))

  def fromQ(subQuery: Free[Query,Unit]): Free[Query, Unit] = 
    Suspend(FromQ(subQuery, Return(())))
 
  def done: Free[Query, Unit] = 
    Return(Done)

  def sqlInterpreter[A](query: Free[Query, A], statements: List[String]): String = query match {
    case Suspend(Select(columns, a)) => sqlInterpreter(a, statements :::  "select %s".format(columns.mkString(",")) :: Nil)
    case Suspend(From(tables, a)) => sqlInterpreter(a, statements ::: "from %s".format(tables.mkString(",")) :: Nil)
    case Suspend(FromQ(subquery, a)) => sqlInterpreter(a, statements ::: "from ( %s )".format(sqlInterpreter(subquery,Nil)) :: Nil)
    case Suspend(Done) => statements.mkString("\n")
    case Return(a) => statements.mkString("\n")
    case Gosub(a, f) => {
      val freeA = a()
      val value = sqlInterpreter(freeA, statements)
      sqlInterpreter(f(()), List(value))
      /*freeA match {
        case Suspend(Select(columns,_)) => sqlInterpreter(f(()), value)
        case Suspend(From(tables, _)) => sqlInterpreter(f(()), value)
        case Suspend(FromQ(_,_)) => sqlInterpreter(f(()), value)
        case Return(newA) => sqlInterpreter(f(newA), value)
        case _ => value
      }*/
    }
  }
 
  val sub: Free[Query, Unit] = 
    for {
      _ <- select("XXXX" :: "YYY" :: Nil)
    } yield ()

  val tmp: Free[Query, Unit] =
    for {
      y <- select("column1" :: "column2" :: Nil)
      _ <- from("cool" :: Nil) 
      _ <- fromQ(sub)
      _ <- done
    } yield ()

}

object QueryMain {

  def main(args: Array[String]) {

    import Query._

    println("*** tmp: %s".format(sqlInterpreter(tmp,Nil)))

  }

}
