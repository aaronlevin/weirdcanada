package org.weirdcanada.common.query

import scalaz.{Free, FreeInstances, Functor}
import Free._

sealed trait Query[+A] 

case class Select[A](columns: Iterable[String], next: A) extends Query[A]
case class From[A](tables: Iterable[String], next: A) extends Query[A]
case class FromQ[A,B](subQuery: Query[Free[B,A], next: A) extends Query[A]
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

  def fromQ[A,B](subQuery: Query[Free[B,A]]): Free[Query, Unit] = 
    Suspend(FromQ(subQuery, Return(())))
 
  def done: Free[Query, Unit] = 
    Return(Done)

  def sqlInterpreter[A](query: Free[Query, A], string: String): String = query match {
    case Suspend(Select(columns, a)) => sqlInterpreter(a, string + "select %s".format(columns.mkString(",")))
    case Suspend(From(tables, a)) => sqlInterpreter(a, string + "from %s".format(tables.mkString(",")))
    case Suspend(FromQ(subquery, a)) => 
      sqlInterpreter(a, string + "from ( %s )".format(sqlInterpreter(Suspend(subquery),"")))
    case Suspend(Done) => string
    case Return(a) => string
    case Gosub(a, f) => {
      val freeA = a()
      val value = sqlInterpreter(freeA, string)
      freeA match {
        case Suspend(Select(columns,_)) => sqlInterpreter(f(()), value)
        case Suspend(From(tables, _)) => sqlInterpreter(f(()), value)
        case Suspend(FromQ(_,_)) => sqlInterpreter(f(()), value)
        case Return(newA) => sqlInterpreter(f(newA), value)
        case _ => value
      }
    }
  }

  val tmp: Free[Query, Unit] =
    for {
      y <- select("column1" :: "column2" :: Nil)
      _ <- from("cool" :: Nil) 
      _ <- done
    } yield ()

}

object QueryMain {

  def main(args: Array[String]) {

    import Query._

    println("*** tmp: %s".format(sqlInterpreter(tmp,"")))

  }

}
