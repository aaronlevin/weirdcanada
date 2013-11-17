package org.weirdcanada.common.query

import scalaz.Free
import scalaz.State

import org.specs2._
import org.specs2.mock.Mockito

import java.sql.{DriverManager, Connection, PreparedStatement}

class FreeQueryPreparedStatementTest extends Specification with Mockito { def is = s2"""

  This is a specification to ensure that prepared statements are updated properly

  The simple FreeQuery query should
    ensure the column2 is equal to 100 before updating $e1
    ensure after the freeQuery updated, column2 is equal to 5 $e2
    handle `column1 in (?,?,?)` type queries $e3
  """

  import FreeQueryPreparedStatementTest._
  import FreeQueryStringTest.cleanse
  import FreeQuery._

  val clazz = Class.forName("org.h2.Driver")
  val connection = DriverManager.getConnection("jdbc:h2:test.db;AUTO_SERVER=TRUE", "", "")

  connection
    .prepareStatement("""drop table table1""")
    .execute()

  connection
    .prepareStatement("""create table table1 (id int primary key, column1 int, column2 text)""")
    .execute()

  connection
    .prepareStatement("""insert into table1 values (0, 100, 'hello')""")
    .execute()
  connection
    .prepareStatement("""insert into table1 values (1, 6, 'world')""")
    .execute()

  val beforeRs = 
    connection
      .prepareStatement("""select column1 from table1 where column2 = 'world'""")
      .executeQuery()

  beforeRs.next()
  val before = beforeRs.getInt(1)

  val ps1 = connection.prepareStatement(sqlInterpreter(freeQuery1, Nil))
  val init: (PreparedStatement, Int) = (ps1, 1)
  val initState = for { _ <- State.init[(PreparedStatement, Int)] } yield ()

  sqlPrepared(freeQuery1, initState).run(init)._1._1.execute()

  val afterRs = 
    connection
      .prepareStatement("""select column1 from table1 where column2 = 'world'""")
      .executeQuery()

  afterRs.next()
  val after = afterRs.getInt(1)
  
  /**
   * Execute second query
   */
  val ps2 = connection.prepareStatement(sqlInterpreter(freeQuery2, Nil))
  val init2: (PreparedStatement, Int) = (ps2, 1)

  val secondQueryRs = sqlPrepared(freeQuery2, initState).run(init2)._1._1.executeQuery()

  secondQueryRs.next()
  val secondQueryResult = secondQueryRs.getInt(1)

  def e1 = before mustEqual 6
  def e2 = after mustEqual 5
  def e3 = secondQueryResult mustEqual 100

}

object FreeQueryPreparedStatementTest {

  import FreeQuery._
  import SQLTable._


  val freeQuery1: Free[FreeQuery, Unit] = 
    for { 
      t1 <- table("table1" as "t1")
      column1 <- t1.column("column1")
      column2 <- t1.column("column2")
      _ <- update(t1)
      _ <- set(column1 === 5)
      _ <- where { column2 =!= "hello" }
    } yield ()

  val freeQuery2: Free[FreeQuery, Unit] = 
    for { 
      t1 <- table("table1" as "t1")
      column1 <- t1.column("column1")
      _ <- select(column1)
      _ <- from(t1)
      _ <- where { column1 in List(100,200,300,400,500,600,700) }
    } yield ()


}
