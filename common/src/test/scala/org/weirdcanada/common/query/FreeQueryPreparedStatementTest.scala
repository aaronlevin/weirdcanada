package org.weirdcanada.common.query

import scalaz.Free
import scalaz.State

import org.specs2._

import java.sql.{DriverManager, Connection, PreparedStatement}

class FreeQueryPreparedStatementTest extends Specification { def is = s2"""

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

  /**
   * Execute update statement via freeQuery1
   */
  FreeQuery.execute(connection, freeQuery1)

  val afterRs = 
    connection
      .prepareStatement("""select column1 from table1 where column2 = 'world'""")
      .executeQuery()

  afterRs.next()
  val after = afterRs.getInt(1)
  
  /**
   * Execute second query using `executeQuery` helper method
   */

  val secondQueryResult =
    FreeQuery.executeQuery[Int](connection, freeQuery2) { _.getInt(1) }.head

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
      _ <- where { column1 in List(200,300,400,100,600,700) }
    } yield ()


}
