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

  def executeQueryWithReducer[T](st: PreparedStatement)(reducer: Iterable[T] => Iterable[T])(f: ResultSet => T): Iterable[T] =
    reducer( executeQuery(st) { f } )

  /**
   * Small helpe rmethod to create clauses from collections of things (including options). It puts the ?,?,? in your queries.
   *
   * @param formatString a string that will be formatted to add ?,?,?... to your queries
   * @param items the collection of things
   * @return an option string where `None` implies the collection was empty
   */
  def createClauseString[A](formatString: String, items: Iterable[A]): Option[String] = 
    items.isEmpty match {
      case true => None
      case false => Some( formatString.format( items.map { _ => "?" }.mkString(",") ) )
    }

  /**
   * Take a list of option clause lins, flatten them, and then construct a final query. Good for forms that have many option fields
   *
   * @param clauses the list of clauses used to construct the final clause
   * @return a string representing a clause
   */
  def constructClause(prefix: Option[String], clauses: Option[String]*): String =
    clauses.flatten match {
      case Nil => ""
      case cs @ _ => prefix.getOrElse("") + " " + cs.mkString(" AND ")
    }

  /**
   * Helper method to fold over collections and set prepared statements for strings.
   *
   * Caution: side effects
   *
   * @param st the prepared statement to update
   * @param counter where in the prepared statement are we updating? what you pass in is what gets updated, so if you are starting a prepared statement, you should pass in 1. if
   * you are continuing after having made some additions, pass in the previous value + 1
   * @param listOfCollections the list of collections to fold over. They should be ordered the same as the sql was constructed
   * @return an integer representing how many things were updated (in case you need to further amend your prepared statement)
   */
  def setStatement(st: PreparedStatement, counter: Int, listOfCollections: List[Iterable[String]]): Int = {
    listOfCollections.foldLeft(counter){ (inc, collection) => 
        collection.foldLeft(inc){ (i, s) => st.setString(i,s); i + 1 }
    }
  }





}
