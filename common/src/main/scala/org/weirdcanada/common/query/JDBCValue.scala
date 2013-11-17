package org.weirdcanada.common.query

import java.sql.{PreparedStatement, Types}

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

