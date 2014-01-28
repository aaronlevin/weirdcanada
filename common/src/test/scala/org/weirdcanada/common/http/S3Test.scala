package org.weirdcanada.common.http

import org.specs2._

class S3Test extends Spcification { def is = s2"""
  This is a specification to ensure S3 signs requests properly

  The S3 signed request method should
    produce a signature that matches amazon's example $e1
  """




class FreeQueryPreparedStatementTest extends Specification { def is = s2"""

  This is a specification to ensure that prepared statements are updated properly

  The simple FreeQuery query should
    ensure the column2 is equal to 100 before updating $e1
    ensure after the freeQuery updated, column2 is equal to 5 $e2
    handle `column1 in (?,?,?)` type queries $e3
    handle joins $e4
  """



  /*
    def e1 = before mustEqual 6
  def e2 = after mustEqual 5
  def e3 = secondQueryResult mustEqual 100
  def e4 = thirdQueryResult mustEqual List(("hello", "olleh"), ("world", "dlrow"))*/
}
