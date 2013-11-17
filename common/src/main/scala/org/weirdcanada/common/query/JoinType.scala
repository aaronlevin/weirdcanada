package org.weirdcanada.common.query

sealed trait JoinType
object JoinTypeInstances {
  case object InnerJoin extends JoinType
  case object LeftJoin extends JoinType
  case object RightJoin extends JoinType
  case object OuterJoin extends JoinType
  case object CrossJoin extends JoinType
}

