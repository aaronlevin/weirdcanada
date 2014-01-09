package org.weirdcanada.distro.util

object IdList {
  def unapply(commaJoinedIds: String): Option[Seq[Long]] = {
    Some(commaJoinedIds.split(',').map(_.toLong).toSeq)
  }
}
