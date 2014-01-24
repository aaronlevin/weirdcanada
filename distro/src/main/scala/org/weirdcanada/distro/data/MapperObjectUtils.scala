package org.weirdcanada.distro.data

import net.liftweb.common.{Box, Failure, Full}
import net.liftweb.mapper._
import org.weirdcanada.common.util.StringParsingUtil.safeParse

/**
 * Simple utilities for companion objects to `Mapper` classes
 */
trait MapperObjectUtils[A <: LongKeyedMapper[A]] { this: LongKeyedMetaMapper[A] => 

  /**
   * Find an `A` when the id is a string. Need to safely parse it first.
   *
   * @param id an id in the form of a string.
   * @returns a box contaning `A`
   */
  def findByStringId(id: String): Box[A] = safeParse[Long](id) match {
    case None => Failure("%s is not a valid Long".format(id))
    case Some(i) => this.findByKey(i)
  }
}
