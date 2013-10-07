package org.weirdcanada.distro.data

import net.liftweb.mapper.BaseMapper
import net.liftweb.mapper.MappedString


/**
 * A trait you can mix into a Mapper class that gives you
 * city, province and country columns
 */
trait Geography {
  self: BaseMapper =>

  lazy val city: MappedString[MapperType] = new MyStringField(this, 32, true)
  lazy val province: MappedString[MapperType] = new MyStringField(this, 32, true)
  lazy val country: MappedString[MapperType] = new MyStringField(this, 32, false)

  protected class MyStringField(obj: self.type, length: Int, indexed: Boolean) extends MappedString[MapperType](obj.asInstanceOf[MapperType], length) {
    override def dbIndexed_? = indexed
  }
}

