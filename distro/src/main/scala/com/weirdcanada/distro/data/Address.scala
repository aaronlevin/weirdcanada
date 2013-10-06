package com.weirdcanada.distro.data

import net.liftweb.mapper.BaseMapper
import net.liftweb.mapper.MappedString


trait Address {
  self: BaseMapper =>

  lazy val addressLine1 = new MyStringField(this, 256, false)
  lazy val addressLine2 = new MyStringField(this, 256, false)
  lazy val city: MappedString[MapperType] = new MyStringField(this, 32, true)
  lazy val province: MappedString[MapperType] = new MyStringField(this, 32, true)
  lazy val postalCode = new MyStringField(this, 16, false)
  lazy val country: MappedString[MapperType] = new MyStringField(this, 32, false)

  protected class MyStringField(obj: self.type, length: Int, indexed: Boolean) extends MappedString[MapperType](obj.asInstanceOf[MapperType], length) {
    override def dbIndexed_? = indexed
  }
}

