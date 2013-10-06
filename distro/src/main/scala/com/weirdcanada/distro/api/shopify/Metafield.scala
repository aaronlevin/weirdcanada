package com.weirdcanada.distro.api.shopify

import net.liftweb.json._
import org.joda.time.DateTime

object Metafield {
  implicit private val formats = DefaultFormats

  def unapply(jsonString: String): Option[Metafield] = {
    parseOpt(jsonString) match {
      case Some(JObject(JField("metafield", metafield) :: Nil)) => Some(Metafield(metafield))
      case _ => None // TODO: failed to parse
    }
  }
  
  def apply(key: String, value: String, namespace: String) =
    new Metafield(key, value, "string", namespace)
  
  def apply(jv: JValue): Metafield = {
     //TODO: trace? println(jv.toString)
    new PersistentMetafield(
      id = (jv \ "id").extract[Long],
      key = (jv \ "key").extract[String],
      createdAt = new DateTime((jv \ "created_at").extract[String]),
      value = (jv \ "value").extract[String],
      valueType = (jv \ "value_type").extract[String],
      namespace = (jv \ "namespace").extract[String]
    )
  }

  object List {
    def unapply(jsonString: String): Option[List[Metafield]] = {
      parseOpt(jsonString) match {
        case Some(JObject(JField("metafields", JArray(metafields)) :: Nil)) => Some(metafields.map(Metafield(_)))
        case _ => None // TODO: failed to parse
      }
    }
    
    def apply(metafields: Iterable[Metafield]) =
      JField("metafields",
        JArray(metafields.map(_.toJValue).toList)
      )
  }
}

class Metafield(
  val key: String,
  val value: String,
  val valueType: String,
  val namespace: String
) {
  def toJson = compactRender(toJValue).toString
  
  def toJValue: JValue =
    JObject(List(
      JField("namespace", JString(namespace)),
      JField("key", JString(key)),
      JField("value", (valueType match { case "string" => JString(value); case "integer" => JInt(value.toInt) })),
      JField("value_type", JString(valueType))
    ))
}

class PersistentMetafield(
  val id: Long,
  key: String,
  val createdAt: DateTime,
  value: String,
  valueType: String,
  namespace: String
)
extends Metafield(key, value, valueType, namespace) {
  // Could override toJson and toJValue... but we don't need to because we don't serialize these
}