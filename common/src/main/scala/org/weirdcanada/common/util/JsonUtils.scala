package org.weirdcanada.common.util

import argonaut._
import Argonaut._
import java.net.{MalformedURLException, URL}

object JsonUtils {

  implicit val JavaNetUrlEncodeJson: EncodeJson[URL] =
    EncodeJson(u => jString(u.toString))

  implicit val JavaNetUrlDecodeJson: DecodeJson[URL] = 
    optionDecoder( (j: argonaut.Json) => j.string.flatMap { s => try {
        Some(new URL(s))
      } catch {
        case _:MalformedURLException => None
      }},
      "java.net.URL"
    )
}
