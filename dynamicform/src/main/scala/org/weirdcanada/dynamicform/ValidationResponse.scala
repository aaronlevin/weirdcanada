package org.weirdcanada.dynamicform

import net.liftweb.http.js.JsCmd

/*
 * Case class representing a single validation
 */
case class ValidationResponse(validity: Boolean, response: JsCmd)
