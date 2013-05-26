package org.weirdcanada.site.model

// scala
import scala.xml.{Text, NodeSeq}

// weirdcanada
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty, HasFields}

// scalaz
import scalaz.Lens

case class Translator(name: String, url: String)
object Translator {
  private val translatorNameLens: Lens[Translator, String] = Lens.lensu( (t, n) => t.copy(name = n), (t) => t.name )
  private val translatorUrlLens: Lens[Translator, String] = Lens.lensu( (t, u) => t.copy(url = u), (t) => t.url )

  implicit object TranslatorRecord extends HasFields[Translator] {
    val fields: List[DynamicField[Translator]] = List(
      BasicField[Translator]("translator-name", translatorNameLens)
    , BasicField[Translator]("translator-url", translatorUrlLens)
    )
  }

  implicit object TranslatorEmpty extends HasEmpty[Translator] {
    val empty: Translator = Translator("","")
  }

  def renderAsXml(translator: Translator): NodeSeq = 
    if(translator.url.isEmpty) Text(translator.name) else <a href={translator.url} target="_blank">{translator.name}</a>
}

