package org.weirdcanada.site.model

// weirdcanada
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty, HasFields}

// scalaz
import scalaz.Lens

case class Author(name: String, url: String)
object Author {
  private val authorNameLens: Lens[Author, String] = Lens.lensu( (a, n) => a.copy(name = n), (a) => a.name )
  private val authorUrlLens: Lens[Author,String] = Lens.lensu( (a, u) => a.copy(url = u), (a) => a.url )

  implicit object AuthorRecord extends HasFields[Author] {
    val fields: List[DynamicField[Author]] = List(
      BasicField[Author]("author-name", authorNameLens)
    , BasicField[Author]("author-url", authorUrlLens)
    )
  }

  implicit object AuthorEmpty extends HasEmpty[Author] {
    val empty: Author = Author("","")
  }
}
