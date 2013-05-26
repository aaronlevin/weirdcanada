package org.weirdcanada.site.model

// weirdcanada
import org.weirdcanada.dynamicform.{BasicField, DynamicField, HasEmpty, HasFields, ManyRecordField, RecordField}

// scalaz
import scalaz.Lens

case class Post(
  release: Release,
  authors: List[Author],
  translators: List[Translator],
  translatorText: String,
  fromThe: String,
  contentEnglish: String,
  deLa: String,
  contentFrench: String
)

object Post {
  private val postReleaseLens: Lens[Post,Release] = Lens.lensu( (p,r) => p.copy(release = r), (p) => p.release)
  private val postAuthorsLens: Lens[Post, Map[Int, Author]] = Lens.lensu(
    set = (p: Post, am: Map[Int,Author]) => p.copy(authors = am.toList.sortBy { _._1 }.map { _._2})
  , get = (p: Post) => p.authors.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  private val postTranslatorsLens: Lens[Post, Map[Int, Translator]] = Lens.lensu(
    set = (p: Post, tm: Map[Int,Translator]) => p.copy(translators = tm.toList.sortBy { _._1 }.map { _._2})
  , get = (p: Post) => p.translators.zipWithIndex.map { x => (x._2, x._1)}.toMap
  )
  private val postFromTheLens: Lens[Post, String] = Lens.lensu( (p,ft) => p.copy(fromThe = ft), (p) => p.fromThe)
  private val postContentEnglishLens: Lens[Post, String] = Lens.lensu( (p,ce) => p.copy(contentEnglish = ce), (p) => p.contentEnglish)
  private val postDeLaLens: Lens[Post, String] = Lens.lensu( (p,dl) => p.copy(deLa = dl), (p) => p.deLa)
  private val postContentFrenchLens: Lens[Post, String] = Lens.lensu( (p,cf) => p.copy(contentFrench = cf), (p) => p.contentFrench)

  implicit object PostRecord extends HasFields[Post] {
    val fields: List[DynamicField[Post]] = List(
      RecordField[Post, Release]("release", postReleaseLens)
    , ManyRecordField[Post, Author]("author", postAuthorsLens)
    , ManyRecordField[Post, Translator]("translator", postTranslatorsLens)
    , BasicField[Post]("from-the", postFromTheLens)
    , BasicField[Post]("content-english", postContentEnglishLens)
    , BasicField[Post]("de-la", postDeLaLens)
    , BasicField[Post]("content-french", postContentFrenchLens)
    )
  }
}

