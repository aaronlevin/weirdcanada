package org.weirdcanada.site.model

// weirdcanada
import org.weirdcanada.dynamicform.{
  BasicField, 
  DynamicField, 
  DynamicFieldPrimitives,
  DynamicFormFieldRenderHelpers,
  HasEmpty, 
  HasFields,
  ManyRecordField,
  RecordField
}

// scalaz
import scalaz.Lens

case class VolunteerBio(
  bylineEnglish: String,
  bylineFrancais: String,
  website: String,
  descriptionEnglish: String,
  descriptionFrancais: String,
  image: String
)

object VolunteerBio { 

  val volunteerBioBylineEnglishLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(bylineEnglish = s), (vb) => vb.bylineEnglish)
  val volunteerBioBylineFrancaisLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(bylineFrancais = s), (vb) => vb.bylineFrancais)
  val volunteerBioWebsiteLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(website = s), (vb) => vb.website)
  val volunteerBioDescriptionEnglishLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(descriptionEnglish = s), (vb) => vb.descriptionEnglish)
  val volunteerBioDescriptionFrancaisLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(descriptionFrancais = s), (vb) => vb.descriptionFrancais)
  val volunteerBioImageLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(image = s), (vb) => vb.image)

  // generate text areas!
  import DynamicFormFieldRenderHelpers.textAreaRender

  private val descriptionEnglishArea = textAreaRender("name=volunteerbio-description-english-input")("English Bio") _
  private val descriptionFrancaisArea = textAreaRender("name=volunteerbio-description-francais-input")("Francais Bio") _

  implicit object VolunteerBioRecord extends HasFields[VolunteerBio] {
    val fields: List[DynamicField[VolunteerBio]] = List(
      BasicField[VolunteerBio]("volunteerbio-byline-english", volunteerBioBylineEnglishLens),
      BasicField[VolunteerBio]("volunteerbio-byline-francais", volunteerBioBylineFrancaisLens),
      BasicField[VolunteerBio]("volunteerbio-website", volunteerBioWebsiteLens),
      BasicField[VolunteerBio]("volunteerbio-description-english", volunteerBioDescriptionEnglishLens, Some(descriptionEnglishArea)),
      BasicField[VolunteerBio]("volunteerbio-description-francais", volunteerBioDescriptionFrancaisLens, Some(descriptionFrancaisArea)),
      BasicField[VolunteerBio]("volunteerbio-image", volunteerBioImageLens)
    )
  }

}
