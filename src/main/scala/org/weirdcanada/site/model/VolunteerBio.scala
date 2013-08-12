package org.weirdcanada.site.model

// weirdcanada
import org.weirdcanada.dynamicform.{
  BasicField, 
  DynamicField, 
  DynamicFieldPrimitives,
  HasEmpty, 
  HasFields,
  ManyRecordField,
  RecordField
}

// scalaz
import scalaz.Lens

case class VolunteerBio(
  byline: String,
  website: String,
  description: String,
  image: String
)

object VolunteerBio { 

  val volunteerBioBylineLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(byline = s), (vb) => vb.byline)
  val volunteerBioWebsiteLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(website = s), (vb) => vb.website)
  val volunteerBioDescriptionLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(description = s), (vb) => vb.description)
  val volunteerBioImageLens: Lens[VolunteerBio, String] = Lens.lensu( (vb, s) => vb.copy(image = s), (vb) => vb.image)

  implicit object VolunteerBioRecord extends HasFields[VolunteerBio] {
    val fields: List[DynamicField[VolunteerBio]] = List(
      BasicField[VolunteerBio]("volunteerbio-byline", volunteerBioBylineLens),
      BasicField[VolunteerBio]("volunteerbio-website", volunteerBioWebsiteLens),
      BasicField[VolunteerBio]("volunteerbio-description", volunteerBioDescriptionLens),
      BasicField[VolunteerBio]("volunteerbio-image", volunteerBioImageLens)
    )
  }

}
