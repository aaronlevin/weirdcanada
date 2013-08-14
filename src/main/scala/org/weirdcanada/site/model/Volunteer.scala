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
import DynamicFieldPrimitives.{StringPrimitive,StringPrimitiveEmpty}

// scalaz
import scalaz.Lens

case class Volunteer(
  firstName: String,
  lastName: String,
  email: String,
  phone: String,
  city: String,
  province: String,
  interests: Map[Int, String],
  availability: String,
  whyWorkWithUs: String,
  gender: String,
  address: String,
  birthday: String,
  bio: VolunteerBio
)

object Volunteer {

  // Boilerplate lenses
  val volunteerFirstNameLens: Lens[Volunteer, String] = Lens.lensu( (v, n) => v.copy(firstName = n), (v) => v.firstName)
  val volunteerLastNameLens: Lens[Volunteer, String] = Lens.lensu( (v, n) => v.copy(lastName = n), (v) => v.lastName)
  val volunteerEmailLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(email = s), (v) => v.email)
  val volunteerPhoneLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(phone = s), (v) => v.phone)
  val volunteerCityLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(city = s), (v) => v.city)
  val volunteerProvinceLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(province = s), (v) => v.province)
  val volunteerInterestsLens: Lens[Volunteer, Map[Int, String]] = Lens.lensu( (v, s) => v.copy(interests = s), (v) => v.interests)
  val volunteerAvailabilityLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(availability = s), (v) => v.availability)
  val volunteerWhyWorkWithUsLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(whyWorkWithUs = s), (v) => v.whyWorkWithUs)
  val volunteerGenderLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(gender = s), (v) => v.gender)
  val volunteerAddressLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(address = s), (v) => v.address)
  val volunteerBirthdayLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(birthday = s), (v) => v.birthday)
  val volunteerBioLens: Lens[Volunteer, VolunteerBio] = Lens.lensu( (v, s) => v.copy(bio = s), (v) => v.bio)

  // Helper function to create text areas
  import DynamicFormFieldRenderHelpers.textAreaRender

  implicit object VolunteerRecord extends HasFields[Volunteer] { 
    val fields: List[DynamicField[Volunteer]] = List(
      BasicField[Volunteer]("volunteer-firstname", volunteerFirstNameLens),
      BasicField[Volunteer]("volunteer-lastname", volunteerLastNameLens),
      BasicField[Volunteer]("volunteer-email", volunteerEmailLens),
      BasicField[Volunteer]("volunteer-phone", volunteerPhoneLens),
      BasicField[Volunteer]("volunteer-city", volunteerCityLens),
      BasicField[Volunteer]("volunteer-province", volunteerProvinceLens),
      ManyRecordField[Volunteer, String]("interests", volunteerInterestsLens),
      BasicField[Volunteer]("volunteer-availability", volunteerAvailabilityLens),
      BasicField[Volunteer]("volunteer-whyworkwithus", volunteerWhyWorkWithUsLens),
      BasicField[Volunteer]("volunteer-gender", volunteerGenderLens),
      BasicField[Volunteer]("volunteer-address", volunteerAddressLens),
      BasicField[Volunteer]("volunteer-birthday", volunteerBirthdayLens),
      RecordField[Volunteer, VolunteerBio]("volunteerbio", volunteerBioLens)
    )
  }

}
