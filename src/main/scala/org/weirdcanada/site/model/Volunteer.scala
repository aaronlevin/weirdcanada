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

// Lift
import net.liftweb.common.{Empty, Full}
import net.liftweb.db.{DB, DefaultConnectionIdentifier}
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

// scalaz
import scalaz.Lens

// Scala
import scala.xml.NodeSeq

// 3rd party
import org.joda.time.DateTime

// Java
import java.sql.Timestamp

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
  birthday: DateTime,
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
  val volunteerBirthdayLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(birthday = new DateTime(s)), (v) => v.birthday.toString("yyyy-MM-dd"))
  val volunteerBioLens: Lens[Volunteer, VolunteerBio] = Lens.lensu( (v, s) => v.copy(bio = s), (v) => v.bio)

  // Helper function to create text areas
  import DynamicFormFieldRenderHelpers.textAreaRender

  private def birthdayRenderer(updateFunc: String => JsCmd): NodeSeq => NodeSeq = 
    "name=volunteer-birthday-input" #> SHtml.ajaxText((new DateTime).toString("yyyy-MM-dd"), updateFunc, "id" -> "birthday")

  private val genderSelectOptions: Seq[(String, String)] = Seq(("", "(select gender)"),("male", "Male"), ("female", "Female"), ("other", "Other"))

  private def genderSelectRenderer(updateFunc: String => JsCmd): NodeSeq => NodeSeq =
    "name=volunteer-gender-input" #> SHtml.ajaxSelect(genderSelectOptions, Empty, updateFunc)

  private val provinceSelectOptions: Seq[(String,String)] = Seq(("","(select province)"),("bc","British Columbia"),("ab","Alberta"),("sk","Saskatchewan"),("mb","Manitoba"),("on","Ontario"),("qc","Quebec"),("nb","New Brunswick"),("ns","Nova Scotia"),("nl","Newfoundland and Labrador"),("yk","Yukon"),("nt", "Northwest Territories"),("nu", "Nunavut"))

  private def provinceSelectRenderer(updateFunc: String => JsCmd): NodeSeq => NodeSeq = 
    "name=volunteer-province-input" #> SHtml.ajaxSelect(provinceSelectOptions, Empty, updateFunc)

  private val addressArea = textAreaRender("name=volunteer-address-input")("Address") _
  private val whyWorkWithUsArea = textAreaRender("name=volunteer-whyworkwithus-input")("Why Work With Us") _

  implicit object VolunteerRecord extends HasFields[Volunteer] { 
    val fields: List[DynamicField[Volunteer]] = List(
      BasicField[Volunteer]("volunteer-firstname", volunteerFirstNameLens),
      BasicField[Volunteer]("volunteer-lastname", volunteerLastNameLens),
      BasicField[Volunteer]("volunteer-email", volunteerEmailLens),
      BasicField[Volunteer]("volunteer-phone", volunteerPhoneLens),
      BasicField[Volunteer]("volunteer-city", volunteerCityLens),
      BasicField[Volunteer]("volunteer-province", volunteerProvinceLens, Some(provinceSelectRenderer)),
      ManyRecordField[Volunteer, String]("interests", volunteerInterestsLens),
      BasicField[Volunteer]("volunteer-availability", volunteerAvailabilityLens),
      BasicField[Volunteer]("volunteer-whyworkwithus", volunteerWhyWorkWithUsLens, Some(whyWorkWithUsArea)),
      BasicField[Volunteer]("volunteer-gender", volunteerGenderLens, Some(genderSelectRenderer)),
      BasicField[Volunteer]("volunteer-address", volunteerAddressLens, Some(addressArea)),
      BasicField[Volunteer]("volunteer-birthday", volunteerBirthdayLens, Some(birthdayRenderer)),
      RecordField[Volunteer, VolunteerBio]("volunteerbio", volunteerBioLens)
    )
  }

  private val sqlSelectVolunteerId = 
    """
    SELECT id FROM wc_volunteer WHERE first_name = ? AND last_name = ?;
    """

  private val sqlInsertVolunteer: String = 
    """
    INSERT INTO wc_volunteer 
      (id, first_name, last_name, email, phone, city, province, availability, why, gender, address, birthday, bio_english, bio_francais, byline_english, byline_francais, website, image)
      VALUES (default,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);
    """
  private val sqlInsertVolunteerInterest =
    """
    INSERT INTO wc_volunteer_interest VALUES (default,?,?)
    """

  def insertIntoDB(db: DB)(volunteer: Volunteer) = {
    DB.use(DefaultConnectionIdentifier) { conn =>
      DB.prepareStatement(sqlInsertVolunteer, conn) { s =>
        s.setString(1, volunteer.firstName)
        s.setString(2, volunteer.lastName)
        s.setString(3, volunteer.email)
        s.setString(4, volunteer.phone)
        s.setString(5, volunteer.city)
        s.setString(6, volunteer.province)
        s.setString(7, volunteer.availability)
        s.setString(8, volunteer.whyWorkWithUs)
        s.setString(9, volunteer.gender)
        s.setString(10, volunteer.address)
        s.setTimestamp(11, new Timestamp(volunteer.birthday.getMillis))
        s.setString(12, volunteer.bio.descriptionEnglish)
        s.setString(13, volunteer.bio.descriptionFrancais)
        s.setString(14, volunteer.bio.bylineEnglish)
        s.setString(15, volunteer.bio.bylineFrancais)
        s.setString(16, volunteer.bio.website)
        s.setString(17, volunteer.bio.image)
        s.executeUpdate()
      }
      val volunteerId = DB.prepareStatement(sqlSelectVolunteerId, conn) { s =>
        s.setString(1, volunteer.firstName)
        s.setString(2, volunteer.lastName)
        val rs = s.executeQuery() 
        rs.next()
        rs.getLong(1)
      }
      volunteer.interests.toList.foreach { case (_,i) =>
        DB.prepareStatement(sqlInsertVolunteerInterest, conn) { s =>
          s.setString(1,i)
          s.setLong(2,volunteerId)
          s.executeUpdate()
        }
      }

    }
  }

}
