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
import org.weirdcanada.site.lib.DBHelpers
import org.weirdcanada.common.util.EmailUtil

// Lift
import net.liftweb.common.{Empty, Full}
import net.liftweb.db.{DB, DefaultConnectionIdentifier}
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

// scalaz
import scalaz.Lens

// Scala
import scala.annotation.tailrec
import scala.xml.NodeSeq

// 3rd party
import org.joda.time.DateTime

// Java
import java.sql.{PreparedStatement, ResultSet, Timestamp}

case class Volunteer(
  id: Option[Long],
  firstName: String,
  lastName: String,
  email: String,
  phone: String,
  city: String,
  province: String,
  interests: Map[Int, VolunteerInterest],
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
  val volunteerInterestsLens: Lens[Volunteer, Map[Int, VolunteerInterest]] = Lens.lensu( (v, s) => v.copy(interests = s), (v) => v.interests)
  val volunteerAvailabilityLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(availability = s), (v) => v.availability)
  val volunteerWhyWorkWithUsLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(whyWorkWithUs = s), (v) => v.whyWorkWithUs)
  val volunteerGenderLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(gender = s), (v) => v.gender)
  val volunteerAddressLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(address = s), (v) => v.address)
  val volunteerBirthdayLens: Lens[Volunteer, String] = Lens.lensu( (v, s) => v.copy(birthday = new DateTime(s)), (v) => v.birthday.toString("yyyy-MM-dd"))
  val volunteerBioLens: Lens[Volunteer, VolunteerBio] = Lens.lensu( (v, s) => v.copy(bio = s), (v) => v.bio)

  // Helper function to create text areas
  import DynamicFormFieldRenderHelpers.{textAreaRender, selectRender}

  private def birthdayRenderer(current: Volunteer)(updateFunc: String => JsCmd): NodeSeq => NodeSeq = 
    "name=volunteer-birthday-input" #> SHtml.ajaxText(
      (new DateTime).toString("yyyy-MM-dd"), updateFunc, "id" -> "birthday", "value" -> volunteerBirthdayLens.get(current)
    )

  private val genderSelectOptions: Seq[(String, String)] = Seq(("", "(select gender)"),("male", "Male"), ("female", "Female"), ("other", "Other"))
  private def genderSelectRenderer: Volunteer => (String => JsCmd) => (NodeSeq => NodeSeq) = 
    selectRender(volunteerGenderLens.get)("name=volunteer-gender-input")(genderSelectOptions) _

  val provinceSelectOptions: Seq[(String,String)] = Seq(("","(select province)"),("bc","British Columbia"),("ab","Alberta"),("sk","Saskatchewan"),("mb","Manitoba"),("on","Ontario"),("qc","Quebec"),("nb","New Brunswick"),("ns","Nova Scotia"),("nl","Newfoundland and Labrador"),("yk","Yukon"),("nt", "Northwest Territories"),("nu", "Nunavut"))
  private def provinceSelectRenderer: Volunteer => (String => JsCmd) => (NodeSeq => NodeSeq) = 
    selectRender(volunteerProvinceLens.get)("name=volunteer-province-input")(provinceSelectOptions) _

  private val addressArea = textAreaRender(volunteerAddressLens.get)("name=volunteer-address-input")("Address") _
  private val whyWorkWithUsArea = textAreaRender(volunteerWhyWorkWithUsLens.get)("name=volunteer-whyworkwithus-input")("Why Work With Us") _

  /**
   * Method to render a volunteer given the template in: `_volunteer_bio.html`
   *
   * @param volunteer the volunteer whose bio we're rendinger
   * @return a nodeseq -> nodeseq transformation
   */
  def renderVolunteerBio(volunteer: Volunteer, isEnglishBio: Boolean): NodeSeq => NodeSeq = {
    import EmailUtil.parseEmail

    val parsedEmail: Option[(String, String, String)] = parseEmail(volunteer.email)

    "name=volunteer-bio-image [src]" #> volunteer.bio.image &
    "name=volunteer-bio-name *" #> "%s %s".format(volunteer.firstName, volunteer.lastName) &
    "name=volunteer-bio-tagline *" #> { if(isEnglishBio) volunteer.bio.bylineEnglish else volunteer.bio.bylineFrancais } &
    "name=volunteer-bio-geo *" #> "%s, %s".format(volunteer.city, volunteer.province) &
    "name=volunteer-bio-website [href]" #> volunteer.bio.website &
    "name=volunteer-bio-website *" #> volunteer.bio.website &
    "name=volunteer-bio-email *" #> parsedEmail.map { case (e, h, t) => "%s [at] %s [dot] %s".format(e,h,t) } &
    "name=volunteer-bio-words *" #> { if(isEnglishBio) volunteer.bio.descriptionEnglish else volunteer.bio.descriptionFrancais }
  }


  implicit object VolunteerRecord extends HasFields[Volunteer] { 
    val fields: List[DynamicField[Volunteer]] = List(
      BasicField[Volunteer]("volunteer-firstname", volunteerFirstNameLens),
      BasicField[Volunteer]("volunteer-lastname", volunteerLastNameLens),
      BasicField[Volunteer]("volunteer-email", volunteerEmailLens),
      BasicField[Volunteer]("volunteer-phone", volunteerPhoneLens),
      BasicField[Volunteer]("volunteer-city", volunteerCityLens),
      BasicField[Volunteer]("volunteer-province", volunteerProvinceLens, Some(provinceSelectRenderer)),
      ManyRecordField[Volunteer, VolunteerInterest]("volunteer-interests", volunteerInterestsLens),
      BasicField[Volunteer]("volunteer-availability", volunteerAvailabilityLens),
      BasicField[Volunteer]("volunteer-whyworkwithus", volunteerWhyWorkWithUsLens, Some(whyWorkWithUsArea)),
      BasicField[Volunteer]("volunteer-gender", volunteerGenderLens, Some(genderSelectRenderer)),
      BasicField[Volunteer]("volunteer-address", volunteerAddressLens, Some(addressArea)),
      BasicField[Volunteer]("volunteer-birthday", volunteerBirthdayLens, Some(birthdayRenderer)),
      RecordField[Volunteer, VolunteerBio]("volunteerbio", volunteerBioLens)
    )
  }

  private val sqlInsertVolunteer: String = 
    """
    SELECT volunteer_upsert(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?::text[])
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
        s.setArray(18, conn.createArrayOf("varchar", volunteer.interests.values.map{_.interest}.toArray))

        s.execute()
      }
    }
  }

  /**
   * Helper method to create a volunteer from result set
   */
  private def getVolunteerFromRs(rs: ResultSet): Volunteer = 
    Volunteer(
      Option(rs.getLong(1)),
      rs.getString(2),
      rs.getString(3),
      rs.getString(4),
      rs.getString(5),
      rs.getString(6),
      rs.getString(7),
      Map(rs.getRow -> VolunteerInterest(rs.getString(19))),
      Option(rs.getString(8)).getOrElse(""),
      Option(rs.getString(9)).getOrElse(""),
      Option(rs.getString(10)).getOrElse(""),
      Option(rs.getString(11)).getOrElse(""),
      Option(new DateTime(rs.getTimestamp(12))).getOrElse(new DateTime()),
      VolunteerBio(
        Option(rs.getString(13)).getOrElse(""),
        Option(rs.getString(14)).getOrElse(""),
        Option(rs.getString(15)).getOrElse(""),
        Option(rs.getString(16)).getOrElse(""),
        Option(rs.getString(17)).getOrElse(""),
        Option(rs.getString(18)).getOrElse("")
      )
    )

  /**
   * Take a list of volunteers, group them by id, then merge their maps.
   */
  private def reduceVolunteers(volunteers: Iterable[Volunteer]): Iterable[Volunteer] = {
    volunteers
      .groupBy { _.id }
      .flatMap { case (_, vs) => 
        vs match {
          case head::_ => 
            val newMap: Map[Int, VolunteerInterest] = 
              vs
              .flatMap { volunteerInterestsLens.get(_).values }
              .zipWithIndex
              .map { case (v, i) => i -> v }
              .toMap

            Some(volunteerInterestsLens.set(head, newMap))
          case _ => None
        }
      }
  }

  private val firstNameClauseString = "lower(wcv.first_name) = lower(%s)"
  private val lastNameClauseString = "lower(wcv.last_name) = lower(%s)"
  private val cityClauseString = "lower(wcv.city) ~* %s"
  private val provinceClauseString = "wcv.province = %s"
  private val interestsClauseString = "lower(wcvi.interest) IN (%s)"

  private val sqlSearchVolunteers = """
    SELECT 
      wcv.id,
      wcv.first_name,
      wcv.last_name,
      wcv.email,
      wcv.phone,
      wcv.city,
      wcv.province,
      wcv.availability,
      wcv.why,
      wcv.gender,
      wcv.address,
      wcv.birthday,
      wcv.bio_english,
      wcv.bio_francais,
      wcv.byline_english,
      wcv.byline_francais,
      wcv.website,
      wcv.image,
      wcvi.interest
    FROM
      wc_volunteer_interest AS wcvi
      INNER JOIN wc_volunteer AS wcv ON (wcv.id = wcvi.volunteer_id)
    %s
  """

  def searchVolunteers(
    db: DB
  )(
    firstName: Option[String], 
    lastName: Option[String], 
    city: Option[String], 
    province: Option[String],
    interests: Iterable[String]
  ): Iterable[Volunteer] = {
    import DBHelpers.{createClauseString, constructClause, setStatement}

    val firstNameClause: Option[String] = createClauseString(firstNameClauseString, firstName)
    val lastNameClause: Option[String] = createClauseString(lastNameClauseString, lastName)
    val cityClause: Option[String] = createClauseString(cityClauseString, city)
    val provinceClause: Option[String] = createClauseString(provinceClauseString, province)
    val interestsClause: Option[String] = createClauseString(interestsClauseString, interests)

    val clause: String = constructClause(Some("WHERE"), firstNameClause, lastNameClause, cityClause, provinceClause, interestsClause)

    val sql: String = sqlSearchVolunteers.format(clause)

    val collections: List[Iterable[String]] = List(firstName, lastName, city, province, interests)

    DB.use(DefaultConnectionIdentifier) { conn =>
      DB.prepareStatement(sql, conn) { s =>

        setStatement(s, 1, collections)

        DBHelpers.executeQueryWithReducer(s)( reduceVolunteers ){ getVolunteerFromRs }
      }
    }
  }

  private val sqlGetVolunteerByName = """
    SELECT 
      wcv.id,
      wcv.first_name,
      wcv.last_name,
      wcv.email,
      wcv.phone,
      wcv.city,
      wcv.province,
      wcv.availability,
      wcv.why,
      wcv.gender,
      wcv.address,
      wcv.birthday,
      wcv.byline_english,
      wcv.byline_francais,
      wcv.website,
      wcv.bio_english,
      wcv.bio_francais,
      wcv.image,
      wcvi.interest
    FROM
      wc_volunteer_interest AS wcvi
      INNER JOIN wc_volunteer AS wcv ON (wcv.id = wcvi.volunteer_id)
    WHERE
          lower(wcv.first_name) = lower(?)
      AND lower(wcv.last_name) = (?)
  """

  def getVolunteerByName(db: DB)(firstName: String, lastName: String): Option[Volunteer] = {
    DB.use(DefaultConnectionIdentifier) { conn =>
      DB.prepareStatement(sqlGetVolunteerByName, conn) { s =>
        s.setString(1, firstName)
        s.setString(2, lastName)

        val volunteers: List[Volunteer] = DBHelpers.executeQuery(s){ getVolunteerFromRs }

        volunteers.headOption.map { v =>
          volunteers.foldLeft(v){ (acc, newV) => 
            val accMap = acc.interests
            val newMap = newV.interests
            acc.copy(interests = (accMap ++ newMap))
          }
        }
      }
    }
  }

  private val sqlGetVolunteerById = """
    SELECT 
      wcv.id,
      wcv.first_name,
      wcv.last_name,
      wcv.email,
      wcv.phone,
      wcv.city,
      wcv.province,
      wcv.availability,
      wcv.why,
      wcv.gender,
      wcv.address,
      wcv.birthday,
      wcv.byline_english,
      wcv.byline_francais,
      wcv.website,
      wcv.bio_english,
      wcv.bio_francais,
      wcv.image,
      wcvi.interest
    FROM
      wc_volunteer_interest AS wcvi
      INNER JOIN wc_volunteer AS wcv ON (wcv.id = wcvi.volunteer_id)
    WHERE
      wcv.id = ?
  """

  def getVolunteerById(db: DB)(id: Long): Option[Volunteer] = {
    DB.use(DefaultConnectionIdentifier) { conn =>
      DB.prepareStatement(sqlGetVolunteerById, conn) { s =>
        s.setLong(1, id)

        val volunteers: List[Volunteer] = DBHelpers.executeQuery(s){ getVolunteerFromRs }

        volunteers.headOption.map { v =>
          volunteers.foldLeft(v){ (acc, newV) => 
            val accMap = acc.interests
            val newMap = newV.interests
            acc.copy(interests = (accMap ++ newMap))
          }
        }
      }
    }
  }
}
