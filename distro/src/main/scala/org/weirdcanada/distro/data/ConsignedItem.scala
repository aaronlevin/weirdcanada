package org.weirdcanada.distro.data

import java.math.MathContext
import net.liftweb.common.Box
import net.liftweb.mapper._
import org.joda.time.DateTime
import org.weirdcanada.common.util.{EnumerationUtils, PhysicalCondition, StringParsingUtil}
import PhysicalCondition._
import org.weirdcanada.dynamicform.{BasicField, DynamicField, DynamicFormFieldRenderHelpers, HasFields, HasEmpty, ManyRecordField, ManyTypeaheadField, TypeaheadField}
import StringParsingUtil.safeParse
import scalaz.Lens
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

class ConsignedItem extends LongKeyedMapper[ConsignedItem] with IdPK with OneToMany[Long, ConsignedItem] with CreatedTrait {
  def getSingleton = ConsignedItem

  override def createdAtIndexed_? = true

  
  object consignor extends MappedLongForeignKey(this, Account)
  object album extends MappedLongForeignKey(this, Album)

  object coverCondition extends MappedEnum(this, ConsignedItem.Condition)
  object mediaCondition extends MappedEnum(this, ConsignedItem.Condition)
  object additionalNotes extends MappedText(this)
  // TODO: object age 
  object consignedDate extends MappedDateTime(this)
  object quantity extends MappedInt(this)
  object customerCost extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  object wholesaleCost extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  object markUp extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  
  object guid extends MappedUniqueId(this, 32) with DBIndexed // Use this ID to track an item in the store
}

// The companion object to the above Class
object ConsignedItem extends ConsignedItem with LongKeyedMetaMapper[ConsignedItem] {

  object Condition extends Enumeration with EnumerationUtils {
    type Type = Value
    val StillSealed, NearMint, Excellent, VeryGoodPlus, VeryGood, VeryGoodMinus, Good, Poor = Value

    def fromPhysicalCondition(p: PhysicalCondition): Option[Value] = 
      withNameOption(p.name.replace(" ",""))

    def toPhysicalCondition(cond: Value): PhysicalCondition = cond match {
      case StillSealed => PhysicalCondition.StillSealed
      case NearMint => PhysicalCondition.NearMint
      case Excellent => PhysicalCondition.Excellent
      case VeryGoodPlus => PhysicalCondition.VeryGoodPlus
      case VeryGood => PhysicalCondition.VeryGood
      case VeryGoodMinus => PhysicalCondition.VeryGoodMinus
      case Good => PhysicalCondition.Good
      case Poor => PhysicalCondition.Poor
      case _ => PhysicalCondition.NearMint
    }
  }
  
  object Age extends Enumeration with EnumerationUtils {
    type Type = Value
    val New, Vintage = Value // TODO: anything between new and vintage? perhaps just used/preowned
  }

  def findByGuid(guid: String): Box[ConsignedItem] = {
    find(By(ConsignedItem.guid, guid), PreCache(ConsignedItem.consignor))
  }

  case class ConsignedItemData(
    id: Option[Long],
    coverCondition: PhysicalCondition,
    mediaCondition: PhysicalCondition,
    additionalNotes: String,
    consignedDate: DateTime,
    quantity: Option[Int],
    customerCost: BigDecimal,
    wholesaleCost: BigDecimal,
    markUp: BigDecimal,
    guid: Option[String],
    consignorId: Option[Long],
    albumId: Option[Long]
  )

  /**
   * ConsignorItem lenses
   */
  private val itemIdLens: Lens[ConsignedItemData, String] = Lens.lensu( 
    (c,i) => c.copy(id = safeParse[Long](i)), 
    (c) => c.id.map { _.toString }.getOrElse { "" }
  )
  private val coverConditionLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c, cc) => PhysicalCondition.fromSlug(cc).map { n => c.copy(coverCondition = n) }.getOrElse { c } ,
    (c) => c.coverCondition.slug
  )
  private val mediaConditionLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c, cc) => PhysicalCondition.fromSlug(cc).map { n => c.copy(mediaCondition = n) }.getOrElse { c } ,
    (c) => c.mediaCondition.slug
  )
  private val additionalNotesLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c,a) => c.copy(additionalNotes = a), (c) => c.additionalNotes
  )
  private val dateLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c,ds) => c.copy(consignedDate = new DateTime(ds)),
    (c) => c.consignedDate.toString("YYYY-MM-DD")
  )
  private val quantityLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c,q) => c.copy(quantity = safeParse[Int](q)),
    (c) => c.quantity.map { _.toString }.getOrElse { "" }
  )
  private val customerCostLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c,cc) => safeParse[BigDecimal](cc).map { cost => c.copy(customerCost = cost) }.getOrElse { c },
    (c) => c.customerCost.toString
  )
  private val wholesaleCostLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c,cc) => safeParse[BigDecimal](cc).map { cost => c.copy(wholesaleCost = cost) }.getOrElse { c },
    (c) => c.wholesaleCost.toString
  )
  private val markUpLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c,m) => safeParse[BigDecimal](m).map { cost => c.copy(markUp = cost) }.getOrElse { c },
    (c) => c.markUp.toString
  )
  private val guidLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c,g) => c.copy(guid = Some(g)),
    (c) => c.guid.getOrElse { "" }
  )
  private val consignorIdLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c,i) => c.copy(consignorId = safeParse[Long](i)),
    (c) => c.consignorId.map { _.toString }.getOrElse { "" }
  )
  private val albumIdLens: Lens[ConsignedItemData, String] = Lens.lensu(
    (c,i) => c.copy(albumId = safeParse[Long](i)),
    (c) => c.albumId.map { _.toString }.getOrElse { "" }
  )

  import DynamicFormFieldRenderHelpers.{checkboxRender, selectRender, textAreaRender}
  /**
   * Select fields
   */
  private val conditionSelect =
    selectRender(coverConditionLens.get)("name=consigneditem-coverCondition-input")(PhysicalCondition.conditionNameTuples) _
  private val mediaSelect =
    selectRender(mediaConditionLens.get)("name=consigneditem-mediaCondition-input")(PhysicalCondition.conditionNameTuples) _
  private val notesArea = 
    textAreaRender(additionalNotesLens.get)("name=consigneditem-additionalNotes-input")("") _

    import Album._

  /**
   * Witness to the `HasFields` typeclass
   */
  implicit object ConsignedItemDataFields extends HasFields[ConsignedItemData] {
    val fields: List[DynamicField[ConsignedItemData]] = List(
      BasicField[ConsignedItemData]("consigneditem-coverCondition", coverConditionLens, Some(conditionSelect)),
      BasicField[ConsignedItemData]("consigneditem-mediaCondition", mediaConditionLens, Some(mediaSelect)),
      BasicField[ConsignedItemData]("consigneditem-additionalNotes", additionalNotesLens, Some(notesArea)),
      BasicField[ConsignedItemData]("consigneditem-date", dateLens),
      BasicField[ConsignedItemData]("consigneditem-quantity", quantityLens),
      BasicField[ConsignedItemData]("consigneditem-customerCost", customerCostLens),
      BasicField[ConsignedItemData]("consigneditem-wholesaleCost", wholesaleCostLens),
      BasicField[ConsignedItemData]("consigneditem-markUp", markUpLens),
      BasicField[ConsignedItemData]("consigneditem-consignorId", consignorIdLens),
      TypeaheadField[ConsignedItemData, AlbumData](
        name = "consigneditem-albumId",
        typeaheadLabel = "Add Album",
        apiEndpoint = "/api/album/%QUERY",
        template = "templates-hidden" :: "_add_album" :: Nil,
        sideEffectB = Album.insertAlbumSideEffect,
        bStateValue = (s: String) => Album.findByStringId(s).map { _.title.is}.toOption,
        lens = consignorIdLens
      )
    )
  }

  implicit object ConsignedItemDataEmpty extends HasEmpty[ConsignedItemData] {
    val empty = ConsignedItemData(
      id = None,
      coverCondition = NearMint,
      mediaCondition = NearMint,
      additionalNotes = "",
      consignedDate = new DateTime,
      quantity = None,
      customerCost = 0.0,
      wholesaleCost = 0.0,
      markUp = 1.0,
      guid = None,
      consignorId = None,
      albumId = None
    )
  }

  def findDuplicates(data: ConsignedItemData): List[ConsignedItem] = 
    (for {
      consignorId <- data.consignorId
      albumId <- data.albumId
    } yield {
      ConsignedItem.findAll(
        By(ConsignedItem.album, albumId), 
        By(ConsignedItem.consignor, consignorId)
      )
    }).getOrElse{ List.empty[ConsignedItem] }

  private def setConsignedItemFromData(data: ConsignedItemData, item: ConsignedItem): \/[String, ConsignedItem] = try {
    item
      .additionalNotes(data.additionalNotes)
      .consignedDate(data.consignedDate.toDate)
      .customerCost(data.customerCost)
      .wholesaleCost(data.wholesaleCost)
      .markUp(data.markUp)

    data.consignorId.map { i => item.consignor(i) }
    data.albumId.map { i => item.album(i) }
    Condition.withNameOption(data.coverCondition.name.replace(" ","")).map { c => item.coverCondition(c) }
    Condition.withNameOption(data.mediaCondition.name.replace(" ","")).map { m => item.mediaCondition(m) }
    data.quantity.map { q => item.quantity(q) }
    data.guid.map { g => item.guid(g) }

    \/-(item)
  } catch {
    case e: Throwable => -\/("We've encountered an error...\n%s".format(e))
  }


  def fromData(data: ConsignedItemData): Option[ConsignedItem] = {
    val newConsignedItem = ConsignedItem.create
    setConsignedItemFromData(data, newConsignedItem) match {
      case \/-(newItem) => 
        Some(newItem.saveMe)
      case -\/(_) => None
    }
  }

  def toData(data: ConsignedItem): ConsignedItemData = ConsignedItemData(
    id = Some(data.id.is),
    coverCondition = Condition.toPhysicalCondition(data.coverCondition.is),
    mediaCondition = Condition.toPhysicalCondition(data.mediaCondition.is),
    additionalNotes = data.additionalNotes.is,
    consignedDate = new DateTime(data.consignedDate.is),
    quantity = Some(data.quantity.is),
    customerCost = data.customerCost.is,
    wholesaleCost = data.wholesaleCost.is,
    markUp = data.markUp.is,
    guid = Some(data.guid.is),
    consignorId = Some(data.consignor.is),
    albumId = Some(data.album.is)
  )
 
}
