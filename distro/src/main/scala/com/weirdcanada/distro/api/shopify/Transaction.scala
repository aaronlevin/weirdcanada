package org.weirdcanada.distro.api.shopify

import net.liftweb.json._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import org.joda.time.DateTime

object Transaction {
  sealed trait Gateway
  final case object BogusGateway extends Gateway
  final case object RealGateway extends Gateway

  sealed trait Kind
  final case object Authorization extends Kind
  final case object Capture extends Kind
  final case object Sale extends Kind
  final case object Void extends Kind
  final case object Refund extends Kind

  def unapply(jsonString: String): Option[Transaction] = {
    val json = string2jvalue(jsonString)

    case class TempTransaction(
      amount: String,
      authorization: String,
      created_at: String,
      gateway: String,
      id: Long,
      kind: String,
      location_id: String,
      message: String,
      order_id: Long,
      parent_id: Long,
      status: String,
      test: Boolean,
      user_id: String,
      device_id: String,
      receipt: Object
    )
    
    // Shopify has some fields stored as strings where we'd prefer something more usable.
    // So we'll deserialize into a temporary structure and then convert to something better.
    def dedumbifyTransaction(tx: TempTransaction): Transaction = {
      Transaction(
        BigDecimal(tx.amount),
        Option(tx.authorization),
        new DateTime(tx.created_at),
        tx.gateway match {
          case "bogus" => BogusGateway
          case "real" => RealGateway
          case invalidGateway => sys.error("Unhandled gateway on transaction %s: %s".format(tx.id, invalidGateway))
        },
        tx.id,
        tx.kind match {
          case "authorization" => Authorization
          case "capture" => Capture
          case "sale" => Sale
          case "void" => Void
          case "refund" => Refund
          case invalidKind => sys.error("Unhandled kind on transaction %s: %s".format(tx.id, invalidKind))
        },
        Option(tx.location_id),
        tx.message,
        tx.order_id,
        tx.parent_id,
        tx.status,
        tx.test,
        Option(tx.user_id),
        Option(tx.device_id)//,
        //receipt?
      )
    }
    
    implicit val formats = DefaultFormats 
    json.extractOpt[TempTransaction].map(dedumbifyTransaction)
  }
}

case class Transaction(
  amount: BigDecimal,
  authorization: Option[String],
  createdAt: DateTime,
  gateway: Transaction.Gateway,
  id: Long,
  kind: Transaction.Kind,
  locationId: Option[String],
  message: String,
  orderId: Long,
  parentId: Long,
  status: String, // TODO: is this actually an enum?
  test: Boolean,
  userId: Option[String],
  deviceId: Option[String]
  //receipt: ??
)
