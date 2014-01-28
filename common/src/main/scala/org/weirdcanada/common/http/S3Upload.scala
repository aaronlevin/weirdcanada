package org.weirdcanada.common.http

import argonaut._
import Argonaut._
import org.weirdcanada.common.util.StringUtils

/**
 * Sign requests to S3
 */
object S3 {

  import javax.crypto
  import java.util.{Date,Locale,SimpleTimeZone}
  import java.text.SimpleDateFormat

  case class SignedRequest(url: String, signedRequest: String)
  object SignedRequest {
    implicit def SignedRequestCodec = 
      casecodec2(SignedRequest.apply, SignedRequest.unapply)("url", "signed_request")
  }

  val amzHeaders: Map[String, String] = Map("x-amz-acl" -> "public-read")

  object rfc822DateParser extends SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US) {
    this.setTimeZone(new SimpleTimeZone(0, "GMT"))
  }

  private val s3Root = "s3.amazonaws.com"

  private def defaultExpiryTime = System.currentTimeMillis() / 1000 + 600

  private def stringToBytes(s: String): Array[Byte] = s.getBytes("UTF-8")


  private def md5(bytes: Array[Byte]): String = { import java.security.MessageDigest
    import org.apache.commons.codec.binary.Base64

    val md = MessageDigest.getInstance("MD5")
    md.reset
    md.update(bytes)

    new String(Base64.encodeBase64(bytes))
  }

  /**
   * This is the request string that needs to be signed
   */
  private def canonicalRequestString(
    method: String, 
    path: String, 
    dateOrExpires: Either[Date, Long], 
    contentType: Option[String], 
    contentMd5: Option[String],
    amzHeaders: Map[String, Set[String]]
  ): String = {
    val amzString = amzHeaders.toList.sortWith(_._1.toLowerCase < _._1.toLowerCase).map{ case (k,v) => "%s:%s".format(k.toLowerCase, v.map(StringUtils.trim _).mkString(",")) }
    val dateExpiresString = dateOrExpires match {
      case Left(date) => rfc822DateParser.format(date)
      case Right(expires) => expires.toString
    }
    println("xxx PATH FOR CANONICAL: %s".format(path))
    (method :: contentMd5.getOrElse("") :: contentType.getOrElse("") :: dateExpiresString :: Nil) ++ amzString ++ List(path) mkString("\n")
  }

  /**
   * Given details about a request, produced a canonical string and then sign it
   */
  private def sign(
    method: String, 
    path: String, 
    secretKey: String, 
    dateOrExpires: Either[Date, Long],
    contentType: Option[String], 
    contentMd5: Option[String], 
    amzHeaders: Map[String,Set[String]]
  ): String = {
    import org.apache.commons.codec.binary.Base64
    val SHA1 = "HmacSHA1"
    val message = canonicalRequestString(method, path, dateOrExpires, contentType, contentMd5, amzHeaders)
    val mac = crypto.Mac.getInstance(SHA1)
    val key = new crypto.spec.SecretKeySpec(stringToBytes(secretKey), SHA1)
    mac.init(key)
    println("xxx message to sign: %s".format(message.replace("\n","\nxxx ")))
    new String(Base64.encodeBase64(mac.doFinal(stringToBytes(message))))
  }

  /**
   * Sign an upload to S3
   */
  def signedS3Request(
    method: String, 
    path: String, 
    secretKey: String,
    accessKey: String, 
    amzHeaders: Map[String, Set[String]], 
    expires: Long = defaultExpiryTime, 
    contentType: Option[String] = None, 
    contentMd5: Option[String] = None
  ): String = {
    import java.net.URLEncoder.encode
    val signed = encode(sign(method, path, secretKey, Right(expires), contentType, contentMd5, amzHeaders), "UTF-8")
    "%s?Signature=%s&Expires=%s&AWSAccessKeyId=%s".format(path, signed, expires, accessKey)
  }

  def signedUrl(
    bucket: String,
    objectName: String,
    method: String, 
    secretKey: String,
    accessKey: String, 
    amzHeaders: Map[String, Set[String]], 
    expires: Long = defaultExpiryTime, 
    contentType: Option[String] = None, 
    contentMd5: Option[String] = None
  ): String = {
    val path = "/%s/%s".format(bucket, objectName)
    "https://%s%s".format(s3Root, signedS3Request(method, path, secretKey, accessKey, amzHeaders, expires, contentType, contentMd5))
  }

  def signedUrlJson(
    bucket: String,
    objectName: String,
    method: String, 
    secretKey: String,
    accessKey: String, 
    amzHeaders: Map[String, Set[String]], 
    expires: Long = defaultExpiryTime, 
    contentType: Option[String] = None, 
    contentMd5: Option[String] = None
  ): String = {
    val path = "https://%s.%s/%s".format(bucket, s3Root, objectName)
    SignedRequest(path, signedS3Request(path, method, secretKey, accessKey, amzHeaders, expires, contentType, contentMd5)).asJson.nospaces
  }



}
