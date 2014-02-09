package org.weirdcanada.dynamicform

import java.net.{MalformedURLException, URL}
import net.liftweb.http.js.JsCmd
import scala.xml.NodeSeq
import scalaz.Lens

sealed trait S3Bucket { val slug: String }
case object WCS3Image extends S3Bucket { val slug = "wc-img" }
case object WCS3Audio extends S3Bucket { val slug = "wc-tracks" }
case object WCS3Binary extends S3Bucket { val slug = "wc-dump" }

sealed trait S3Resource { val url: URL ; val bucket: S3Bucket}
case class S3Image(url: URL) extends S3Resource { val bucket = WCS3Image }
case class S3Audio(url: URL) extends S3Resource { val bucket = WCS3Audio }
case class S3Binary(url: URL) extends S3Resource { val bucket = WCS3Binary }

object S3Resource {

  def urlFromString(s: String): Option[URL] = try {
    Some(new URL(s))
  } catch { case _:MalformedURLException => None }

  import DynamicFormFieldRenderHelpers.s3SignedUploadRender

  val s3ImageUrlLens: Lens[S3Image, String] = Lens.lensu(
    (u,i) => try { u.copy(url = new URL(i)) } catch { case _:MalformedURLException => u },
    (u) => u.url.toString
  )

  val s3AudioUrlLens: Lens[S3Audio, String] = Lens.lensu(
    (u,i) => try { u.copy(url = new URL(i)) } catch { case _:MalformedURLException => u },
    (u) => u.url.toString
  )

  val s3BinaryUrlLens: Lens[S3Binary, String] = Lens.lensu(
    (u,i) => try { u.copy(url = new URL(i)) } catch { case _:MalformedURLException => u },
    (u) => u.url.toString
  )

  implicit object S3ImageFields extends HasFields[S3Image] {
    val fields: List[DynamicField[S3Image]] = List(
      BasicField[S3Image](
        "s3-url", 
        s3ImageUrlLens, 
        Some(s3SignedUploadRender(s3ImageUrlLens.get)("@s3-url")("/sign_s3/%s".format(WCS3Image.slug), "name", "type") _ )
      )
    )
  }

  implicit object S3AudioFields extends HasFields[S3Audio] {
    val fields: List[DynamicField[S3Audio]] = List(
      BasicField[S3Audio](
        "s3-url", 
        s3AudioUrlLens, 
        Some(s3SignedUploadRender(s3AudioUrlLens.get)("@s3-url")("/sign_s3/%s".format(WCS3Audio.slug), "name", "type") _ )
      )
    )
  }

  implicit object S3BinaryFields extends HasFields[S3Binary] {
    val fields: List[DynamicField[S3Binary]] = List(
      BasicField[S3Binary](
        "s3-url", 
        s3BinaryUrlLens, 
        Some(s3SignedUploadRender(s3BinaryUrlLens.get)("@s3-url")("/sign_s3/%s".format(WCS3Binary.slug), "name", "type") _ )
      )
    )
  }

  implicit object EmptyS3Image extends HasEmpty[S3Image] {
    val empty: S3Image = S3Image(url = new URL("http://s3.amazonaws.com"))
  }

  implicit object EmptyS3Audio extends HasEmpty[S3Audio] {
    val empty: S3Audio = S3Audio(url = new URL("http://s3.amazonaws.com"))
  }

  implicit object EmptyS3Binary extends HasEmpty[S3Binary] {
    val empty: S3Binary = S3Binary(url = new URL("http://s3.amazonaws.com"))
  }

}

