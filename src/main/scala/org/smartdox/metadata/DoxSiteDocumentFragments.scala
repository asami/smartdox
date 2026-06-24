package org.smartdox.metadata

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._

/*
 * @since   Jun. 24, 2026
 * @version Jun. 24, 2026
 * @author  ASAMI, Tomoharu
 */
case class DoxSiteDocumentFragments(
  fragments: Vector[DoxSiteDocumentFragments.Fragment] = Vector.empty
)

object DoxSiteDocumentFragments {
  implicit val circeconf: Configuration = Configuration.default.withDefaults.withSnakeCaseMemberNames

  val empty: DoxSiteDocumentFragments = DoxSiteDocumentFragments()

  case class Fragment(
    sourcePath: String,
    publicPath: String,
    locale: String,
    kind: Option[String],
    category: Option[String],
    title: Option[String],
    headline: Option[String],
    brief: Option[String],
    bodyHtml: String
  )
  object Fragment {
    implicit val fragmentEncoder: Encoder.AsObject[Fragment] = deriveConfiguredEncoder
  }

  implicit val documentFragmentsEncoder: Encoder.AsObject[DoxSiteDocumentFragments] = deriveConfiguredEncoder

  def toJsonString(p: DoxSiteDocumentFragments): String =
    p.asJson.spaces2 + "\n"
}
