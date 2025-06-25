package org.smartdox.metadata

import java.net.URI
import org.joda.time.DateTime
import org.joda.time.LocalDate
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import org.goldenport.i18n.I18NString
import org.goldenport.util.CirceUtils
import org.goldenport.util.CirceUtils.Codec._
import org.smartdox._

/*
 * @since   Apr. 28, 2025
 *  version Apr. 30, 2025
 * @version Jun. 24, 2025
 * @author  ASAMI, Tomoharu
 */
case class Notices(
  notices: Vector[Notices.Notice] = Vector.empty
) {
  import Notices._

  def take(status: DocumentMetaData.Status, statuses: DocumentMetaData.Status*): Vector[Notice] =
    take(status +: statuses)

  def take(statuses: Seq[DocumentMetaData.Status]): Vector[Notice] =
    notices.filter(_.status.fold(false)(statuses.contains))
}

object Notices {
  val empty = Notices()

  case class Notice(
    title: String,
    titleImage: Option[URI],
    category: Option[Category],
    uri: URI,
    description: String,
    keywords: List[String],
    published: Option[LocalDate],
    updated: Option[LocalDate],
    kind: Option[DocumentMetaData.Kind],
    status: Option[DocumentMetaData.Status]
  ) {
    def yamlString: String = CirceUtils.toYamlString(this.asJson)
  }
  object Notice {
    val notitle: Notice = Notice(
      "No Title",
      None,
      None,
      new URI("nolink"),
      "No article",
      Nil,
      None,
      None,
      None,
      None
    )

//    import Category.categoryDecoder

    implicit val circeconf = Configuration.default.
      withDefaults.withSnakeCaseMemberNames

    def noticeEncoderRaw: Encoder.AsObject[Notice] = deriveConfiguredEncoder

    implicit val noticeEncoder: Encoder[Notice] = CirceUtils.prefixedEncoder[Notice]("notice.")(noticeEncoderRaw)
  }
  // case class Builder() {
  //   def build(): Notices = ???
  // }
}
