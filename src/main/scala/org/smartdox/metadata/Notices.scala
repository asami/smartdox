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
 * @version Jun. 16, 2025
 * @author  ASAMI, Tomoharu
 */
case class Notices(
  notices: Vector[Notices.Notice] = Vector.empty
) {
  import Notices._

  def take(n: Int): Vector[Notice] = notices.take(n)
}

object Notices {
  val empty = Notices()

  implicit val circeconf = Configuration.default.
    withDefaults.withSnakeCaseMemberNames

  implicit val noticeEncoderRaw: Encoder.AsObject[Notice] = deriveConfiguredEncoder

  implicit val noticeEncoder: Encoder[Notice] = CirceUtils.prefixedEncoder[Notice]("notice.")

  case class Notice(
    title: String,
    titleImage: Option[URI],
    category: Option[String],
    uri: URI,
    description: String,
    keywords: List[String],
    published: LocalDate,
    updated: Option[LocalDate]
  ) {
    def yamlString: String = CirceUtils.toYamlString(this.asJson)
  }

  // case class Builder() {
  //   def build(): Notices = ???
  // }
}
