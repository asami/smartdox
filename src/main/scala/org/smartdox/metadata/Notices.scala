package org.smartdox.metadata

import java.net.URI
import org.joda.time.DateTime
import org.joda.time.LocalDate
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.I18NString
import org.goldenport.util.CirceUtils
import org.goldenport.util.CirceUtils.Codec._
import org.smartdox._

/*
 * @since   Apr. 28, 2025
 *  version Apr. 30, 2025
 * @version Jun. 26, 2025
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
    title: I18NString,
    titleImage: Option[URI],
    category: Option[Category],
    uri: URI,
    description: I18NString,
    keywords: List[String],
    published: Option[LocalDate],
    updated: Option[LocalDate],
    kind: Option[DocumentMetaData.Kind],
    status: Option[DocumentMetaData.Status]
  ) {
    import Notice._

    def yamlString(ctx: I18NContext): String = {
      val json = this.asJson(noticeEncoder(ctx))
      CirceUtils.toYamlString(json)
    }
  }
  object Notice {
    val notitle: Notice = Notice(
      I18NString("No Title"),
      None,
      None,
      new URI("nolink"),
      I18NString("No article"),
      Nil,
      None,
      None,
      None,
      None
    )

//    import Category.categoryDecoder

    implicit val circeconf = Configuration.default.
      withDefaults.withSnakeCaseMemberNames

    def noticeEncoderRaw(implicit ctx: I18NContext): Encoder.AsObject[Notice] = Encoder.AsObject.instance { n =>
      io.circe.JsonObject.fromMap(
        Map(
          "title" -> n.title.apply(ctx).asJson,
          "title_image" -> n.titleImage.asJson,
          "category" -> n.category.asJson(Encoder.encodeOption(Category.categoryEncoderWithLocale(ctx.locale))),
          "uri" -> n.uri.asJson,
          "description" -> n.description.apply(ctx).asJson,
          "keywords" -> n.keywords.asJson,
          "published" -> n.published.asJson(Encoder.encodeOption(localdateFormatEncoder)),
          "updated" -> n.updated.asJson(Encoder.encodeOption(localdateFormatEncoder)),
          "kind" -> n.kind.asJson,
          "status" -> n.status.asJson
        )
      )
}

    def noticeEncoder(ctx: I18NContext): Encoder[Notice] = CirceUtils.prefixedEncoder[Notice]("notice.")(noticeEncoderRaw(ctx))
  }
  // case class Builder() {
  //   def build(): Notices = ???
  // }
}
