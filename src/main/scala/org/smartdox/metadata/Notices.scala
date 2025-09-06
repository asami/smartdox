package org.smartdox.metadata

import java.net.URI
import java.time.Instant
import java.time.ZoneId
import java.util.Locale
import org.joda.time.DateTime
import org.joda.time.LocalDate
import org.joda.time.DateTimeZone
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.I18NString
import org.goldenport.tree.TreeNode
import org.goldenport.util.InstantUtils
import org.goldenport.util.CirceUtils
import org.goldenport.util.CirceUtils.Codec._
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.doxsite.Node
import org.smartdox.doxsite.Page
import org.smartdox.doxsite.CategoryMetaData

/*
 * @since   Apr. 28, 2025
 *  version Apr. 30, 2025
 *  version Jun. 26, 2025
 *  version Jul. 26, 2025
 *  version Aug. 16, 2025
 * @version Sep.  3, 2025
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

  def toAtomFeed(locale: Locale): AtomFeed = {
    val id = "id:urn:SimpleModeling.org"
    val title = "SimpleModeling.org"
    val updated = _updated
    val links = List(
      AtomFeed.Link(href = "https://www.simplemodeling.org/atom.${locale}.xml", rel = Some("self")),
      AtomFeed.Link(href = "https://www.simplemodeling.org/")
    )
    val entries = _make_entries(locale)
    AtomFeed(
      id,
      title,
      updated,
      links = links,
      entries = entries
    )
  }

  private def _updated = notices.flatMap(_.getTimestamp) match {
    case Vector() => Instant.now
    case xs => xs.max
  }

  private def _make_entries(locale: Locale): List[AtomFeed.Entry] =
    notices.map(_create_entry(locale, _)).toList

  private def _create_entry(locale: Locale, p: Notice): AtomFeed.Entry = {
    val id = p.id
    val title = p.title.as(locale)
    val updated = p.getTimestamp getOrElse Instant.now()
    val published = p.published.map(InstantUtils.toInstant)
    val author = None
    val contributors = Nil
    val links = List(
      AtomFeed.Link(href = s"https://www.simplemodeling.org/${locale}/${p.uri}")
    )
    val categories = Nil
    val content = None
    val summary = Some(p.description.as(locale))
    val rights = None
    val source = None
    AtomFeed.Entry(
      id,
      title,
      updated,
      published,
      author,
      contributors,
      links,
      categories,
      content,
      summary,
      rights,
      source
    )
  }
}

object Notices {
  val empty = Notices()

  case class Notice(
    title: I18NString,
    titleImage: Option[URI],
    category: Option[Category],
    uri: URI,
    summary: I18NString,
    description: I18NString,
    keywords: List[String],
    published: Option[LocalDate],
    updated: Option[LocalDate],
    kind: Option[DocumentMetaData.Kind],
    status: Option[DocumentMetaData.Status],
    lastModified: Option[Instant]
  ) {
    import Notice._

    def id = s"id:urn:${uri}"

    def getTimestamp: Option[Instant] = {
      val a = Vector(
        published.map(_to_instant),
        updated.map(_to_instant),
        lastModified
      ).flatten
      a match {
        case Vector() => None
        case xs => Some(xs.max)
      }
    }

    private def _to_instant(p: LocalDate) = InstantUtils.toInstant(p)

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
      I18NString("No article"),
      Nil,
      None,
      None,
      None,
      None,
      None
    )

    def createOption(node: TreeNode[Node], content: Node): Option[Notice] =
      content match {
        case m: Page => for {
          md <- m.getMetadata
          title <- md.getTitleI18NString
        } yield {
          val pathname = StringUtils.changeSuffix(node.pathnameRelative, "html")
          val uri = new URI(pathname)
          val category = _find_category(node, md.category)
          Notice(
            title,
            md.titleImage,
            category,
            uri,
            md.getSummaryI18NString getOrElse I18NString.empty,
            md.getDescriptionI18NString getOrElse I18NString.empty,
            md.keywords,
            md.publishedAt.map(_.toLocalDate),
            md.modifiedAt.map(_.toLocalDate),
            md.kindOption,
            md.statusOption,
            m.lastModified
          )
        }
        case _ => None
      }

    private def _find_category(
      node: TreeNode[Node],
      p: Option[String]
    ): Option[Category] = _get_category(node.parent)

    private def _get_category(p: TreeNode[Node]): Option[Category] = {
      val a = p.children.flatMap(_.getContent) collect {
        case m: CategoryMetaData => m.category
      }
      a.headOption orElse {
        if (p.isRoot)
          None
        else
          _get_category(p.parent)
      }
    }

//    import Category.categoryDecoder

    implicit val circeconf = Configuration.default.
      withDefaults.withSnakeCaseMemberNames

    def noticeEncoderRaw(implicit ctx: I18NContext): Encoder.AsObject[Notice] = Encoder.AsObject.instance { n =>
      io.circe.JsonObject.fromMap(
        Map(
          "title" -> n.title.distill(ctx).asJson,
          "title_image" -> n.titleImage.asJson,
          "category" -> n.category.asJson(Encoder.encodeOption(Category.categoryEncoderWithLocale(ctx.locale))),
          "uri" -> n.uri.asJson,
          "description" -> n.description.distill(ctx).asJson,
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
