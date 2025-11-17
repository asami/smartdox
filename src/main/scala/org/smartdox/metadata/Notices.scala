package org.smartdox.metadata

import scala.collection.immutable.SortedSet
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
import org.goldenport.util.LocalDateUtils.Implicits._
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
 *  version Sep. 22, 2025
 *  version Oct. 12, 2025
 * @version Nov. 17, 2025
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
    brief: Option[I18NString],
    summary: I18NString,
    description: I18NString,
    keywords: List[String],
    published: Option[LocalDate],
    updateds: DocumentMetaData.UpdateHistory,
    kind: Option[DocumentMetaData.Kind],
    status: Option[DocumentMetaData.Status],
    lastModified: Option[Instant]
  ) {
    import Notice._

    def id = s"id:urn:${uri}"

    def lastUpdated: Option[LocalDate] = updateds.lastOption.map(_.toLocalDate)

    def getTimestamp: Option[Instant] = {
      val a = published.map(_to_instant).toVector ++
      updateds.localDates.map(_to_instant).toVector ++
      lastModified.toVector
      a match {
        case Vector() => None
        case xs => Some(xs.max)
      }
    }

    def effectiveBrief: I18NString = brief getOrElse summary

    private def _to_instant(p: LocalDate) = InstantUtils.toInstant(p)

    def withSummaryDescription(s: Option[I18NString], d: Dox) = {
      val desc = I18NFragment.create(d).toI18NString
      val summary = s getOrElse desc // TODO
      copy(summary = summary, description = desc)
    }

    def withBrief(s: Option[I18NString]) = copy(brief = s)

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
      None,
      I18NString("No article"),
      I18NString("No article"),
      Nil,
      None,
      DocumentMetaData.UpdateHistory.empty,
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
            md.getEffectiveBrief,
            md.getEffectiveSummary getOrElse I18NString.empty,
            md.getEffectiveDescription getOrElse I18NString.empty,
            md.keywords,
            md.publishedAt.map(_.toLocalDate),
            md.modifiedAtHistory,
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
      val effectivebrief = n.brief getOrElse n.summary
      io.circe.JsonObject.fromMap(
        Map(
          "title" -> n.title.distill(ctx).asJson,
          "title_image" -> n.titleImage.asJson,
          "category" -> n.category.asJson(Encoder.encodeOption(Category.categoryEncoderWithLocale(ctx.locale))),
          "uri" -> n.uri.asJson,
          "brief" -> effectivebrief.distill(ctx).asJson,
          "summary" -> n.summary.distill(ctx).asJson,
          "description" -> n.description.distill(ctx).asJson,
          "keywords" -> n.keywords.asJson,
          "published" -> n.published.asJson(Encoder.encodeOption(localdateFormatEncoder)),
          "updateds" -> n.updateds.asJson,
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
