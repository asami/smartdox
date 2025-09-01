package org.smartdox.doxsite

import java.net.URI
import org.joda.time.LocalDate
import org.goldenport.i18n.I18NString
import org.goldenport.tree.TreeNode
import org.goldenport.util.StringUtils
import org.smartdox.generator.Context
import org.smartdox.metadata.Notices
import org.smartdox.metadata.Notices.Notice
import org.smartdox.metadata.History
import org.smartdox.metadata.Category
import org.smartdox.metadata.CategoryCollection

/*
 * @since   Apr. 29, 2025
 *  version Apr. 30, 2025
 *  version Jun. 29, 2025
 *  version Jul. 22, 2025
 * @version Aug. 27, 2025
 * @author  ASAMI, Tomoharu
 */
class NoticeCollector(
  context: Context,
  categories: CategoryCollection
) extends DoxSiteVisitor {
  import NoticeCollector._

  private var _notices: Vector[Notice] = Vector.empty
  private var _history_slots: Vector[History.Slot] = Vector.empty

  def notices: Notices = Notices(_notices)
  def history: History = History(_history_slots)

  override protected def enter_Content(node: TreeNode[Node], content: Node): Unit = {
    if (!node.pathname.startsWith("/glossary/"))
      _enter_content(node, content)
  }

  private def _enter_content(node: TreeNode[Node], content: Node): Unit =
    _make_notice(node, content).foreach(_record_notice)

  private def _make_notice(node: TreeNode[Node], content: Node): Option[Notice] =
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

  private def _record_notice(p: Notice): Unit = {
    if (_is_notice(p))
      _notices = _notices :+ p
    for ((evt, d) <- _make_event_kind(p)) {
      val ckind = _make_content_kind(p)
      val slot = History.Slot(evt, d, ckind, p)
      _history_slots = _history_slots :+ slot
    }
  }

  private def _is_notice(p: Notice) = !p.uri.toString.startsWith("/glossary/")

  private def _make_content_kind(p: Notice): History.ContentKind = {
    val path = p.uri.toString
    if (path.startsWith("/glossary/"))
      History.ContentKind.Glossary
    else if (path.startsWith("/keyword/"))
      History.ContentKind.Keyword
    else if (path.startsWith("/tag/"))
      History.ContentKind.Tag
    else
      History.ContentKind.Article
  }

  private def _make_event_kind(p: Notice): Option[(History.EventKind, LocalDate)] =
    p.updated match {
      case Some(s) => Some((History.EventKind.Updated, s))
      case None => p.published match {
        case Some(ss) => Some((History.EventKind.Created, ss))
        case None => None
      }
    }
}

object NoticeCollector {
}
