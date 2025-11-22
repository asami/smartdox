package org.smartdox.doxsite

import java.net.URI
import org.joda.time.LocalDate
import org.goldenport.i18n.I18NString
import org.goldenport.tree.TreeNode
import org.goldenport.collection.NonEmptyVector
import org.smartdox.{Dox, I18NFragment}
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
 *  version Aug. 27, 2025
 *  version Sep.  3, 2025
 * @version Nov. 21, 2025
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
    if (_is_article(node))
      _enter_content(node, content)
  }

  private def _is_article(node: TreeNode[Node]) = {
    val pathname = node.pathname
    val r = specialPaths.exists(x => pathname.startsWith(x))
    !r
  }

  private def _enter_content(node: TreeNode[Node], content: Node): Unit =
    _make_notice(node, content).foreach(_record_notice)

  private def _make_notice(node: TreeNode[Node], content: Node): Option[Notice] =
    Notice.createOption(node, content)

  private def _record_notice(p: Notice): Unit = {
    if (_is_notice(p))
      _notices = _notices :+ p
    for (xs <- _make_event_kind(p)) {
      for ((evt, date, desc) <- xs.vector) {
        val ckind = _make_content_kind(p)
        val slot = History.Slot(evt, date, ckind, p, desc)
        _history_slots = _history_slots :+ slot
      }
    }
  }

  private def _is_notice(p: Notice) = !p.uri.toString.startsWith(PROP_PATH_GLOSSARY)

  private def _make_content_kind(p: Notice): History.ContentKind = {
    val path = p.uri.toString
    if (path.startsWith(PROP_PATH_GLOSSARY))
      History.ContentKind.Glossary
    else if (path.startsWith(PROP_PATH_BIBLIOGRAPHY))
      History.ContentKind.Bibliography
    else if (path.startsWith(PROP_PATH_KEYWORD))
      History.ContentKind.Keyword
    else if (path.startsWith(PROP_PATH_TAG))
      History.ContentKind.Tag
    else
      History.ContentKind.Article
  }

  private def _make_event_kind(p: Notice): Option[NonEmptyVector[(History.EventKind, LocalDate, Option[I18NFragment])]] = {
    val a = p.published.map(x => (History.EventKind.Created, x, None))
    val b = p.updateds.slots.toVector.map(x => (
      History.EventKind.Updated,
      x.modifiedAt.toLocalDate,
      x.description
    ))
    val c = a.toVector ++ b
    NonEmptyVector.createOption(c)
  }
}

object NoticeCollector {
  val PROP_PATH_GLOSSARY = "/glossary/"
  val PROP_PATH_BIBLIOGRAPHY = "/bibliography/"
  val PROP_PATH_KEYWORD = "/keyword/"
  val PROP_PATH_TAG = "/tag/"

  val specialPaths = Vector(
    PROP_PATH_GLOSSARY,
    PROP_PATH_BIBLIOGRAPHY,
    PROP_PATH_KEYWORD,
    PROP_PATH_TAG
  )
}
