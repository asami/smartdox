package org.smartdox.doxsite

import java.net.URI
import org.goldenport.i18n.I18NString
import org.goldenport.tree.TreeNode
import org.goldenport.util.StringUtils
import org.smartdox.generator.Context
import org.smartdox.metadata.Notices
import org.smartdox.metadata.Category
import org.smartdox.metadata.CategoryCollection

/*
 * @since   Apr. 29, 2025
 *  version Apr. 30, 2025
 * @version Jun. 29, 2025
 * @author  ASAMI, Tomoharu
 */
class NoticeCollector(
  context: Context,
  categories: CategoryCollection
) extends DoxSiteVisitor {
  import NoticeCollector._

  private var _notices: Vector[Notices.Notice] = Vector.empty

  def notices: Notices = Notices(_notices)

  override protected def enter_Content(node: TreeNode[Node], content: Node): Unit = {
    content match {
      case m: Page => for {
        md <- m.getMetadata
        title <- md.getTitleI18NString
      } yield {
        val pathname = StringUtils.changeSuffix(node.pathnameRelative, "html")
        val uri = new URI(pathname)
        val category = _find_category(node, md.category)
        val notice = Notices.Notice(
          title,
          md.titleImage,
          category,
          uri,
          md.getDescriptionI18NString getOrElse I18NString.empty,
          md.keywords,
          md.datePublished.map(_.toLocalDate),
          md.dateModified.map(_.toLocalDate),
          md.kindOption,
          md.statusOption
        )
        _notices = _notices :+ notice
      }
      case _ => // do nothing
    }
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
}

object NoticeCollector {
}
