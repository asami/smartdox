package org.smartdox.doxsite

import java.net.URI
import org.goldenport.tree.TreeNode
import org.smartdox.generator.Context
import org.smartdox.metadata.Notices

/*
 * @since   Apr. 29, 2025
 *  version Apr. 30, 2025
 * @version Jun. 16, 2025
 * @author  ASAMI, Tomoharu
 */
class NoticeCollector(
  context: Context
) extends DoxSiteVisitor {
  import NoticeCollector._

  private var _notices: Vector[Notices.Notice] = Vector.empty

  def notices: Notices = Notices(_notices)

  override protected def enter_Content(node: TreeNode[Node], content: Node): Unit = {
    content match {
      case m: Page => for {
        md <- m.getMetadata
        title <- md.getTitleString
        published <- md.datePublished
      } yield {
        val pathname = node.pathname
        val uri = new URI(pathname)
        val notice = Notices.Notice(
          title,
          md.titleImage,
          md.category,
          uri,
          md.description getOrElse "",
          md.keywords,
          published.toLocalDate,
          md.dateModified.map(_.toLocalDate)
        )
        _notices = _notices :+ notice
      }
      case _ => // do nothing
    }
  }
}

object NoticeCollector {
}
