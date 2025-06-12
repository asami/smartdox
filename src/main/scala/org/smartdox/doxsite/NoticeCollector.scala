package org.smartdox.doxsite

import org.goldenport.tree.TreeNode
import org.smartdox.generator.Context
import org.smartdox.metadata.Notices

/*
 * @since   Apr. 29, 2025
 *  version Apr. 30, 2025
 * @version Jun.  9, 2025
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
        md <- m.getMetadata(context)
        title <- md.getTitleString
        date <- md.datePublished
      } yield {
        val notice = Notices.Notice(title, date)
        _notices = _notices :+ notice
      }
      case _ => // do nothing
    }
  }
}

object NoticeCollector {
}
