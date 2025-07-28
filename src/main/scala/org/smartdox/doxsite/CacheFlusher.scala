package org.smartdox.doxsite

import org.goldenport.tree.TreeNode
import org.smartdox.generator.Context
import org.smartdox.metadata.DoxCacheControl

/*
 * @since   Jul. 26, 2025
 * @version Jul. 26, 2025
 * @author  ASAMI, Tomoharu
 */
class CacheFlusher(
  context: DoxSiteTransformer.Context
) extends DoxSiteVisitor {
  override protected def enter_Content(node: TreeNode[Node], content: Node): Unit = {
    content match {
      case m: Page => m.getHead match {
        case Some(h) => h.doxCacheControl match {
          case Some(dcc) =>
            if (dcc.isFlush(m.lastModified))
              context.cache.set(node.pathname, m.dox)
          case None => 
            context.cache.set(node.pathname, m.dox)
        }
        case None => Unit
      }
      case _ => Unit
    }
  }
}
