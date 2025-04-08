package org.smartdox.doxsite

import java.net.URI
import org.goldenport.tree._
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.transformer._

/*
 * @since   Mar.  7, 2025
 * @version Apr.  5, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxInSiteTransformer extends DoxHomoTreeTransformer {
  def context: DoxSiteTransformer.Context

  def treeTransformerContext = context.doxContext

  protected final def create_href(from: TreeNode[Node], to: URI, id: Dox.Id) = {
    val path = StringUtils.getRelativePath(from.pathname, to.toString)
    val body = StringUtils.toPathnameBody(path)
    val s = s"${body}.html#${id.id}"
    new URI(s)
  }
}

