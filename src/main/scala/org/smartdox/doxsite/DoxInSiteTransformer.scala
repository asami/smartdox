package org.smartdox.doxsite

import java.net.URI
import org.smartdox._
import org.smartdox.transformer._
import org.smartdox.metadata._
import org.goldenport.values.PathName
import org.goldenport.tree._
import org.goldenport.util.StringUtils

/*
 * @since   Mar.  7, 2025
 * @version Mar.  9, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxInSiteTransformer extends HomoTreeTransformer[Dox] {
  def context: DoxSiteTransformer.Context

  def treeTransformerContext = context.doxContext

  protected final def directive_node(p: Dox): TreeTransformer.Directive[Dox] =
    TreeTransformer.Directive.Node(Dox.toTreeNode(p))

  protected final def directive_nodes(ps: Seq[Dox]): TreeTransformer.Directive[Dox] =
    TreeTransformer.Directive.Nodes(ps.map(Dox.toTreeNode(_)).toList)

  protected final def create_href(from: TreeNode[Node], to: URI, id: Dox.Id) = {
    val path = StringUtils.getRelativePath(from.pathname, to.toString)
    val body = StringUtils.toPathnameBody(path)
    val s = s"${body}.html#${id.id}"
    new URI(s)
  }
}

