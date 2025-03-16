package org.smartdox.doxsite

import org.smartdox._
import org.smartdox.transformer._
import org.smartdox.metadata._
import org.goldenport.tree._

/*
 * @since   Mar.  7, 2025
 * @version Mar.  9, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxSiteTransformer extends HomoTreeTransformer[Node] {
  def context: DoxSiteTransformer.Context

  def treeTransformerContext = context.nodeContext

  protected def dox_Transformer(
    ctx: DoxSiteTransformer.Context
  ): HomoTreeTransformer[Dox]

  override protected def make_Node(
    node: TreeNode[Node],
    content: Node
  ): TreeTransformer.Directive[Node] = content match {
    case m: Page => TreeTransformer.Directive.Node(make_page(node, m))
    case m => TreeTransformer.Directive.Default[Node]
  }

  protected def make_page(
    node: TreeNode[Node],
    p: Page
  ): TreeNode[Node] = make_Page(node, p)

  protected def make_Page(
    node: TreeNode[Node],
    p: Page
  ): TreeNode[Node] = {
    val doxt = dox_Transformer(context.withTreeNode(node))
    val a = Dox.toTree(p.dox)
    val b = a.transform(doxt)
    val c = Dox.toDox(b)
    TreeNode.create(node.name, Page(node.name, c))
  }
}

object DoxSiteTransformer {
  case class Context(
    nodeContext: TreeTransformer.Context[Node],
    doxContext: TreeTransformer.Context[Dox],
    metadata: MetaData = MetaData.empty,
    pageNode: Option[TreeNode[Node]] = None
  ) {
    def withTreeNode(node: TreeNode[Node]) = copy(pageNode = Some(node))
    def withMetaData(metadata: MetaData): Context = copy(metadata = metadata)
  }
}
