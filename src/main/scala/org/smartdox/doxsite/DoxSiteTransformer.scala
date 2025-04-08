package org.smartdox.doxsite

import org.goldenport.RAISE
import org.smartdox._
import org.smartdox.transformer._
import org.smartdox.metadata._
import org.goldenport.tree._

/*
 * @since   Mar.  7, 2025
 *  version Mar.  9, 2025
 * @version Apr.  5, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxSiteTransformer extends HomoTreeTransformer[Node] {
  def context: DoxSiteTransformer.Context

  def treeTransformerContext = context.nodeContext

  protected def dox_Transformers(
    ctx: DoxSiteTransformer.Context
  ): List[HomoTreeTransformer[Dox]] = List(dox_Transformer(ctx))

  protected def dox_Transformer(
    ctx: DoxSiteTransformer.Context
  ): HomoTreeTransformer[Dox] = RAISE.notImplementedYetDefect("DoxSiteTransformer#dox_Transformer")

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
    dox_Transformers(context.withTreeNode(node)) match {
      case Nil => TreeNode.create(node.name, Page(node.name, p.dox))
      case xs => 
        val a = Dox.toTree(p.dox)
        val b = xs.foldLeft(a)((z, x) => z.transform(x))
        val c = Dox.toDox(b)
        TreeNode.create(node.name, Page(node.name, c))
    }
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
