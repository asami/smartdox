package org.smartdox.transformers

import org.goldenport.Strings
import org.goldenport.tree._
import org.smartdox._
import org.smartdox.transformer._

/*
 * @since   Jun. 12, 2025
 * @version Jun. 12, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxTreeNormalizationTransformer(
  val treeTransformerContext: TreeTransformer.Context[Dox]
) extends DoxHomoTreeTransformer {
  override protected def make_Node(
    node: TreeNode[Dox],
    content: Dox
  ): TreeTransformer.Directive[Dox] = content match {
    case m: Text =>
      if (_is_blank(node, m))
        directive_empty
      else
        directive_default
    case m => directive_default
  }

  private def _is_blank(node: TreeNode[Dox], p: Text): Boolean =
    Strings.blankp(p.contents) && node.getParent.fold(false) { n =>
      val xs = n.children
      xs.headOption.fold(false)(_ == n) ||
      xs.lastOption.fold(false)(_ == n)
    }
}
