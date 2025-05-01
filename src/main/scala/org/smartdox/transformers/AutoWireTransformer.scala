package org.smartdox.transformers

import org.goldenport.tree._
import org.goldenport.util.RegexMatchSequence
import org.smartdox._
import org.smartdox.transformer._

/*
 * @since   Apr.  5, 2025
 * @version Apr.  5, 2025
 * @author  ASAMI, Tomoharu
 */
class AutoWireTransformer(
  val treeTransformerContext: TreeTransformer.Context[Dox]
) extends DoxHomoTreeTransformer {
  override protected def make_Node(
    node: TreeNode[Dox],
    content: Dox
  ): TreeTransformer.Directive[Dox] = content match {
    case m: Text =>
      val a = RegexMatchSequence.createUrl(m.contents)
      if (a.isMatched)
        _make(a)
      else
        directive_node(m)
    case _ => directive_default
  }

  private def _make(p: RegexMatchSequence) = {
    val a = p.vector.map {
      case m: RegexMatchSequence.Slot.Matched => Hyperlink.create(m.text)
      case m: RegexMatchSequence.Slot.Unmatched => Text(m.text)
    }
    directive_nodes(a)
  }
}

object AutoWireTransformer {
}
