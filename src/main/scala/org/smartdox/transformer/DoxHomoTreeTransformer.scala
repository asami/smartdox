package org.smartdox.transformer

import org.smartdox._
import org.goldenport.tree._

/*
 * @since   Apr.  5, 2025
 * @version Apr.  7, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxHomoTreeTransformer extends HomoTreeTransformer[Dox] {
  protected final def directive_empty: TreeTransformer.Directive[Dox] =
    TreeTransformer.Directive.Empty()

  protected final def directive_default: TreeTransformer.Directive[Dox] =
    TreeTransformer.Directive.Default()

  protected final def directive_node(p: Dox): TreeTransformer.Directive[Dox] =
    TreeTransformer.Directive.Node(Dox.toTreeNode(p))

  protected final def directive_nodes(ps: Seq[Dox]): TreeTransformer.Directive[Dox] =
    TreeTransformer.Directive.Nodes(ps.map(Dox.toTreeNode(_)).toList)
}
