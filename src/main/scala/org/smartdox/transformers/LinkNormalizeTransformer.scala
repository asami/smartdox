package org.smartdox.transformers

import java.util.Locale
import org.goldenport.tree._
import org.goldenport.i18n.LocaleUtils
import org.smartdox._
import org.smartdox.transformer._
import org.smartdox.doxsite.DoxSiteTransformer

/*
 * @since   Aug.  5, 2025
 * @version Aug.  5, 2025
 * @author  ASAMI, Tomoharu
 */
class LinkNormalizeTransformer(
  context: DoxSiteTransformer.Context
) extends DoxHomoTreeTransformer {
  val treeTransformerContext: TreeTransformer.Context[Dox] = context.doxContext

  override protected def make_Node(
    node: TreeNode[Dox],
    content: Dox
  ): TreeTransformer.Directive[Dox] = content match {
    case m: ReferenceImg =>
      val uri = m.src.toString
      val newuri = context.normalizeUriName(uri)
      if (uri == newuri)
        directive_node(m)
      else
        directive_node(m.withSrc(newuri))
    case _ => directive_default
  }
}
