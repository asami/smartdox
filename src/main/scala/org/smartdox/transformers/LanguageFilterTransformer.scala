package org.smartdox.transformers

import java.util.Locale
import org.goldenport.tree._
import org.goldenport.i18n.LocaleUtils
import org.smartdox._
import org.smartdox.transformer._

/*
 * @since   Apr.  7, 2025
 * @version Apr.  9, 2025
 * @author  ASAMI, Tomoharu
 */
class LanguageFilterTransformer(
  val treeTransformerContext: TreeTransformer.Context[Dox],
  val locale: Locale
) extends DoxHomoTreeTransformer {
  import LanguageFilterTransformer._

  override protected def make_Node(
    node: TreeNode[Dox],
    content: Dox
  ): TreeTransformer.Directive[Dox] = content.getLanguage match {
    case Some(s) => if (_is_accept(s))
      directive_default
    else
      directive_empty
    case None => directive_default
  }

  private def _is_accept(p: Locale): Boolean =
    LocaleUtils.isInclude(locale, p)
}

object LanguageFilterTransformer {
  val delimiter = "｜"
  val languages = List(LocaleUtils.ja, LocaleUtils.en)
}
