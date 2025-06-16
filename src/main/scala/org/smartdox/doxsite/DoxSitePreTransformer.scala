package org.smartdox.doxsite

import org.goldenport.tree._
import org.smartdox._
import org.smartdox.transformers.AutoWireTransformer
import org.smartdox.transformers.AutoI18nTransformer

/*
 * @since   Apr.  5, 2025
 *  version Apr.  7, 2025
 * @version Jun. 16, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSitePreTransformer(
  config: DoxSite.Config,
  val context: DoxSiteTransformer.Context
) extends DoxSiteTransformer {
  override protected def dox_Transformers(
    context: DoxSiteTransformer.Context,
    node: TreeNode[Node],
    page: Page
  ): List[HomoTreeTransformer[Dox]] =
    List(_auto_wire(page), _auto_i18n(page)).flatten

  private def _auto_wire(p: Page) =
    if (config.isAutoWire(p))
      Some(new AutoWireTransformer(context.doxContext))
    else
      None

  private def _auto_i18n(p: Page) =
    if (config.isAutoI18n(p))
      Some(new AutoI18nTransformer(context.doxContext))
    else
      None
}

object DoxSitePreTransformer {
}
