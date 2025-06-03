package org.smartdox.doxsite

import org.goldenport.tree._
import org.smartdox._
import org.smartdox.transformers.AutoWireTransformer
import org.smartdox.transformers.AutoI18nTransformer

/*
 * @since   Apr.  5, 2025
 *  version Apr.  7, 2025
 * @version Jun.  1, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSitePreTransformer(
  config: DoxSite.Config,
  val context: DoxSiteTransformer.Context
) extends DoxSiteTransformer {
  override protected def dox_Transformers(
    context: DoxSiteTransformer.Context
  ): List[HomoTreeTransformer[Dox]] =
    List(_auto_wire, _auto_i18n).flatten

  private def _auto_wire =
    if (config.isAutoWire)
      Some(new AutoWireTransformer(context.doxContext))
    else
      None

  private def _auto_i18n =
    if (config.isAutoI18n)
      Some(new AutoI18nTransformer(context.doxContext))
    else
      None
}

object DoxSitePreTransformer {
}
