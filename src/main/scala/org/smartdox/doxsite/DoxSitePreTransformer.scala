package org.smartdox.doxsite

import org.goldenport.tree._
import org.smartdox._
import org.smartdox.transformers.AutoWireTransformer
import org.smartdox.transformers.AutoI18nTransformer

/*
 * @since   Apr.  5, 2025
 * @version Apr.  7, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSitePreTransformer(val context: DoxSiteTransformer.Context)
    extends DoxSiteTransformer {

  override protected def dox_Transformers(
    context: DoxSiteTransformer.Context
  ): List[HomoTreeTransformer[Dox]] = List(
    new AutoWireTransformer(context.doxContext),
    new AutoI18nTransformer(context.doxContext)
  )
}

object DoxSitePreTransformer {
}
