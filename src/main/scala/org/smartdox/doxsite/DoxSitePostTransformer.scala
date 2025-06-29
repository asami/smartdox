package org.smartdox.doxsite

import org.goldenport.tree._
import org.smartdox._

/*
 * @since   Apr.  5, 2025
 *  version Apr.  5, 2025
 * @version Jun. 28, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSitePostTransformer(
  val context: DoxSiteTransformer.Context
) extends DoxSiteTransformer {
  override protected def dox_Transformers(
    context: DoxSiteTransformer.Context,
    node: TreeNode[Node],
    p: Page
  ): List[HomoTreeTransformer[Dox]] = Nil
}

object DoxSitePostTransformer {
}
