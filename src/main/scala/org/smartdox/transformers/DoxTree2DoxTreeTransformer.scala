package org.smartdox.transformers

import org.goldenport.RAISE
import org.goldenport.tree.TreeTransformer
import org.smartdox.transformer.DoxTreeTransformer
import org.smartdox._

/*
 * @since   Feb.  1, 2021
 *  version Feb.  2, 2021
 *  version Mar.  5, 2025
 *  version May. 31, 2025
 * @version Jun.  2, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxTree2DoxTreeTransformer(
  val treeTransformerContext: TreeTransformer.Context[Dox],
  override val rule: DoxTree2DoxTreeTransformer.Rule
) extends DoxTreeTransformer[Dox] {
}

object DoxTree2DoxTreeTransformer {
  case class Rule() extends DoxTreeTransformer.Rule[Dox] {
  }

  def transform(rule: Rule, p: Dox): Dox = {
    val ctx = RAISE.notImplementedYetDefect
    val tx = new DoxTree2DoxTreeTransformer(ctx, rule)
    val r = tx.apply(p.toTree)
    Dox.toDox(r)
  }
}
