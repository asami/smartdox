package org.smartdox.transformer

import org.goldenport.RAISE
import org.goldenport.tree.{TreeTransformer, TreeNode}
import org.smartdox._

/*
 * @since   Feb.  1, 2021
 *  version Mar.  4, 2025
 * @version Jun.  2, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxTreeTransformer[T] extends TreeTransformer[Dox, T] {
}

object DoxTreeTransformer {
  trait Rule[B] extends TreeTransformer.Rule[Dox, B] {
  }
}
