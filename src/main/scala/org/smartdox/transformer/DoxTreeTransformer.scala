package org.smartdox.transformer

import org.goldenport.RAISE
import org.goldenport.tree.{TreeTransformer, TreeNode}
import org.smartdox._

/*
 * @since   Feb.  1, 2021
 * @version Feb.  2, 2021
 * @author  ASAMI, Tomoharu
 */
trait DoxTreeTransformer[T] extends TreeTransformer[Dox, T] {
}

object DoxTreeTransformer {
  trait Rule[B] extends TreeTransformer.Rule[Dox, B] {
    def getTargetName(p: TreeNode[Dox]): Option[String] = RAISE.notImplementedYetDefect
    def mapContent(p: Dox): B = RAISE.notImplementedYetDefect
  }
}
