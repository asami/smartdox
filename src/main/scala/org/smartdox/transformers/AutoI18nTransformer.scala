package org.smartdox.transformers

import java.util.Locale
import org.goldenport.tree._
import org.goldenport.i18n.LocaleUtils
import org.smartdox._
import org.smartdox.transformer._

/*
 * @since   Apr.  7, 2025
 * @version May. 21, 2025
 * @author  ASAMI, Tomoharu
 */
class AutoI18nTransformer(
  val treeTransformerContext: TreeTransformer.Context[Dox]
) extends DoxHomoTreeTransformer {
  import AutoI18nTransformer._

  override protected def make_Node(
    node: TreeNode[Dox],
    content: Dox
  ): TreeTransformer.Directive[Dox] = content match {
    case m: Text =>
      val a = m.contents.split(delimiter).toList
      a match {
        case Nil => directive_node(m)
        case x :: Nil => directive_node(m)
        case xs => _make_spans(xs)
      }
    case _ => directive_default
  }

  private def _make_spans(ps: List[String]) = {
    case class Z(xs: Vector[Span] = Vector.empty) {
      def r = directive_nodes(xs)

      def +(rhs: (Locale, String)) =
        copy(xs = xs :+ Span.create(rhs._1, rhs._2))
    }
    languages.zip(ps).foldLeft(Z())(_+_).r
  }
}

object AutoI18nTransformer {
  val delimiter = "｜"
  val languages = List(LocaleUtils.ja, LocaleUtils.en)
}
