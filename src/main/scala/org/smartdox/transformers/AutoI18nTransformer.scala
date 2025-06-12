package org.smartdox.transformers

import java.util.Locale
import org.goldenport.tree._
import org.goldenport.i18n.LocaleUtils
import org.smartdox._
import org.smartdox.transformer._

/*
 * @since   Apr.  7, 2025
 *  version May. 21, 2025
 * @version Jun. 12, 2025
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
        case xs => directive_nodes(_make_spans(xs))
      }
    case m: Head => directive_node(m.copy(title = _inline_contents(m.title)))
    case m: Section => directive_node(m.copy(title = _inline_contents(m.title)))
    case _ => directive_default
  }

  private def _inline_contents(ps: InlineContents): InlineContents =
    ps.flatMap {
      case m: Text => _text_i18n(m)
      case m => List(m)
    }

  private def _text_i18n(p: Text): List[Inline] = {
    val a = p.contents.split(delimiter).toList
    a match {
      case Nil => List(p)
      case x :: Nil => List(p)
      case xs => _make_spans(xs)
    }
  }

  private def _make_spans(ps: List[String]) = {
    case class Z(xs: Vector[Span] = Vector.empty) {
      def r = xs.toList

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
