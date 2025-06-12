package org.smartdox.transformers

import java.util.Locale
import org.goldenport.Strings
import org.goldenport.tree._
import org.goldenport.i18n.LocaleUtils
import org.goldenport.collection.VectorMap
import org.smartdox._
import org.smartdox.transformer._

/*
 * @since   Apr.  7, 2025
 *  version Apr.  9, 2025
 *  version May. 21, 2025
 * @version Jun. 12, 2025
 * @author  ASAMI, Tomoharu
 */
class LanguageFilterTransformer(
  val treeTransformerContext: TreeTransformer.Context[Dox]
) extends DoxHomoTreeTransformer {
  import LanguageFilterTransformer._

  private val _locale_option = treeTransformerContext.i18NContextOption.map(_.locale)

  override protected def make_Node(
    node: TreeNode[Dox],
    content: Dox
  ): TreeTransformer.Directive[Dox] = content match {
    case m: Section =>
      val a = _filter_inlines(m.title)
      directive_container_content(m.withTitle(a))
    case m: Head =>
      val a = _filter_inlines(m.title)
      directive_node(m.withTitle(a))
    case m =>
      if (_is_accept(m))
        directive_default
      else
        directive_empty
  }

  private def _filter_inlines(ps: List[Inline]) =
    ps.filter(_is_accept)

  private def _is_accept(p: Dox): Boolean =
    p.getLanguage.fold(true)(_is_accept)

  override protected def mutation_Normalize_Node(p: TreeNode[Dox]): TreeNode[Dox] = {
    def _to_vector_(x: TreeNode[Dox]): Vector[TreeNode[Dox]] =
      if (_is_blank(x))
        Vector.empty
      else
        Vector(x)

    val cs = p.children.toList
    cs match {
      case Nil => p
      case x :: Nil =>
        if (_is_blank(x)) {
          p.clear()
          p
        } else {
          p
        }
      // case x :: x1 :: Nil =>
      //   val a = (_to_vector_(x) +++ _to_vector_(xs)).flatten
      //   p.setChild(a)
      case x :: xs =>
        val a = _to_vector_(x) ++ xs.init ++ _to_vector_(xs.last)
        p.setChildren(a)
        p
    }
  }

  private def _is_blank(p: TreeNode[Dox]): Boolean =
    p.content match {
      case m: Text => Strings.blankp(m.contents)
      case _ => false
    }

  private def _mark(content: Dox) = content match {
    case m: Paragraph => _divide_languages(m.contents) { (ja, en) =>
      (
        Paragraph(List(ja), VectorMap("lang" -> "ja")),
        Paragraph(List(en), VectorMap("lang" -> "en"))
      )
    }
    case m: Div => _divide_languages(m.contents) { (ja, en) =>
      (
        Div(List(ja), VectorMap("lang" -> "ja")),
        Div(List(en), VectorMap("lang" -> "en"))
      )
    }
    case m: Span => _divide_languages(m.contents) { (ja, en) =>
      (
        Span(List(ja), VectorMap("lang" -> "ja")),
        Span(List(en), VectorMap("lang" -> "en"))
      )
    }
    case m: Text => _divide_languages(m.contents) { (ja, en) =>
      (
        Span(List(ja), VectorMap("lang" -> "ja")),
        Span(List(en), VectorMap("lang" -> "en"))
      )
    }
    case _ => directive_default
  }

  private def _is_accept(p: Locale): Boolean =
    _locale_option.fold(true)(LocaleUtils.isInclude(_, p))

  private def _divide_languages(
    ps: List[Dox]
  )(f: (Text, Text) => (Dox, Dox)): TreeTransformer.Directive[Dox] =
    ps match {
        case Nil => directive_default
        case x :: Nil => x match {
          case mm: Text => _divide_languages(mm.contents)(f)
          case _ => directive_default
        }
      case xs => directive_default
    }

  private def _divide_languages(
    p: String
  )(f: (Text, Text) => (Dox, Dox)): TreeTransformer.Directive[Dox] =
    Strings.totokens(p) match {
      case Nil => directive_default
      case y :: Nil => directive_default
      case y :: y1 :: Nil =>
        val (ja, en) = f(Text(y), Text(y1))
        directive_nodes(ja, en)
      case _ => directive_default
    }
}

object LanguageFilterTransformer {
  // val delimiter = "｜"
  // val languages = List(LocaleUtils.ja, LocaleUtils.en)
}
