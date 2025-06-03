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
 * @version May. 21, 2025
 * @author  ASAMI, Tomoharu
 */
class LanguageFilterTransformer(
  val treeTransformerContext: TreeTransformer.Context[Dox],
  val locale: Locale
) extends DoxHomoTreeTransformer {
  import LanguageFilterTransformer._

  override protected def make_Node(
    node: TreeNode[Dox],
    content: Dox
  ): TreeTransformer.Directive[Dox] = content.getLanguage match {
    case Some(s) => if (_is_accept(s))
      directive_default
    else
      directive_empty
    case None => content match {
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
  }

  private def _is_accept(p: Locale): Boolean =
    LocaleUtils.isInclude(locale, p)


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
  val delimiter = "｜"
  val languages = List(LocaleUtils.ja, LocaleUtils.en)
}
