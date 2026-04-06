package org.smartdox.transformers

import java.util.Locale
import org.goldenport.tree._
import org.goldenport.i18n.LocaleUtils
import org.smartdox._
import org.smartdox.transformer._
import org.smartdox.doxsite.DoxSiteTransformer

/*
 * @since   Apr.  7, 2025
 *  version May. 21, 2025
 *  version Jun. 28, 2025
 * @version Jul.  4, 2025
 * @author  ASAMI, Tomoharu
 */
class AutoI18nTransformer(
  context: DoxSiteTransformer.Context
) extends DoxHomoTreeTransformer {
  val treeTransformerContext: TreeTransformer.Context[Dox] = context.doxContext

  private lazy val _locale_setting = context.config.doxsiteConfig.map(_.localeSetting)
  private val _delimiter = _locale_setting.map(_.autoI18nDelimiter) getOrElse AutoI18nTransformer.delimiter
  private val _languages = _locale_setting.map(_.autoI18nLanguages) getOrElse AutoI18nTransformer.languages

  override protected def make_Node(
    node: TreeNode[Dox],
    content: Dox
  ): TreeTransformer.Directive[Dox] = content match {
    case m: Head =>
      _transform_head_title(m).map(directive_node).getOrElse(directive_default)
    case m: Section =>
      directive_container_content(m.withTitle(_inline_contents(m.title)))
    case m: Li =>
      directive_container_content(m.copy(contents = _list_contents(m.contents)))
    case m: Text =>
      if (_is_in_preserve(node))
        directive_node(m)
      else {
        val a = m.contents.split(_delimiter).toList
        a match {
          case Nil => directive_node(m)
          case x :: Nil => directive_node(m)
          case xs => directive_nodes(_make_spans(xs))
        }
      }
    case _ => directive_default
  }

  private def _transform_head_title(p: Head): Option[Head] =
    p.title match {
      case Some(title) if title.isSimple =>
        val a = _inline_contents(title.makeInlines)
        if (a == p.titleDefault)
          None
        else
          Some(p.withTitle(a))
      case _ => None
    }

  private def _is_in_preserve(node: TreeNode[Dox]): Boolean =
    node.getParent.exists(_.getContent.exists(_.isInstanceOf[Preserve]))

  private def _inline_contents(ps: InlineContents): InlineContents =
    ps.flatMap {
      case m: Text => _text_i18n(m)
      case m => List(m)
    }

  private def _list_contents(ps: List[ListContent]): List[ListContent] =
    ps.flatMap {
      case m: Text => _text_i18n(m)
      case m => List(m)
    }

  private def _text_i18n(p: Text): List[Inline] = {
    val a = p.contents.split(_delimiter).toList
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
    _languages.zip(ps).foldLeft(Z())(_+_).r
  }
}

object AutoI18nTransformer {
  val delimiter = "｜"
  val languages = List(LocaleUtils.en, LocaleUtils.ja)
}
