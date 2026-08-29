package org.smartdox.transformers

import java.util.Locale
import org.goldenport.Strings
import org.goldenport.tree._
import org.goldenport.i18n.{I18NContainer, LocaleUtils}
import org.goldenport.collection.VectorMap
import org.smartdox._
import org.smartdox.metadata.{DocumentMetaData, Explanation}
import org.smartdox.transformer._

/*
 * @since   Apr.  7, 2025
 *  version Apr.  9, 2025
 *  version May. 21, 2025
 *  version Jun. 12, 2025
 *  version Jul.  3, 2025
 *  version Aug. 31, 2025
 * @version Aug. 29, 2026
 * @author  ASAMI, Tomoharu
 */
class LanguageFilterTransformer private[transformers] (
  val treeTransformerContext: TreeTransformer.Context[Dox],
  private val _strict_locale: Boolean
) extends DoxHomoTreeTransformer {
  import LanguageFilterTransformer._

  def this(treeTransformerContext: TreeTransformer.Context[Dox]) =
    this(treeTransformerContext, false)

  private val _i18ncontext_option = treeTransformerContext.i18NContextOption
  private val _locale_option = _i18ncontext_option.map(_.locale)

  override protected def make_Node(
    node: TreeNode[Dox],
    content: Dox
  ): TreeTransformer.Directive[Dox] = content match {
    case m: Section =>
      if (!_strict_locale || _is_accept(m)) {
        val a = if (_strict_locale) _strict_inlines(m.title) else _distill_inlines(m.title)
        directive_container_content(m.withTitle(a))
      } else {
        directive_empty
      }
    case m: Head =>
      if (!_strict_locale || _is_accept(m)) {
        val meta = _distill_metadata(m.metadata)
        directive_node(m.withDocumentMetaData(meta))
      } else {
        directive_empty
      }
    case m: I18NFragment =>
      val xs = _distill_fragment(m)
      if (_strict_locale)
        directive_nodes(_strict_nodes(xs))
      else
        directive_nodes(xs)
    case m: Value.I18N => _locale_option match {
      case Some(l) if _strict_locale => _strict_value(m, l).fold(directive_empty)(directive_node)
      case Some(l) => m.distill(l).fold(directive_empty)(directive_node)
      case None => directive_empty
    }
    case m =>
      if (_is_accept(m))
        directive_default
      else
        directive_empty
  }

  // private def _distill_inlines(p: Option[I18NFragment]): InlineContents =
  //   p.map(_distill_inlines).getOrElse(Nil)

  private def _distill_inlines(p: I18NFragment): InlineContents =
    _locale_option match {
      case Some(s) => _distill_fragment(p, s).flatMap(Dox.distillInline)
      case None if _strict_locale => Nil
      case None => p.distillInlineContentsDefault
    }

  private def _distill_inlines(ps: List[Inline]) =
    _i18ncontext_option match {
      case Some(s) => ps.flatMap {
        case m: I18NFragment => _distill_fragment(m, s.locale).flatMap(Dox.distillInline)
        case m =>
          if (_is_accept(m))
            List(m)
          else
            Nil
      }
      case None if _strict_locale => Nil
      case None => ps.filter(_is_accept)
    }

  private def _strict_inlines(ps: List[Inline]): InlineContents =
    Dox.transformInlineContents(ps, new LanguageFilterTransformer(treeTransformerContext, true))

  // private def _distill_locale(p: I18NFragment): I18NFragment =
  //   _locale_option match {
  //     case Some(s) => p.distillI18NFragment(s)
  //     case None => p.distillI18NFragmentDefault
  //   }

  private def _is_accept(p: Dox): Boolean =
    p.getLanguage.fold(true)(_is_accept)

  private def _distill_metadata(p: DocumentMetaData): DocumentMetaData =
    _locale_option match {
      case Some(locale) if _strict_locale =>
        p.copy(
          title = p.title.map(x => _distill_i18n_fragment(x, locale)),
          author = p.author.map(x => _distill_i18n_fragment(x, locale)),
          organization = p.organization.map(x => _distill_i18n_fragment(x, locale)),
          explanation = _distill_explanation(p.explanation, locale)
        )
      case None if _strict_locale => p
      case _ => p.distillLocale(_locale_option)
    }

  private def _distill_explanation(p: Explanation, locale: Locale): Explanation =
    p.copy(
      headline = p.headline.map(x => _distill_i18n_fragment(x, locale)),
      brief = p.brief.map(x => _distill_i18n_fragment(x, locale)),
      summary = p.summary.map(x => _distill_i18n_fragment(x, locale)),
      description = p.description.map(x => _distill_i18n_fragment(x, locale)),
      lead = p.lead.map(x => _distill_i18n_fragment(x, locale)),
      `abstract` = p.`abstract`.map(x => _distill_i18n_fragment(x, locale)),
      remarks = p.remarks.map(x => _distill_i18n_fragment(x, locale)),
      tooltip = p.tooltip.map(x => _distill_i18n_fragment(x, locale))
    )

  private def _distill_i18n_fragment(p: I18NFragment, locale: Locale): I18NFragment = {
    val xs = _distill_fragment(p, locale)
    val filtered = if (_strict_locale) _strict_nodes(xs) else xs
    I18NFragment(I18NContainer.make(filtered))
  }

  private def _distill_fragment(p: I18NFragment): List[Dox] =
    _locale_option match {
      case Some(locale) => _distill_fragment(p, locale)
      case None if _strict_locale => Nil
      case None => p.distill(None)
    }

  private def _distill_fragment(p: I18NFragment, locale: Locale): List[Dox] =
    if (_strict_locale) {
      p.distillExact(locale)
    } else {
      p.distill(locale)
    }

  private def _strict_nodes(ps: List[Dox]): List[Dox] =
    Dox.transform(Fragment(ps), new LanguageFilterTransformer(treeTransformerContext, true)) match {
      case m: Fragment => m.contents
      case m => List(m)
    }

  private def _strict_value(p: Value.I18N, locale: Locale): Option[Value] = {
    val exact = p.hangar.get(locale).getOrElse(Vector.empty)
    val values = p.hangar.commons ++ exact
    if (values.nonEmpty)
      Some(Value(values))
    else
      None
  }

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
    _locale_option.fold(true) { locale =>
      if (_strict_locale)
        locale == p
      else
        LocaleUtils.isInclude(locale, p)
    }

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
  private[smartdox] def _strict(
    treeTransformerContext: TreeTransformer.Context[Dox]
  ): LanguageFilterTransformer =
    new LanguageFilterTransformer(treeTransformerContext, true)

  private[smartdox] def _has_exact_locale(p: Dox, locale: Locale): Boolean =
    _has_exact_locale(p, locale, isactive = true, isexact = false)

  private def _has_exact_locale(
    p: Dox,
    locale: Locale,
    isactive: Boolean,
    isexact: Boolean
  ): Boolean = {
    val active = isactive && p.getLanguage.forall(_ == locale)
    if (!active) {
      false
    } else {
      val exact = isexact || p.getLanguage.contains(locale)
      p match {
        case m: Head => _has_exact_metadata(m.metadata, locale, exact)
        case m: Section =>
          _has_exact_inlines(m.title, locale, active, exact) ||
            m.contents.exists(_has_exact_locale(_, locale, active, exact))
        case m: I18NFragment =>
          _selected_fragment_source(m, locale, exact).
            exists(_has_exact_locale(_, locale, active, isexact = true))
        case m: Value.I18N =>
          m.hangar.get(locale).exists(_.exists(x => !Strings.blankp(x))) ||
            (exact && m.hangar.commons.exists(x => !Strings.blankp(x)))
        case m: Value.Single => exact && !Strings.blankp(m.v)
        case m: Value.Multiple => exact && m.vs.exists(x => !Strings.blankp(x))
        case m: Text => exact && !Strings.blankp(m.contents)
        case m if m.elements.nonEmpty =>
          m.elements.exists(_has_exact_locale(_, locale, active, exact))
        case m => exact && _is_effective_leaf(m)
      }
    }
  }

  private def _is_effective_leaf(p: Dox): Boolean = p match {
    case _: Img | _: HorizontalRule => true
    case _: EmptyLine | _: Newline | _: Space => false
    case _: Body => false
    case _: Block | _: Inline => false
    case _ => true
  }

  private def _has_exact_inlines(
    ps: List[Inline],
    locale: Locale,
    isactive: Boolean,
    isexact: Boolean
  ): Boolean =
    ps.exists(_has_exact_locale(_, locale, isactive, isexact))

  private def _has_exact_metadata(
    p: DocumentMetaData,
    locale: Locale,
    isexact: Boolean
  ): Boolean = {
    val fields = List(
      p.title,
      p.author,
      p.organization,
      p.explanation.headline,
      p.explanation.brief,
      p.explanation.summary,
      p.explanation.description,
      p.explanation.lead,
      p.explanation.`abstract`,
      p.explanation.remarks,
      p.explanation.tooltip
    ).flatten
    fields.exists(_has_exact_fragment(_, locale, isexact))
  }

  private def _has_exact_fragment(
    p: I18NFragment,
    locale: Locale,
    isexact: Boolean
  ): Boolean =
    _selected_fragment_source(p, locale, isexact).
      exists(_has_exact_locale(_, locale, isactive = true, isexact = true))

  private def _selected_fragment_source(
    p: I18NFragment,
    locale: Locale,
    isexact: Boolean
  ): List[Dox] =
    if (isexact)
      p.distillExact(locale)
    else
      p._exact_source_contents(locale)

  // val delimiter = "｜"
  // val languages = List(LocaleUtils.ja, LocaleUtils.en)
}
