package org.smartdox.metadata

import scala.xml.{Node => XNode, Text => XText, _}
import java.util.Locale
import com.typesafe.config.{Config => Hocon}
import org.goldenport.context.Consequence
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.util.OptionUtils
import org.smartdox._

/*
 * @since   Aug. 16, 2025
 *  version Aug. 25, 2025
 *  version Sep. 22, 2025
 * @version Oct. 25, 2025
 * @author  ASAMI, Tomoharu
 */
case class Explanation(
  headline: Option[I18NFragment] = None,
  brief: Option[I18NFragment] = None,
  summary: Option[I18NFragment] = None,
  description: Option[I18NFragment] = None,
  lead: Option[I18NFragment] = None,
  `abstract`: Option[I18NFragment] = None,
  remarks: Option[I18NFragment] = None,
  tooltip: Option[I18NFragment] = None
) {
  import Explanation._

  def isEmpty = headline.isEmpty && brief.isEmpty && tooltip.isEmpty && summary.isEmpty &&
  `abstract`.isEmpty && description.isEmpty && remarks.isEmpty

  def getEffectiveHeadline = headline orElse brief orElse tooltip

  def getEffectiveSummary = summary orElse description orElse brief

  def withSummary(p: InlineContents) = {
    val x = Dox.trimSingleLine(p)
    copy(summary = Some(I18NFragment.create(x)))
  }

  def withSummary(p: String) =
    copy(summary = Some(I18NFragment.create(p)))

  def +(rhs: Explanation): Explanation =
    Explanation(
      _plus(headline, rhs.headline),
      _plus(brief, rhs.brief),
      _plus(summary, rhs.summary),
      _plus(description, rhs.description),
      _plus(lead, rhs.lead),
      _plus(`abstract`, rhs.`abstract`),
      _plus(remarks, rhs.remarks),
      _plus(tooltip, rhs.tooltip)
    )

  private def _plus(l: Option[I18NFragment], r: Option[I18NFragment]): Option[I18NFragment] =
    OptionUtils.lastOption(l, r)

  def distillLocale(locale: Option[Locale]): Explanation =
    locale.fold(distillLocaleDefault)(distillLocale)

  def distillLocale(locale: Locale): Explanation = {
    def _distill_(p: Option[I18NFragment]) = p.map(_.distillI18NFragment(locale))
    Explanation(
      _distill_(headline),
      _distill_(brief),
      _distill_(summary),
      _distill_(description),
      _distill_(lead),
      _distill_(`abstract`),
      _distill_(remarks),
      _distill_(tooltip)
    )
  }

  def distillLocaleDefault: Explanation = {
    def _distill_(p: Option[I18NFragment]) = p.map(_.distillI18NFragmentDefault)
    Explanation(
      _distill_(headline),
      _distill_(brief),
      _distill_(summary),
      _distill_(description),
      _distill_(lead),
      _distill_(`abstract`),
      _distill_(remarks),
      _distill_(tooltip)
    )
  }

  def getEffectiveTooltip: Option[I18NFragment] = tooltip orElse brief orElse headline orElse summary orElse `abstract`

  def printFlat(buf: StringBuilder): Unit = {
    Dox.printI18NFragment(buf, PROP_HEADLINE, headline)
    Dox.printI18NFragment(buf, PROP_BRIEF, brief)
    Dox.printI18NFragment(buf, PROP_SUMMARY, summary)
    Dox.printI18NFragment(buf, PROP_DESCRIPTION, description)
    Dox.printI18NFragment(buf, PROP_LEAD, lead)
    Dox.printI18NFragment(buf, PROP_ABSTRACT, `abstract`)
    Dox.printI18NFragment(buf, PROP_REMARKS, remarks)
    Dox.printI18NFragment(buf, PROP_TOOLTIP, tooltip)
  }
}
object Explanation {
  final val PROP_HEADLINE = "headline"
  final val PROP_BRIEF = "brief"
  final val PROP_SUMMARY = "summary"
  final val PROP_DESCRIPTION = "description"
  final val PROP_LEAD = "lead"
  final val PROP_ABSTRACT = "abstract"
  final val PROP_REMARKS = "remarks"
  final val PROP_TOOLTIP = "tooltip"

  trait Holder {
    def explanation: Explanation

    def headline = explanation.headline
    def brief = explanation.brief
    def summary = explanation.summary
    def description = explanation.description
    def lead = explanation.lead
    def `abstract` = explanation.`abstract`
    def remarks = explanation.remarks
    def tooltip = explanation.tooltip
  }

  val empty = Explanation()

  def parse(hocon: Hocon): Consequence[Explanation] =
    for {
      headline <- _parse_hocon(PROP_HEADLINE, hocon)
      brief <- _parse_hocon(PROP_BRIEF, hocon)
      summary <- _parse_hocon(PROP_SUMMARY, hocon)
      description <- _parse_hocon(PROP_DESCRIPTION, hocon)
      lead <- _parse_hocon(PROP_LEAD, hocon)
      `abstract` <- _parse_hocon(PROP_ABSTRACT, hocon)
      remarks <-  _parse_hocon(PROP_REMARKS, hocon)
      tooltip <- _parse_hocon(PROP_TOOLTIP, hocon)
    } yield Explanation(
      headline,
      brief,
      summary,
      description,
      lead,
      `abstract`,
      remarks,
      tooltip
    )

  private def _parse_hocon(
    key: String,
    hocon: Hocon
  ): Consequence[Option[I18NFragment]] = hocon.cStringOption(key).map(_.map(x => I18NFragment.create(x)))

  def parse(p: XNode): Consequence[Explanation] =
    for {
      headline <- _parse_node(PROP_HEADLINE, p)
      brief <- _parse_node(PROP_BRIEF, p)
      summary <- _parse_node(PROP_SUMMARY, p)
      description <- _parse_node(PROP_DESCRIPTION, p)
      lead <- _parse_node(PROP_LEAD, p)
      `abstract` <- _parse_node(PROP_ABSTRACT, p)
      remarks <-  _parse_node(PROP_REMARKS, p)
      tooltip <- _parse_node(PROP_TOOLTIP, p)
    } yield Explanation(
      headline,
      brief,
      summary,
      description,
      lead,
      `abstract`,
      remarks,
      tooltip
    )

  private def _parse_node(
    key: String,
    p: XNode
  ): Consequence[Option[I18NFragment]] = I18NFragment.getC(key, p)

  def parse(h: Section): Consequence[Explanation] =
    parse(h.elements)

  def parse(ps: Seq[Dox]): Consequence[Explanation] =
    for {
      headline <- _parse_subsection_inline(PROP_HEADLINE, ps)
      brief <- _parse_subsection_inline(PROP_BRIEF, ps)
      summary <- _parse_subsection_inline(PROP_SUMMARY, ps)
      description <- _parse_subsection(PROP_DESCRIPTION, ps)
      lead <- _parse_subsection(PROP_LEAD, ps)
      `abstract` <- _parse_subsection_inline(PROP_ABSTRACT, ps)
      remarks <-  _parse_subsection_inline(PROP_REMARKS, ps)
      tooltip <- _parse_subsection_inline(PROP_TOOLTIP, ps)
    } yield Explanation(
      headline,
      brief,
      summary,
      description,
      lead,
      `abstract`,
      remarks,
      tooltip
    )

  private def _parse_subsection(
    key: String,
    ps: Seq[Dox]
  ): Consequence[Option[I18NFragment]] = Consequence {
    val k = key.toUpperCase
    val a = ps.toStream.collect {
      case m: Section if m.nameForModel == k => m
    }.headOption
    a.map(x => I18NFragment.create(x.contents))
  }

  private def _parse_subsection_inline(
    key: String,
    ps: Seq[Dox]
  ): Consequence[Option[I18NFragment]] = Consequence {
    val k = key.toUpperCase
    val a = ps.toStream.collect {
      case m: Section if m.nameForModel == k => m
    }.headOption
    a.map(x => I18NFragment.create(_normalize(x.contents)))
  }

  private def _normalize(ps: List[Dox]): List[Dox] = ps match {
    case Nil => Nil
    case x :: Nil => x match {
      case m: Paragraph if m.attributes.isEmpty => m.contents
      case _ => List(x)
    }
    case xs => xs
  }
}
