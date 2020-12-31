 package org.smartdox

import java.util.Locale

/*
 * Derived from SSummary.
 *
 * @since   Sep. 16, 2008
 *  version Oct. 26, 2008
 *  version Jun. 15, 2020
 *  version Jul. 27, 2020
 *  version Aug. 13, 2020
 *  version Sep. 29, 2020
 * @version Dec. 27, 2020
 */
case class Resume(
  captionOption: Option[Dox] = None, // 見出し
  briefOption: Option[Dox] = None, // 適用・概要
  summaryOption: Option[Dox] = None // 要約
) {
  def isEmpty: Boolean = captionOption.isEmpty && briefOption.isEmpty && summaryOption.isEmpty
  def toOption: Option[Resume] = if (isEmpty) None else Some(this)

  def withCaption(p: Dox) = copy(captionOption = Some(p))
  def withBrief(p: Dox) = copy(briefOption = Some(p))
  def withSummary(p: Dox) = copy(summaryOption = Some(p))

  def caption: Dox = captionOption getOrElse Dox.empty

  // def caption_en: Dox = {
  //   _captions_locale.getEnOrElse(caption)
  // }

  // def caption_en_=(aCaption: Dox) {
  //   _captions_locale.putEn(aCaption)
  // }

  // def brief_en: Dox = {
  //   _briefs_locale.getEnOrElse(brief)
  // }

  // def brief_en_=(aBrief: Dox) = {
  //   _briefs_locale.putEn(aBrief)
  // }

  // def summary_en: Dox = {
  //   _summaries_locale.getEnOrElse(summary)
  // }

  // def summary_en_=(aSummary: Dox) = {
  //   _summaries_locale.putEn(aSummary)
  // }

  def summary: Dox = effectiveSummary

  def effectiveSummary: Dox = summaryOption getOrElse {
    (captionOption, briefOption) match {
      case (Some(c), Some(b)) => Fragment(c, b)
      case (Some(c), None) => c
      case (None, Some(b)) => b
      case (None, None) => Dox.empty
    }
  }

  def brief: Dox = briefOption orElse captionOption orElse summaryOption getOrElse Dox.empty

  // final def copyIn(aSummary: SSummary) {
  //   _caption = aSummary._caption
  //   _brief = aSummary._brief
  //   _summary = aSummary._summary
  //   _captions_locale.copyIn(aSummary._captions_locale)
  //   _briefs_locale.copyIn(aSummary._briefs_locale)
  //   _summaries_locale.copyIn(aSummary._summaries_locale)
  // }
}

object Resume {
  val empty = Resume()

  trait Holder {
    protected def resume: Resume

    def getCaption: Option[Dox] = resume.captionOption
    def getBrief: Option[Dox] = resume.briefOption
    def getSummary: Option[Dox] = resume.summaryOption
  }

  def summary(p: Dox): Resume =
    if (p.isEmpty)
      empty
    else
      empty.copy(summaryOption = Some(p))
}
