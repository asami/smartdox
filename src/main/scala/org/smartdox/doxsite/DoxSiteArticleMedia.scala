package org.smartdox.doxsite

import java.util.Locale
import org.goldenport.collection.VectorMap
import org.smartdox._
import org.smartdox.metadata.PublishMetadata

/*
 * @since   Aug. 30, 2026
 * @version Aug. 30, 2026
 * @author  ASAMI, Tomoharu
 */
private[doxsite] object DoxSiteArticleMedia {
  def projectArticleMedia(
    document: Document,
    sourcePath: String,
    locale: Locale,
    projection: Option[PublishMetadata.ArticleMediaProjection]
  ): Document =
    projectArticleMedia(document, projection.flatMap(_.resolve(sourcePath, locale)), locale)

  def projectArticleMedia(
    document: Document,
    media: Option[PublishMetadata.ArticleMediaVariant],
    locale: Locale
  ): Document =
    media.flatMap(_article_media_block(_, locale, _has_legacy_video_publication(document))) match {
      case Some(block) => _insert_article_media(document, block)
      case _ => document
    }

  private def _article_media_block(
    media: PublishMetadata.ArticleMediaVariant,
    locale: Locale,
    suppressvideo: Boolean
  ): Option[Html5] = {
    val pdfcontrols: List[Dox] = List(
      media.articlePdf.map(_article_pdf_link(_, locale)),
      media.summarySlidesPdf.map(_summary_slides_pdf_link(_, locale))
    ).flatten
    val videocontrols: List[Dox] =
      if (suppressvideo)
        Nil
      else
        media.projectableVideo.toList.flatMap { video =>
          val contents = video.presentation match {
            case PublishMetadata.VideoPresentation.ExternalLink =>
              video.watchUrl.map { watchurl =>
                val label = if (locale.getLanguage == "ja") "動画を見る" else "Watch video"
                Html5("a", VectorMap("href" -> watchurl.toString), List(Text(label)))
              }.toList
            case PublishMetadata.VideoPresentation.SiteHosted =>
              video.contentUrl.map { contenturl =>
                Html5("video", VectorMap("controls" -> "controls", "src" -> contenturl.toString), Nil)
              }.toList
          }
          if (contents.isEmpty)
            Nil
          else
            List(Html5(
              "div",
              VectorMap("class" -> "smartdox-article-media-video"),
              contents
            ))
        }
    val pdfblock: List[Dox] =
      if (pdfcontrols.isEmpty)
        Nil
      else
        List(Html5(
          "div",
          VectorMap("class" -> "smartdox-article-media-pdf"),
          _separate_media_controls(pdfcontrols)
        ))
    val controls = pdfblock ++ videocontrols
    if (controls.isEmpty)
      None
    else
      Some(Html5(
        "div",
        VectorMap("class" -> "smartdox-article-media"),
        controls
      ))
  }

  private def _article_pdf_link(pdf: PublishMetadata.PdfDocumentReference, locale: Locale): Html5 = {
    val label = pdf.label getOrElse _article_pdf_label(locale)
    Html5("a", VectorMap("href" -> pdf.publicPath.toString), List(Text(label)))
  }

  private def _summary_slides_pdf_link(pdf: PublishMetadata.PdfDocumentReference, locale: Locale): Html5 = {
    val label = pdf.label getOrElse _summary_slides_pdf_label(locale)
    Html5("a", VectorMap("href" -> pdf.publicPath.toString), List(Text(label)))
  }

  private def _article_pdf_label(locale: Locale): String =
    if (locale.getLanguage == "ja") "記事 PDF" else "Article PDF"

  private def _summary_slides_pdf_label(locale: Locale): String =
    if (locale.getLanguage == "ja") "要約スライド PDF" else "Summary slides PDF"

  private def _separate_media_controls(controls: List[Dox]): List[Dox] =
    controls.zipWithIndex.flatMap {
      case (control, 0) => List(control)
      case (control, _) => List(Text(" "), control)
    }

  private def _has_legacy_video_publication(document: Document): Boolean =
    _contains_legacy_video_publication(document.body)

  private def _contains_legacy_video_publication(dox: Dox): Boolean =
    dox match {
      case Html5("div", attributes, _, _) if attributes.get("class").contains("smartdox-video-publication") => true
      case _ => dox.elements.exists(_contains_legacy_video_publication)
    }

  private def _insert_article_media(document: Document, block: Html5): Document = {
    val contents = document.body.contents
    val projected = _insert_after_effective_lead(contents, block).getOrElse {
      val sectionindex = contents.indexWhere(_.isInstanceOf[Section])
      if (sectionindex >= 0) {
        val (introduction, sections) = contents.splitAt(sectionindex)
        introduction ++ List(block) ++ sections
      } else {
        contents :+ block
      }
    }
    document.copy(body = document.body.copy(contents = projected))
  }

  private def _insert_after_effective_lead(contents: List[Dox], block: Html5): Option[List[Dox]] = {
    _insert_after_lead(contents, block).orElse {
      contents.zipWithIndex.collectFirst(Function.unlift {
        case (section: Section, index) if section.titleName == "Body" =>
          _insert_after_lead(section.contents, block).map { projected =>
            contents.updated(index, section.copy(contents = projected))
          }
        case _ => None
      })
    }
  }

  private def _insert_after_lead(contents: List[Dox], block: Html5): Option[List[Dox]] = {
    val sectionindex = contents.indexWhere(_.isInstanceOf[Section])
    val introduction = if (sectionindex >= 0) contents.take(sectionindex) else contents
    introduction.indexWhere(_.isInstanceOf[Paragraph]) match {
      case index if index >= 0 => Some(contents.patch(index + 1, List(block), 0))
      case _ => None
    }
  }
}
