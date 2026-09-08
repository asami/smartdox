package org.smartdox.doxsite

import java.util.Locale
import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import org.goldenport.collection.VectorMap
import org.smartdox._
import org.smartdox.metadata.PublishMetadata

/*
 * @since   Aug. 30, 2026
 * @version Sep.  9, 2026
 * @author  ASAMI, Tomoharu
 */
private[doxsite] object DoxSiteArticleMedia {
  def projectArticleHeader(
    document: Document,
    sourcePath: String,
    locale: Locale,
    projection: Option[PublishMetadata.ArticleMediaProjection]
  ): Document =
    projectArticleHeader(document, projection.flatMap(_.resolve(sourcePath, locale)), locale)

  def projectArticleHeader(
    document: Document,
    media: Option[PublishMetadata.ArticleMediaVariant],
    locale: Locale
  ): Document = {
    val actions = media.toList.flatMap(_article_header_actions(_, locale, _has_legacy_video_publication(document)))
    val metadata = _article_header_metadata(document, locale)
    val headercontents = metadata.toList ++ actions
    val withheader = if (headercontents.isEmpty)
      document
    else
      _insert_header(document, Html5(
        "div",
        VectorMap("class" -> "smartdox-article-header"),
        headercontents
      ))
    media.flatMap(_.infographic).map(_article_infographic(_, locale)).fold(withheader) { figure =>
      _insert_after_effective_lead(withheader.body.contents, figure).map { contents =>
        withheader.copy(body = withheader.body.copy(contents = contents))
      }.getOrElse {
        val contents = withheader.body.contents
        val sectionindex = contents.indexWhere(_.isInstanceOf[Section])
        if (sectionindex >= 0) {
          val (introduction, sections) = contents.splitAt(sectionindex)
          withheader.copy(body = withheader.body.copy(contents = introduction ++ List(figure) ++ sections))
        } else {
          withheader.copy(body = withheader.body.copy(contents = contents :+ figure))
        }
      }
    }
  }

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

  private def _article_header_metadata(document: Document, locale: Locale): Option[Html5] = {
    val tags = _metadata_string_list(document, "tags", "tag")
    val publishedat = _metadata_string(document, "published_at", "publishedAt")
    val items: List[Dox] =
      (if (tags.nonEmpty) List(_article_header_tags(tags, locale): Dox) else Nil) ++
        publishedat.toList.map(_article_header_published_at(_, locale))
    if (items.isEmpty)
      None
    else
      Some(Html5(
        "div",
        VectorMap("class" -> "smartdox-article-header-metadata"),
        _separate_media_controls(items)
      ))
  }

  private def _article_header_tags(tags: Vector[String], locale: Locale): Html5 = {
    val label = if (locale.getLanguage == "ja") "タグ" else "Tags"
    Html5(
      "div",
      VectorMap("class" -> "smartdox-article-header-tags"),
      List(
        Html5("span", VectorMap("class" -> "smartdox-article-header-label"), List(Text(label))),
        Html5("ul", VectorMap.empty, tags.map(tag => Html5("li", VectorMap.empty, List(Text(tag)))).toList)
      )
    )
  }

  private def _article_header_published_at(value: String, locale: Locale): Html5 = {
    val label = if (locale.getLanguage == "ja") "公開日" else "Published"
    Html5(
      "div",
      VectorMap("class" -> "smartdox-article-header-published-at"),
      List(
        Html5("span", VectorMap("class" -> "smartdox-article-header-label"), List(Text(label))),
        Html5("time", VectorMap("datetime" -> value), List(Text(value)))
      )
    )
  }

  private def _article_header_actions(
    media: PublishMetadata.ArticleMediaVariant,
    locale: Locale,
    suppressvideo: Boolean
  ): Option[Html5] = {
    val controls: List[Dox] =
      (if (suppressvideo) Nil else media.projectableVideo.toList.flatMap(_article_header_video_action(_, locale))) ++
        media.summarySlidesPdf.toList.map(_article_header_summary_slides_pdf_action(_, locale)) ++
        media.articlePdf.toList.map(_article_header_article_pdf_action(_, locale)) ++
        media.infographic.toList.map(_article_header_infographic_action(_, locale))
    if (controls.isEmpty)
      None
    else
      Some(Html5(
        "div",
        VectorMap("class" -> "smartdox-article-header-actions"),
        _separate_media_controls(controls)
      ))
  }

  private def _article_header_video_action(video: PublishMetadata.VideoReference, locale: Locale): Option[Html5] = {
    val target = video.presentation match {
      case PublishMetadata.VideoPresentation.ExternalLink => video.watchUrl
      case PublishMetadata.VideoPresentation.SiteHosted => video.contentUrl
    }
    target.map { uri =>
      Html5(
        "a",
        VectorMap(
          "class" -> "smartdox-article-header-action",
          "href" -> uri.toString
        ),
        List(Text(_article_header_video_label(locale)))
      )
    }
  }

  private def _article_header_summary_slides_pdf_action(
    pdf: PublishMetadata.PdfDocumentReference,
    locale: Locale
  ): Html5 =
    _article_header_action(pdf.publicPath.toString, _summary_slides_pdf_label(locale))

  private def _article_header_article_pdf_action(
    pdf: PublishMetadata.PdfDocumentReference,
    locale: Locale
  ): Html5 =
    _article_header_action(pdf.publicPath.toString, _article_pdf_label(locale))

  private def _article_header_infographic_action(
    image: PublishMetadata.ImageReference,
    locale: Locale
  ): Html5 =
    _article_header_action("#smartdox-article-infographic", _article_header_infographic_label(locale))

  private def _article_header_action(target: String, label: String): Html5 =
    Html5(
      "a",
      VectorMap("class" -> "smartdox-article-header-action", "href" -> target),
      List(Text(label))
    )

  private def _article_header_video_label(locale: Locale): String =
    if (locale.getLanguage == "ja") "動画を見る" else "Watch video"

  private def _article_header_infographic_label(locale: Locale): String =
    if (locale.getLanguage == "ja") "インフォグラフィックを見る" else "View infographic"

  private def _article_infographic(image: PublishMetadata.ImageReference, locale: Locale): Html5 = {
    val imageelement = Html5(
      "img",
      VectorMap(
        "src" -> image.publicPath.toString,
        "alt" -> image.alt.getOrElse("")
      ),
      Nil
    )
    val link = Html5(
      "a",
      VectorMap(
        "href" -> image.publicPath.toString,
        "aria-label" -> _article_header_infographic_label(locale)
      ),
      List(imageelement)
    )
    Html5(
      "figure",
      VectorMap(
        "id" -> "smartdox-article-infographic",
        "class" -> "smartdox-article-infographic"
      ),
      List(link)
    )
  }

  private def _metadata_string(document: Document, keys: String*): Option[String] =
    document.head.metadata.properties.flatMap { hocon =>
      keys.toStream.flatMap { key =>
        try {
          if (hocon.hasPath(key))
            Some(hocon.getString(key)).map(_.trim).filter(_.nonEmpty)
          else
            None
        } catch {
          case NonFatal(_) => None
        }
      }.headOption
    }

  private def _metadata_string_list(document: Document, keys: String*): Vector[String] =
    document.head.metadata.properties.toVector.flatMap { hocon =>
      keys.toStream.flatMap { key =>
        try {
          if (hocon.hasPath(key)) {
            val values = hocon.getStringList(key).asScala.toVector.flatMap(_metadata_list_token)
            if (values.nonEmpty) Some(values) else None
          } else {
            None
          }
        } catch {
          case NonFatal(_) =>
            try {
              val rawvalue = hocon.getString(key).trim
              val listvalue =
                if (rawvalue.length >= 2 && rawvalue.startsWith("[") && rawvalue.endsWith("]"))
                  rawvalue.substring(1, rawvalue.length - 1)
                else
                  rawvalue
              val values = listvalue.split(',').toVector.flatMap(_metadata_list_token)
              if (values.nonEmpty) Some(values) else None
            } catch {
              case NonFatal(_) => None
            }
        }
      }.headOption
    }.flatten

  private def _metadata_list_token(value: String): Option[String] = {
    val trimmed = value.trim
    val quoted =
      trimmed.length >= 2 &&
        ((trimmed.startsWith("\"") && trimmed.endsWith("\"")) ||
          (trimmed.startsWith("'") && trimmed.endsWith("'")))
    val unquoted = if (quoted) trimmed.substring(1, trimmed.length - 1).trim else trimmed
    Option(unquoted).filter(_.nonEmpty)
  }

  private def _insert_header(document: Document, header: Html5): Document =
    document.copy(body = document.body.copy(contents = header :: document.body.contents))

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
