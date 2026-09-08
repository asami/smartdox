package org.smartdox.metadata

import java.net.URI

import io.circe.Json

import PublishMetadata._

private[metadata] object PublishMetadataArticleMediaSupport {
  private val _document_project_entrypoint_pattern = """^(.+)\.dox/index\.(?:dox|html)$""".r

  def sourcePathToArticleIdentity(sourcePath: String): Option[String] = {
    val normalized = sourcePath.trim.replace('\\', '/').stripPrefix("/")
    val suffixfree = normalized match {
      case _document_project_entrypoint_pattern(identity) => identity
      case value if value.endsWith(".dox") => value.stripSuffix(".dox")
      case value if value.endsWith(".html") => value.stripSuffix(".html")
      case value => value
    }
    _normalize_article_identity_option(suffixfree)
  }

  def resolveArticleMedia(
    variants: Map[(String, String), ArticleMediaVariant],
    compatibilityVariants: Map[String, ArticleMediaVariant],
    articleIdentity: String,
    locale: String
  ): Option[ArticleMediaVariant] =
    for {
      identity <- _normalize_article_identity_option(articleIdentity)
      localetag <- _normalize_locale_option(locale)
      variant <- variants.get(identity -> localetag).orElse(compatibilityVariants.get(identity))
    } yield variant

  def createArticleMediaRegistry(entries: Vector[Entry], videos: Vector[VideoPublication]): ArticleMediaRegistry = {
    val publications = entries.flatMap(_.articleMediaPublication)
    _validate_article_media_duplicates(publications)
    val compatibility = _compatibility_variants(videos)
    ArticleMediaRegistry(publications, compatibility._1, compatibility._2)
  }

  def parseArticleMediaPublication(json: Json, source: String): ArticleMediaPublication =
    _article_media_publication(json, source)

  private def _validate_article_media_duplicates(publications: Vector[ArticleMediaPublication]): Unit = {
    val duplicates = publications.flatMap { publication =>
      publication.variants.map(variant => publication.articleIdentity -> variant.locale)
    }.groupBy(identity).collect {
      case (identity, xs) if xs.size > 1 => identity
    }.toVector.sortBy { case (identity, locale) => s"$identity/$locale" }
    duplicates.headOption.foreach {
      case (identity, locale) =>
        throw new IllegalArgumentException(s"Duplicate article-media publication variant: $identity [$locale]")
    }
  }

  private def _compatibility_variants(videos: Vector[VideoPublication]): (Map[String, ArticleMediaVariant], Vector[ArticleMediaDiagnostic]) = {
    val candidates = videos.flatMap(_compatibility_candidate)
    val grouped = candidates.groupBy(_._1).toVector.sortBy(_._1)
    val diagnostics = Vector.newBuilder[ArticleMediaDiagnostic]
    val variants = grouped.flatMap {
      case (identity, xs) if xs.size == 1 => Some(identity -> xs.head._2)
      case (identity, _) =>
        diagnostics += ArticleMediaDiagnostic(
          "article-media.video-publication-conflict",
          s"Multiple video-publication records derive article media for: $identity"
        )
        None
    }.toMap
    val invalid = videos.flatMap(_invalid_compatibility_diagnostic)
    variants -> (invalid ++ diagnostics.result()).sortBy(x => x.code + ":" + x.message)
  }

  private def _compatibility_candidate(video: VideoPublication): Option[(String, ArticleMediaVariant)] =
    for {
      identity <- _legacy_article_identity(video)
      contenturl <- _site_visible_uri_option(video.publicPath)
    } yield identity -> ArticleMediaVariant(
      locale = "",
      video = Some(VideoReference(
        presentation = VideoPresentation.SiteHosted,
        status = VideoStatus.Published,
        contentUrl = Some(contenturl)
      ))
    )

  private def _invalid_compatibility_diagnostic(video: VideoPublication): Option[ArticleMediaDiagnostic] =
    _legacy_article_identity(video).flatMap { identity =>
      if (_site_visible_uri_option(video.publicPath).nonEmpty)
        None
      else
        Some(ArticleMediaDiagnostic(
          "article-media.video-publication-invalid-content-url",
          s"Video publication for $identity has an invalid article-media content URL: ${video.publicPath}"
        ))
    }

  private def _article_media_publication(json: Json, source: String): ArticleMediaPublication = {
    val identity = _required_json_string(json, source, "article", "identity")
    val variants = _required_json_object(json, source, "variants").toVector.sortBy(_._1).map {
      case (locale, variant) => _article_media_variant(identity, locale, variant, source)
    }
    if (variants.isEmpty)
      throw new IllegalArgumentException(s"Article-media publication must define variants: $source")
    ArticleMediaPublication(_normalize_article_identity(identity), variants)
  }

  private def _article_media_variant(identity: String, locale: String, json: Json, source: String): ArticleMediaVariant = {
    val localetag = _normalize_locale(locale, requirecanonical = true)
    val infographic = _json_field(json, "infographic").map(_image_reference(_, identity, localetag, source))
    val video = _json_field(json, "video").map(_video_reference(_, identity, localetag, source))
    val articlepdf = _json_field(json, "article_pdf").map(_pdf_document_reference(_, "article_pdf", identity, localetag, source))
    val summaryslidespdf = _json_field(json, "summary_slides_pdf").map(_pdf_document_reference(_, "summary_slides_pdf", identity, localetag, source))
    ArticleMediaVariant(localetag, infographic, video, articlepdf, summaryslidespdf)
  }

  private def _pdf_document_reference(json: Json, role: String, identity: String, locale: String, source: String): PdfDocumentReference = {
    val publicpath = _required_json_string(json, source, "public_path")
    val mediatype = _json_string(json, "media_type").getOrElse(
      throw new IllegalArgumentException(s"Missing article-media $role media_type: $source")
    )
    if (mediatype != "application/pdf")
      throw new IllegalArgumentException(s"Article-media $role media_type must be application/pdf: $identity [$locale]")
    val label = _json_field(json, "label").map { value =>
      value.asString.filter(_.trim.nonEmpty).getOrElse(
        throw new IllegalArgumentException(s"Article-media $role label must be nonblank: $identity [$locale]")
      )
    }
    PdfDocumentReference(
      publicPath = _site_visible_uri(publicpath, s"Article-media $role public_path for $identity [$locale]"),
      mediaType = mediatype,
      label = label
    )
  }

  private def _image_reference(json: Json, identity: String, locale: String, source: String): ImageReference = {
    val publicpath = _required_json_string(json, source, "public_path")
    ImageReference(
      publicPath = _site_visible_uri(publicpath, s"Article-media infographic public_path for $identity [$locale]"),
      mediaType = _optional_json_string(json, "media_type"),
      alt = _optional_json_string(json, "alt")
    )
  }

  private def _video_reference(json: Json, identity: String, locale: String, source: String): VideoReference = {
    val presentation = VideoPresentation.parse(_required_json_string(json, source, "presentation"))
    val status = VideoStatus.parse(_required_json_string(json, source, "status"))
    val provider = _optional_json_string(json, "provider")
    val watchurl = _optional_json_string(json, "watch_url").map(_absolute_uri(_, s"Article-media video watch_url for $identity [$locale]"))
    val contenturl = _optional_json_string(json, "content_url").map(_site_visible_uri(_, s"Article-media video content_url for $identity [$locale]"))
    if (status == VideoStatus.Published) {
      presentation match {
        case VideoPresentation.ExternalLink if watchurl.isEmpty =>
          throw new IllegalArgumentException(s"Published external article-media video requires watch_url: $identity [$locale]")
        case VideoPresentation.SiteHosted if contenturl.isEmpty =>
          throw new IllegalArgumentException(s"Published site-hosted article-media video requires content_url: $identity [$locale]")
        case _ =>
      }
    }
    VideoReference(presentation, status, provider, watchurl, contenturl)
  }

  private def _legacy_article_identity(video: VideoPublication): Option[String] =
    video.articlePath.flatMap { rawpath =>
      val path = rawpath.trim.replace('\\', '/')
      if (path == "index.dox")
        video.sourcePackage.flatMap { sourcepackage =>
          val normalized = _normalize_article_identity_option(sourcepackage)
          normalized.filter(_.endsWith(".video")).flatMap { x =>
            _normalize_article_identity_option(x.stripSuffix(".video"))
          }
        }
      else if (path.endsWith(".dox"))
        _normalize_article_identity_option(path.stripSuffix(".dox"))
      else
        None
    }

  private def _normalize_article_identity_option(value: String): Option[String] =
    try {
      Some(_normalize_article_identity(value))
    } catch {
      case _: IllegalArgumentException => None
    }

  private def _normalize_article_identity(value: String): String = {
    val normalized = value.trim.replace('\\', '/')
    if (normalized.isEmpty || normalized.startsWith("/"))
      throw new IllegalArgumentException(s"Invalid article-media article identity: $value")
    val segments = normalized.split("/").toVector.filter(_.nonEmpty)
    if (segments.isEmpty || segments.contains(".") || segments.contains("..") || segments.exists(segment => !_is_valid_path_segment(segment)))
      throw new IllegalArgumentException(s"Invalid article-media article identity: $value")
    if (segments.headOption.exists(_is_locale_prefix))
      throw new IllegalArgumentException(s"Article-media article identity must not have a locale prefix: $value")
    val identity = segments.mkString("/")
    if (identity.endsWith(".dox") || identity.endsWith(".html"))
      throw new IllegalArgumentException(s"Article-media article identity must not have a generated suffix: $value")
    identity
  }

  private def _is_locale_prefix(value: String): Boolean =
    _normalize_locale_option(value).exists { localetag =>
      val language = java.util.Locale.forLanguageTag(localetag).getLanguage
      language == "en" || language == "ja"
    }

  private def _normalize_locale_option(value: String): Option[String] =
    try {
      Some(_normalize_locale(value, requirecanonical = false))
    } catch {
      case _: IllegalArgumentException => None
    }

  private def _normalize_locale(value: String, requirecanonical: Boolean): String = {
    val raw = value
    if (raw.isEmpty || raw != raw.trim)
      throw new IllegalArgumentException(s"Invalid article-media locale: $value")
    val canonical =
      try {
        new java.util.Locale.Builder().setLanguageTag(raw).build().toLanguageTag
      } catch {
        case e: java.util.IllformedLocaleException =>
          throw new IllegalArgumentException(s"Invalid article-media locale: $value", e)
      }
    if (canonical == "und" || canonical.isEmpty || (requirecanonical && raw != canonical) || (!requirecanonical && !raw.equalsIgnoreCase(canonical)))
      throw new IllegalArgumentException(s"Article-media locale must be canonical: $value")
    canonical
  }

  private def _site_visible_uri_option(value: String): Option[URI] =
    try {
      Some(_site_visible_uri(value, "Article-media compatibility content URL"))
    } catch {
      case _: IllegalArgumentException => None
    }

  private def _site_visible_uri(value: String, label: String): URI = {
    val uri = _uri(value, label)
    val path = Option(uri.getPath).getOrElse("")
    if (uri.isAbsolute || uri.getAuthority != null || !path.startsWith("/") || path == "/" || path.split("/").contains("..") || path.split("/").contains("."))
      throw new IllegalArgumentException(s"$label must be a site-visible path: $value")
    uri
  }

  private def _absolute_uri(value: String, label: String): URI = {
    val uri = _uri(value, label)
    if (!uri.isAbsolute)
      throw new IllegalArgumentException(s"$label must be an absolute URI: $value")
    uri
  }

  private def _uri(value: String, label: String): URI =
    try {
      new URI(value.trim)
    } catch {
      case e: Exception => throw new IllegalArgumentException(s"Invalid $label: $value", e)
    }

  private def _required_json_string(json: Json, source: String, path: String*): String =
    _json_string(json, path: _*).map(_.trim).filter(_.nonEmpty).getOrElse(
      throw new IllegalArgumentException(s"Missing article-media ${path.mkString(".")}: $source")
    )

  private def _optional_json_string(json: Json, path: String*): Option[String] =
    _json_string(json, path: _*).map(_.trim).filter(_.nonEmpty)

  private def _required_json_object(json: Json, source: String, path: String*): io.circe.JsonObject =
    _json_field(json, path: _*).flatMap(_.asObject).getOrElse(
      throw new IllegalArgumentException(s"Missing article-media object ${path.mkString(".")}: $source")
    )

  private def _json_field(json: Json, path: String*): Option[Json] =
    path.foldLeft(Option(json)) {
      case (Some(z), segment) => z.hcursor.downField(segment).focus
      case (None, _) => None
    }

  private def _json_string(json: Json, path: String*): Option[String] =
    _json_field(json, path: _*).flatMap(_.asString)

  private def _is_valid_path_segment(s: String): Boolean =
    s.matches("""[A-Za-z0-9._-]+""") && s != "." && s != ".."
}
