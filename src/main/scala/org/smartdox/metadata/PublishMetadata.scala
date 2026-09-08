package org.smartdox.metadata

import java.io.File
import java.net.URI
import io.circe.Json
import org.goldenport.realm.Realm
import org.goldenport.value.DescriptiveAttributes
import org.smartdox._
import org.smartdox.doxsite.Page
import org.smartdox.semanticweb.Rdf

/*
 * @since   May. 13, 2026
 *  version May. 14, 2026
 *  version Jun. 24, 2026
 *  version Aug. 29, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
case class PublishMetadata(
  entries: Vector[PublishMetadata.Entry]
) {
  import PublishMetadata._

  def videoPublications: Vector[VideoPublication] =
    PublishMetadataVideoRdfSupport.videoPublications(entries)

  lazy val articleMedia: ArticleMediaRegistry =
    ArticleMediaRegistry.create(entries, videoPublications)

  lazy val articleMediaProjection: ArticleMediaProjection =
    ArticleMediaProjection(articleMedia)

  def resolveArticleMedia(articleIdentity: String, locale: String): Option[ArticleMediaVariant] =
    articleMedia.resolve(articleIdentity, locale)

  def videoRdfArtifactTriples(config: RdfMergeConfig): Vector[Rdf.Triple] =
    PublishMetadataVideoRdfSupport.videoRdfArtifactTriples(entries, config)

  def generatedPages: Vector[(String, Page)] =
    PublishMetadataCatalogPageSupport.generatedPages(this)

  case class Group(
    name: String,
    entries: Vector[Entry]
  ) {
    lazy val title: String = entries.flatMap(_.titleOption).headOption.getOrElse(name)
    lazy val pageDefinition: Option[PageDefinition] = pageDefinitions.find(_.path == userBasePath)
    lazy val userTitleText: DescriptiveAttributes.Text =
      pageDefinition.map(_.title).filter(_.nonEmpty).getOrElse(DescriptiveAttributes.Text(Some(if (name == "textus-tutorial") "Textus Tutorial" else title)))
    lazy val userTitleString: String =
      userTitleText.default.getOrElse(if (name == "textus-tutorial") "Textus Tutorial" else title)
    lazy val types: Vector[String] = entries.flatMap(_.typeOption).distinct
    lazy val basePath: String =
      entries.flatMap(_.publicationPath).headOption.getOrElse(s"repository/${_path_segment(name)}")
    lazy val userBasePath: String =
      basePath
    lazy val adminPath: String =
      s"catalog/${_path_segment(name)}"
    lazy val summaryText: DescriptiveAttributes.Text =
      pageDefinition.map(_.descriptive.summary).filter(_.nonEmpty).
        getOrElse(DescriptiveAttributes.Text(entries.flatMap(_.summaryOption).headOption.orElse(Some("Published resource."))))
    lazy val descriptionText: DescriptiveAttributes.Text =
      pageDefinition.map(_.descriptive.description).filter(_.nonEmpty).
        getOrElse(DescriptiveAttributes.Text(entries.flatMap(_.descriptionOption).headOption))
    lazy val summary: String = summaryText.default.getOrElse("Published resource.")
    lazy val description: Option[String] = descriptionText.default
    lazy val version: String =
      entries.flatMap(_.versionOption).headOption.getOrElse("")
    lazy val kind: String =
      entries.flatMap(_.kindOption).headOption.getOrElse("")
    lazy val pageDefinitions: Vector[PageDefinition] =
      entries.flatMap(_.pageDefinitions)
    lazy val sampleRefs: Vector[SampleRef] =
      _distinct_samples(entries.flatMap(_.sampleRefs))

    def pages: Vector[(String, Page)] =
      PublishMetadataCatalogPageSupport.pages(this)
  }
}

object PublishMetadata {
  /**
   * Resolves publication-owned article media from a source-tree pathname.
   *
   * Callers provide the pre-locale source path. Locale output directories are
   * deliberately not reverse-engineered here: article identities are source
   * identities, not generated paths.
   */
  case class ArticleMediaProjection(
    registry: ArticleMediaRegistry
  ) {
    def resolve(sourcePath: String, locale: java.util.Locale): Option[ArticleMediaVariant] =
      resolve(sourcePath, locale.toLanguageTag)

    def resolve(sourcePath: String, locale: String): Option[ArticleMediaVariant] =
      PublishMetadata.sourcePathToArticleIdentity(sourcePath).flatMap(registry.resolve(_, locale))
  }

  /**
   * Converts a site-relative source or generated article pathname to the
   * registry identity. It never removes a leading path segment as a guessed
   * locale; source callers must pass a pre-locale path.
   */
  def sourcePathToArticleIdentity(sourcePath: String): Option[String] =
    PublishMetadataArticleMediaSupport.sourcePathToArticleIdentity(sourcePath)

  case class ArticleMediaPublication(
    articleIdentity: String,
    variants: Vector[ArticleMediaVariant]
  )

  case class ArticleMediaVariant(
    locale: String,
    infographic: Option[ImageReference] = None,
    video: Option[VideoReference] = None,
    articlePdf: Option[PdfDocumentReference] = None,
    summarySlidesPdf: Option[PdfDocumentReference] = None
  ) {
    def projectableVideo: Option[VideoReference] =
      video.filter(_.isProjectable)
  }

  case class PdfDocumentReference(
    publicPath: URI,
    mediaType: String,
    label: Option[String] = None
  )

  case class ImageReference(
    publicPath: URI,
    mediaType: Option[String] = None,
    alt: Option[String] = None
  )

  sealed trait VideoPresentation {
    def name: String
  }

  object VideoPresentation {
    case object ExternalLink extends VideoPresentation {
      val name = "external-link"
    }

    case object SiteHosted extends VideoPresentation {
      val name = "site-hosted"
    }

    def parse(value: String): VideoPresentation =
      value match {
        case ExternalLink.name => ExternalLink
        case SiteHosted.name => SiteHosted
        case _ => throw new IllegalArgumentException(s"Unsupported article-media video presentation: $value")
      }
  }

  sealed trait VideoStatus {
    def name: String
  }

  object VideoStatus {
    case object Draft extends VideoStatus {
      val name = "draft"
    }

    case object Published extends VideoStatus {
      val name = "published"
    }

    case object Withdrawn extends VideoStatus {
      val name = "withdrawn"
    }

    def parse(value: String): VideoStatus =
      value match {
        case Draft.name => Draft
        case Published.name => Published
        case Withdrawn.name => Withdrawn
        case _ => throw new IllegalArgumentException(s"Unsupported article-media video status: $value")
      }
  }

  case class VideoReference(
    presentation: VideoPresentation,
    status: VideoStatus,
    provider: Option[String] = None,
    watchUrl: Option[URI] = None,
    contentUrl: Option[URI] = None
  ) {
    def isProjectable: Boolean =
      status == VideoStatus.Published && (presentation match {
        case VideoPresentation.ExternalLink => watchUrl.nonEmpty
        case VideoPresentation.SiteHosted => contentUrl.nonEmpty
      })
  }

  case class ArticleMediaDiagnostic(
    code: String,
    message: String
  )

  case class ArticleMediaRegistry(
    publications: Vector[ArticleMediaPublication],
    compatibilityVariants: Map[String, ArticleMediaVariant],
    diagnostics: Vector[ArticleMediaDiagnostic]
  ) {
    private lazy val _variants: Map[(String, String), ArticleMediaVariant] =
      publications.flatMap { publication =>
        publication.variants.map(variant => (publication.articleIdentity -> variant.locale) -> variant)
      }.toMap

    def resolve(articleIdentity: String, locale: String): Option[ArticleMediaVariant] =
      PublishMetadataArticleMediaSupport.resolveArticleMedia(_variants, compatibilityVariants, articleIdentity, locale)
  }

  object ArticleMediaRegistry {
    def create(entries: Vector[Entry], videos: Vector[VideoPublication]): ArticleMediaRegistry =
      PublishMetadataArticleMediaSupport.createArticleMediaRegistry(entries, videos)
  }

  case class RdfMergeConfig(
    repository: Option[File],
    missingArtifactPolicy: String = "warn",
    mergePublicationArtifacts: Boolean = true
  ) {
    def resolve(artifact: VideoArtifactReference): Option[File] =
      PublishMetadataVideoRdfSupport.resolve(repository, artifact)

    def handleMissing(message: String): Vector[Rdf.Triple] =
      PublishMetadataVideoRdfSupport.handleMissing(missingArtifactPolicy, message)
  }

  case class PageDefinition(
    path: String,
    title: DescriptiveAttributes.Text,
    descriptive: DescriptiveAttributes
  )

  case class VideoArtifactReference(
    kind: String,
    publicPath: String,
    warehousePath: Option[String] = None,
    sha256: Option[String] = None
  )

  case class VideoRdfPublication(
    name: String,
    version: String,
    registryPath: String,
    turtle: Option[VideoArtifactReference] = None,
    jsonLd: Option[VideoArtifactReference] = None,
    manifest: Option[VideoArtifactReference] = None
  )

  case class VideoPublication(
    name: String,
    version: String,
    sourcePackage: Option[String],
    articlePath: Option[String],
    publicPath: String,
    artifact: Option[VideoArtifactReference] = None,
    caption: Option[VideoArtifactReference] = None,
    transcript: Option[VideoArtifactReference] = None,
    rdf: Option[VideoRdfPublication] = None
  ) {
    def matchesPackage(directory: String): Boolean =
      sourcePackage.map(_normalize_path).contains(_normalize_path(directory))
  }

  private def _normalize_path(path: String): String =
    path.trim.replace('\\', '/').stripPrefix("/")

  case class Entry(
    path: String,
    key: String,
    json: Json
  ) {
    def logicalPath: String = _logical_path(path)
    def logicalKey: String = _logical_path(key)
    def schemaOption: Option[String] = string("schema")
    def typeOption: Option[String] = string("type")
    def publicationPath: Option[String] =
      string("publication", "path").map(_.trim).filter(_.nonEmpty).map(_validate_path)
    def identity: String =
      string("project", "name").
        orElse(string("publication", "name")).
        orElse(string("name")).
        orElse(string("artifact", "name")).
        getOrElse(_path_segment(key.split("/").lastOption.getOrElse("metadata")))
    def titleOption: Option[String] =
      string("project", "title").orElse(string("sample", "title")).orElse(string("publication", "title")).orElse(string("title"))
    def summaryOption: Option[String] =
      string("project", "summary").orElse(string("sample", "summary")).orElse(string("summary")).
        map(_.trim).filter(_.nonEmpty).filterNot(_ == ">")
    def descriptionOption: Option[String] =
      string("project", "description").orElse(string("sample", "description")).orElse(string("description")).
        map(_.trim).filter(_.nonEmpty).filterNot(_ == ">")
    def versionOption: Option[String] =
      string("project", "version").orElse(string("sample", "version")).orElse(string("release", "version"))
    def kindOption: Option[String] =
      string("project", "kind").orElse(string("sample", "kind")).orElse(string("kind"))
    def artifactFiles: Vector[Json] =
      array("artifact", "files")
    def sourceFiles: Vector[Json] =
      array("files") ++ array("source_manifest")
    def sampleRefs: Vector[SampleRef] =
      array("samples").flatMap(SampleRef.fromJson)
    def videoPublication: Option[VideoPublication] =
      PublishMetadataVideoRdfSupport.videoPublication(this)

    def videoRdfPublication: Option[VideoRdfPublication] =
      PublishMetadataVideoRdfSupport.videoRdfPublication(this)

    def articleMediaPublication: Option[ArticleMediaPublication] =
      if (typeOption.contains("article-media-publication"))
        Some(PublishMetadataArticleMediaSupport.parseArticleMediaPublication(json, path))
      else
        None

    def displayTitle: String =
      List(typeOption, Some(path)).flatten.mkString(" - ")
    def isSourceManifest: Boolean =
      logicalKey.startsWith("source-manifest/") || typeOption.exists(_.contains("source"))
    def isArtifact: Boolean =
      logicalKey.startsWith("artifacts/") || logicalKey.startsWith("repository/") || logicalKey.startsWith("maven/") ||
        typeOption.exists(x => x.contains("artifact") || x.contains("maven"))
    def isRelease: Boolean =
      logicalKey.startsWith("releases/") || typeOption.exists(_.contains("release"))
    def isPublicationPages: Boolean =
      logicalKey.startsWith("publication-pages/") || typeOption.contains("publication-pages")
    def pageDefinitions: Vector[PageDefinition] =
      if (isPublicationPages)
        array("pages").flatMap { page =>
          _json_string(page, "path").map { p =>
            PageDefinition(
              _validate_path(p),
              DescriptiveAttributes.textFromJson(page, "title"),
              DescriptiveAttributes.fromJson(page)
            )
          }
        }
      else
        Vector.empty

    def string(path: String*): Option[String] =
      field(path: _*).flatMap(_.asString)

    def field(path: String*): Option[Json] =
      path.foldLeft(Option(json)) {
        case (Some(z), x) => z.hcursor.downField(x).focus
        case (None, _) => None
      }

    def array(path: String*): Vector[Json] =
      field(path: _*).flatMap(_.asArray).getOrElse(Vector.empty)

    def artifactReference(kind: String, path: String*): Option[VideoArtifactReference] =
      PublishMetadataVideoRdfSupport.artifactReference(this, kind, path: _*)

    def projectRows: Vector[(String, String)] = {
      val keys = Vector("kind", "version", "scala_version", "scalaVersion", "sbt_version", "sbtVersion")
      keys.flatMap { key =>
        string("project", key).map(key -> _)
      }
    }
  }

  case class SampleRef(
    name: String,
    title: DescriptiveAttributes.Text,
    summaryText: DescriptiveAttributes.Text,
    descriptionText: DescriptiveAttributes.Text,
    directory: Option[String],
    version: Option[String]
  ) {
    def displayTitle: String =
      name

    def section: Section =
      PublishMetadataCatalogPageSupport.sampleSection(this)
  }

  private def _distinct_samples(samples: Vector[SampleRef]): Vector[SampleRef] = {
    case class Z(
      seen: Set[String] = Set.empty,
      samples: Vector[SampleRef] = Vector.empty
    ) {
      def +(rhs: SampleRef): Z =
        if (seen.contains(rhs.name))
          this
        else
          Z(seen + rhs.name, samples :+ rhs)
    }
    samples.foldLeft(Z())(_+_).samples
  }

  object SampleRef {
    def fromJson(json: Json): Option[SampleRef] =
      _json_string(json, "name").map { name =>
        val descriptive = DescriptiveAttributes.fromJson(json)
        SampleRef(
          name = name,
          title = DescriptiveAttributes.textFromJson(json, "title"),
          summaryText = descriptive.summary,
          descriptionText = descriptive.description,
          directory = _json_string(json, "directory"),
          version = _json_string(json, "version")
        )
    }
  }

  def load(publish: Option[File]): Option[PublishMetadata] =
    PublishMetadataRegistrySupport.load(publish)

  def load(publish: File): Option[PublishMetadata] =
    PublishMetadataRegistrySupport.load(publish)

  def publicRealm(publish: Option[File]): Option[Realm] =
    PublishMetadataRegistrySupport.publicRealm(publish)

  private def _logical_path(path: String): String =
    if (path.startsWith("metadata/"))
      path.substring("metadata/".length)
    else
      path

  private def _validate_path(path: String): String = {
    val segments = path.split("/").toVector.filter(_.nonEmpty)
    if (segments.isEmpty)
      throw new IllegalArgumentException("publication.path must not be empty")
    segments.foreach { x =>
      if (!_is_valid_path_segment(x))
        throw new IllegalArgumentException(s"Invalid publication.path segment: $path")
    }
    segments.mkString("/")
  }

  private def _is_valid_path_segment(s: String): Boolean =
    s.matches("""[A-Za-z0-9._-]+""") && s != "." && s != ".."

  private def _path_segment(s: String): String = {
    val a = s.trim.replaceAll("""[^A-Za-z0-9._-]+""", "-")
    if (a.isEmpty) "metadata" else a
  }

  private def _json_string(json: Json, path: String*): Option[String] =
    path.foldLeft(Option(json)) {
      case (Some(z), x) => z.hcursor.downField(x).focus
      case (None, _) => None
    }.flatMap(_.asString)
}
