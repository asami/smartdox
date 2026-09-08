package org.smartdox.metadata

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
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

  def videoPublications: Vector[VideoPublication] = {
    val rdfs = entries.flatMap(_.videoRdfPublication)
    entries.flatMap(_.videoPublication).map { video =>
      val rdf = rdfs.find(x => x.name == video.name && x.version == video.version)
      video.copy(rdf = rdf)
    }
  }

  lazy val articleMedia: ArticleMediaRegistry =
    ArticleMediaRegistry.create(entries, videoPublications)

  lazy val articleMediaProjection: ArticleMediaProjection =
    ArticleMediaProjection(articleMedia)

  def resolveArticleMedia(articleIdentity: String, locale: String): Option[ArticleMediaVariant] =
    articleMedia.resolve(articleIdentity, locale)

  def videoRdfArtifactTriples(config: RdfMergeConfig): Vector[Rdf.Triple] =
    if (config.mergePublicationArtifacts)
      videoPublications.flatMap(_.rdf).flatMap(_.turtle).flatMap(_load_turtle_triples(config, _))
    else
      Vector.empty

  private def _load_turtle_triples(config: RdfMergeConfig, artifact: VideoArtifactReference): Vector[Rdf.Triple] =
    config.resolve(artifact) match {
      case Some(file) if file.isFile =>
        TurtleSubsetParser.parse(new String(java.nio.file.Files.readAllBytes(file.toPath), StandardCharsets.UTF_8), file.getPath)
      case Some(file) =>
        config.handleMissing(s"Missing RDF artifact: ${file.getPath}")
      case None =>
        config.handleMissing(s"Cannot resolve RDF artifact without repository root: ${artifact.warehousePath.getOrElse(artifact.publicPath)}")
    }

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
      repository.map { root =>
        val path = artifact.warehousePath.getOrElse(artifact.publicPath).stripPrefix("/")
        val rootfile = root.getCanonicalFile
        val candidates = _repository_candidates(rootfile, path)
        val artifactfile = candidates.find(_.isFile).getOrElse(candidates.head)
        val rootpath = rootfile.toPath
        val artifactpath = artifactfile.toPath
        if (!artifactpath.startsWith(rootpath))
          throw new IllegalArgumentException(s"RDF artifact is outside repository root: ${artifact.warehousePath.getOrElse(artifact.publicPath)}")
        artifactfile
      }

    private def _repository_candidates(rootfile: File, path: String): Vector[File] = {
      val primary = new File(rootfile, path).getCanonicalFile
      if (path == "repository")
        Vector(primary)
      else if (path.startsWith("repository/")) {
        val stripped = new File(rootfile, path.stripPrefix("repository/")).getCanonicalFile
        Vector(primary, stripped).distinct
      } else {
        Vector(primary)
      }
    }

    def handleMissing(message: String): Vector[Rdf.Triple] =
      missingArtifactPolicy match {
        case "fail" | "error" => throw new IllegalArgumentException(message)
        case _ => Vector.empty
      }
  }


  private object TurtleSubsetParser {
    private val _prefix_pattern = """@prefix\s+([A-Za-z][A-Za-z0-9_-]*):\s+<([^>]+)>\s*\.""".r
    private val _builtin_prefixes = Map(
      "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
      "schema" -> "https://schema.org/",
      "dcterms" -> "http://purl.org/dc/terms/",
      "cozy-video" -> "https://www.simplemodeling.org/ns/cozy/video#"
    )

    def parse(text: String, source: String): Vector[Rdf.Triple] = {
      val lines = text.linesIterator.map(_.trim).filter(x => x.nonEmpty && !x.startsWith("#")).toVector
      val prefixes = lines.collect {
        case _prefix_pattern(prefix, iri) => prefix -> iri
      }.toMap ++ _builtin_prefixes
      _statement_lines(lines).flatMap(_parse_statement(prefixes, source, _))
    }

    private def _statement_lines(lines: Vector[String]): Vector[String] = {
      val statements = Vector.newBuilder[String]
      val buffer = StringBuilder.newBuilder
      lines.foreach {
        case _prefix_pattern(_, _) =>
          statements += buffer.toString.trim
          buffer.clear()
        case line =>
          if (buffer.nonEmpty)
            buffer.append(" ")
          buffer.append(line)
          if (line.endsWith(".")) {
            statements += buffer.toString.trim
            buffer.clear()
          }
      }
      val rest = buffer.toString.trim
      if (rest.nonEmpty)
        statements += rest
      statements.result().filter(_.nonEmpty)
    }

    private def _parse_statement(prefixes: Map[String, String], source: String, statement: String): Vector[Rdf.Triple] =
      statement match {
        case _prefix_pattern(_, _) => Vector.empty
        case x if x.endsWith(".") =>
          _split_subject_predicates(x.dropRight(1).trim).map {
            case (s, predicates) =>
              predicates.flatMap {
                case (p, os) =>
                  os.map(o => Rdf.Triple(_iri(prefixes, s), _predicate(prefixes, p), _node(prefixes, o)))
              }
          }.getOrElse(Vector.empty)
        case _ => Vector.empty
      }

    private def _split_subject_predicates(value: String): Option[(String, Vector[(String, Vector[String])])] = {
      val p = value.indexOf(' ')
      if (p < 0)
        None
      else {
        val subject = value.substring(0, p).trim
        val rest = value.substring(p + 1).trim
        val predicates = _split_top_level(rest, ';').flatMap(_split_predicate_objects)
        Some(subject -> predicates)
      }
    }

    private def _split_predicate_objects(value: String): Option[(String, Vector[String])] = {
      val p = value.indexOf(' ')
      if (p < 0)
        None
      else {
        val predicate = value.substring(0, p).trim
        val objects = _split_top_level(value.substring(p + 1).trim, ',')
        if (predicate.isEmpty || objects.isEmpty)
          None
        else
          Some(predicate -> objects)
      }
    }

    private def _split_top_level(value: String, delimiter: Char): Vector[String] = {
      val builder = Vector.newBuilder[String]
      val buffer = StringBuilder.newBuilder
      var inliteral = false
      var escaped = false
      value.foreach { c =>
        if (escaped) {
          buffer.append(c)
          escaped = false
        } else if (c == '\\') {
          buffer.append(c)
          escaped = true
        } else if (c == '"') {
          buffer.append(c)
          inliteral = !inliteral
        } else if (c == delimiter && !inliteral) {
          val item = buffer.toString.trim
          if (item.nonEmpty)
            builder += item
          buffer.clear()
        } else {
          buffer.append(c)
        }
      }
      val rest = buffer.toString.trim
      if (rest.nonEmpty)
        builder += rest
      builder.result()
    }

    private def _predicate(prefixes: Map[String, String], value: String): Rdf.Node.Uri =
      if (value == "a")
        Rdf.Node.Uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      else
        _iri(prefixes, value)

    private def _node(prefixes: Map[String, String], value: String): Rdf.Node =
      if (value.startsWith("\""))
        Rdf.Node.Literal(_literal(value))
      else
        Rdf.Node.Uri(_iri(prefixes, value).value)

    private def _iri(prefixes: Map[String, String], value: String): Rdf.Node.Uri =
      if (value.startsWith("<") && value.endsWith(">"))
        Rdf.Node.Uri(value.substring(1, value.length - 1))
      else {
        val p = value.indexOf(':')
        if (p <= 0)
          Rdf.Node.Uri(value)
        else {
          val prefix = value.substring(0, p)
          val local = value.substring(p + 1)
          Rdf.Node.Uri(prefixes.getOrElse(prefix, prefix + ":") + local)
        }
      }

    private def _literal(value: String): String = {
      val body = value.drop(1)
      val end = body.lastIndexOf('"')
      if (end < 0)
        body
      else
        body.substring(0, end).replace("\\\"", "\"").replace("\\n", "\n")
    }
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

  private def _public_path(path: String): String = {
    val normalized = _normalize_path(path)
    if (normalized.startsWith("repository/"))
      s"/$normalized"
    else
      normalized
  }

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
      if (typeOption.contains("video-publication")) {
        val artifact = artifactReference("video", "video", "artifact")
        for {
          name <- string("video", "name")
          publicpath <- artifact.map(_.publicPath).
            orElse(string("video", "artifact", "repositoryPublicPath").map(_public_path)).
            orElse(string("video", "artifact", "publicPath").map(_public_path)).
            orElse(string("video", "publish", "publicPath").map(_public_path))
        } yield VideoPublication(
          name = name,
          version = string("video", "version").getOrElse(""),
          sourcePackage = string("video", "sourcePackage"),
          articlePath = string("video", "articlePath"),
          publicPath = publicpath,
          artifact = artifact,
          caption = artifactReference("captions", "video", "captions").orElse(artifactReference("captions", "video", "caption")),
          transcript = artifactReference("transcript", "video", "transcript")
        )
      } else {
        None
      }

    def videoRdfPublication: Option[VideoRdfPublication] =
      if (typeOption.contains("video-rdf"))
        string("video", "name").map { name =>
          VideoRdfPublication(
            name = name,
            version = string("video", "version").getOrElse(""),
            registryPath = string("registryPath").getOrElse(path.stripSuffix(".json")),
            turtle = artifactReference("turtle", "files", "turtle"),
            jsonLd = artifactReference("jsonld", "files", "jsonLd"),
            manifest = artifactReference("rdf-manifest", "files", "manifest")
          )
        }
      else
        None

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
      field(path: _*).flatMap { json =>
        _json_string(json, "repositoryPublicPath").orElse(_json_string(json, "publicPath")).orElse(_json_string(json, "warehousePath")).
          map { publicpath =>
            VideoArtifactReference(
              _json_string(json, "type").getOrElse(kind),
              _public_path(publicpath),
              _json_string(json, "warehousePath"),
              _json_string(json, "sha256")
            )
          }
      }

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
