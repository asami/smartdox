package org.smartdox.metadata

import java.io.File
import java.nio.charset.StandardCharsets

import io.circe.Json
import org.smartdox.semanticweb.Rdf

import PublishMetadata._

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
private[metadata] object PublishMetadataVideoRdfSupport {
  def videoPublications(entries: Vector[Entry]): Vector[VideoPublication] = {
    val rdfs = entries.flatMap(_.videoRdfPublication)
    entries.flatMap(_.videoPublication).map { video =>
      val rdf = rdfs.find(x => x.name == video.name && x.version == video.version)
      video.copy(rdf = rdf)
    }
  }

  def videoRdfArtifactTriples(entries: Vector[Entry], config: RdfMergeConfig): Vector[Rdf.Triple] =
    if (config.mergePublicationArtifacts)
      videoPublications(entries).flatMap(_.rdf).flatMap(_.turtle).flatMap(_load_turtle_triples(config, _))
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

  def resolve(repository: Option[File], artifact: VideoArtifactReference): Option[File] =
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

  def handleMissing(missingArtifactPolicy: String, message: String): Vector[Rdf.Triple] =
    missingArtifactPolicy match {
      case "fail" | "error" => throw new IllegalArgumentException(message)
      case _ => Vector.empty
    }

  def videoPublication(entry: Entry): Option[VideoPublication] =
    if (entry.typeOption.contains("video-publication")) {
      val artifact = entry.artifactReference("video", "video", "artifact")
      for {
        name <- entry.string("video", "name")
        publicpath <- artifact.map(_.publicPath).
          orElse(entry.string("video", "artifact", "repositoryPublicPath").map(_public_path)).
          orElse(entry.string("video", "artifact", "publicPath").map(_public_path)).
          orElse(entry.string("video", "publish", "publicPath").map(_public_path))
      } yield VideoPublication(
        name = name,
        version = entry.string("video", "version").getOrElse(""),
        sourcePackage = entry.string("video", "sourcePackage"),
        articlePath = entry.string("video", "articlePath"),
        publicPath = publicpath,
        artifact = artifact,
        caption = entry.artifactReference("captions", "video", "captions").orElse(entry.artifactReference("captions", "video", "caption")),
        transcript = entry.artifactReference("transcript", "video", "transcript")
      )
    } else {
      None
    }

  def videoRdfPublication(entry: Entry): Option[VideoRdfPublication] =
    if (entry.typeOption.contains("video-rdf"))
      entry.string("video", "name").map { name =>
        VideoRdfPublication(
          name = name,
          version = entry.string("video", "version").getOrElse(""),
          registryPath = entry.string("registryPath").getOrElse(entry.path.stripSuffix(".json")),
          turtle = entry.artifactReference("turtle", "files", "turtle"),
          jsonLd = entry.artifactReference("jsonld", "files", "jsonLd"),
          manifest = entry.artifactReference("rdf-manifest", "files", "manifest")
        )
      }
    else
      None

  def artifactReference(entry: Entry, kind: String, path: String*): Option[VideoArtifactReference] =
    entry.field(path: _*).flatMap { json =>
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

  private def _json_string(json: Json, path: String*): Option[String] =
    path.foldLeft(Option(json)) {
      case (Some(z), x) => z.hcursor.downField(x).focus
      case (None, _) => None
    }.flatMap(_.asString)

  private def _normalize_path(path: String): String =
    path.trim.replace('\\', '/').stripPrefix("/")

  private def _public_path(path: String): String = {
    val normalized = _normalize_path(path)
    if (normalized.startsWith("repository/"))
      s"/$normalized"
    else
      normalized
  }
}
