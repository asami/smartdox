package org.smartdox.metadata

import java.io.File
import io.circe.Json
import io.circe.parser
import org.goldenport.realm.Realm
import org.goldenport.config.ConfigLoader
import org.goldenport.io.InputSource
import org.smartdox._
import org.smartdox.doxsite.Page

/*
 * @since   May. 13, 2026
 * @version May. 13, 2026
 * @author  ASAMI, Tomoharu
 */
case class PublishMetadata(
  entries: Vector[PublishMetadata.Entry]
) {
  import PublishMetadata._

  def generatedPages: Vector[(String, Page)] =
    _catalogPage +: _groups.flatMap(_.pages)

  private def _catalogPage: (String, Page) = {
    val table = Table.Builder.headerString("Name", "Title", "Types", "Path", "Files")
    _groups.foreach { g =>
      table.appendString(List(
        g.name,
        g.title,
        g.types.mkString(", "),
        g.basePath,
        g.entries.size.toString
      ))
    }
    "catalog/index.dox" -> _page("index.dox", "Publish Catalog", List(
      Paragraph.text("Generated publication metadata catalog."),
      table()
    ))
  }

  private lazy val _groups: Vector[Group] =
    entries.groupBy(_.identity).toVector.sortBy(_._1).map {
      case (name, xs) => Group(name, xs.sortBy(_.path))
    }

  case class Group(
    name: String,
    entries: Vector[Entry]
  ) {
    lazy val title: String = entries.flatMap(_.titleOption).headOption.getOrElse(name)
    lazy val types: Vector[String] = entries.flatMap(_.typeOption).distinct
    lazy val basePath: String =
      entries.flatMap(_.publicationPath).headOption.getOrElse(s"repository/${_path_segment(name)}")

    def pages: Vector[(String, Page)] = {
      val index = s"$basePath/index.dox" -> _page("index.dox", title, _indexContents)
      val metadata = s"$basePath/metadata.dox" -> _page("metadata.dox", s"$title Metadata", _metadataContents)
      val source = _typedPage("source-manifest.dox", s"$title Source Manifest", entries.filter(_.isSourceManifest))
      val artifacts = _typedPage("artifacts.dox", s"$title Artifacts", entries.filter(_.isArtifact))
      val releases = _typedPage("releases.dox", s"$title Release History", entries.filter(_.isRelease))
      Vector(Some(index), source, artifacts, releases, Some(metadata)).flatten
    }

    private def _indexContents: List[Dox] = {
      val tb = Table.Builder.headerString("Property", "Value")
      tb.append("Name", name)
      tb.append("Title", title)
      tb.append("Types", types.mkString(", "))
      tb.append("Publication Path", basePath)
      entries.headOption.foreach { entry =>
        entry.projectRows.foreach {
          case (k, v) => tb.append(k, v)
        }
      }
      List(
        Paragraph.text("Publication metadata generated from publish.d."),
        tb(),
        _entriesTable("Metadata Files", entries)
      )
    }

    private def _metadataContents: List[Dox] =
      List(
        Paragraph.text("Raw metadata files used to generate this page."),
        _entriesTable("Metadata Files", entries)
      )

    private def _typedPage(
      name: String,
      title: String,
      xs: Vector[Entry]
    ): Option[(String, Page)] =
      if (xs.isEmpty)
        None
      else
        Some(s"$basePath/$name" -> _page(name, title, _detailContents(xs)))

    private def _detailContents(xs: Vector[Entry]): List[Dox] =
      xs.toList.flatMap { entry =>
        List(
          Section(entry.displayTitle, List(_fieldTable(entry)))
        )
      }
  }
}

object PublishMetadata {
  case class Entry(
    path: String,
    key: String,
    json: Json
  ) {
    def schemaOption: Option[String] = string("schema")
    def typeOption: Option[String] = string("type")
    def publicationPath: Option[String] =
      string("publication", "path").map(_.trim).filter(_.nonEmpty).map(_validate_path)
    def identity: String =
      string("project", "name").
        orElse(string("name")).
        orElse(string("artifact", "name")).
        getOrElse(_path_segment(key.split("/").lastOption.getOrElse("metadata")))
    def titleOption: Option[String] =
      string("project", "title").orElse(string("title"))
    def displayTitle: String =
      List(typeOption, Some(path)).flatten.mkString(" - ")
    def isSourceManifest: Boolean =
      path.startsWith("source-manifest/") || typeOption.exists(_.contains("source"))
    def isArtifact: Boolean =
      path.startsWith("repository/") || path.startsWith("maven/") ||
        typeOption.exists(x => x.contains("artifact") || x.contains("maven"))
    def isRelease: Boolean =
      path.startsWith("releases/") || typeOption.exists(_.contains("release"))

    def string(path: String*): Option[String] =
      field(path: _*).flatMap(_.asString)

    def field(path: String*): Option[Json] =
      path.foldLeft(Option(json)) {
        case (Some(z), x) => z.hcursor.downField(x).focus
        case (None, _) => None
      }

    def projectRows: Vector[(String, String)] = {
      val keys = Vector("kind", "version", "scala_version", "scalaVersion", "sbt_version", "sbtVersion")
      keys.flatMap { key =>
        string("project", key).map(key -> _)
      }
    }
  }

  def load(publish: Option[File]): Option[PublishMetadata] =
    _directory(publish).flatMap(load)

  def load(publish: File): Option[PublishMetadata] = {
    val entries = _metadata_files(publish).map(_entry(publish, _))
    if (entries.isEmpty)
      None
    else
      Some(PublishMetadata(entries))
  }

  def rawRealm(publish: Option[File]): Option[Realm] =
    _directory(publish).map(Realm.create)

  private def _directory(publish: Option[File]): Option[File] =
    publish.filter(x => x.exists && x.isDirectory)

  private def _metadata_files(base: File): Vector[File] = {
    val files = _files(base).filter { x =>
      val n = x.getName.toLowerCase
      n.endsWith(".json") || n.endsWith(".yaml") || n.endsWith(".yml")
    }
    files.groupBy(x => _strip_suffix(_relative(base, x))).toVector.sortBy(_._1).flatMap {
      case (_, xs) => xs.sortBy(_priority).headOption
    }
  }

  private def _files(base: File): Vector[File] =
    Option(base.listFiles).toVector.flatten.toVector.flatMap { x =>
      if (x.isDirectory)
        _files(x)
      else
        Vector(x)
    }

  private def _entry(base: File, file: File): Entry = {
    val json = _parse(file)
    val path = _relative(base, file)
    Entry(path, _strip_suffix(path), json)
  }

  private def _parse(file: File): Json = {
    val in = InputSource(file)
    if (file.getName.toLowerCase.endsWith(".json"))
      parser.parse(in.asText) match {
        case Right(r) => r
        case Left(l) => throw new IllegalArgumentException(s"Invalid publish metadata JSON: ${file.getPath}: ${l.message}", l)
      }
    else
      ConfigLoader.loadConfigFromYaml[Json](in).fold(
        e => throw new IllegalArgumentException(s"Invalid publish metadata YAML: ${file.getPath}: ${e.message}"),
        identity
      )
  }

  private def _priority(file: File): Int =
    if (file.getName.toLowerCase.endsWith(".json")) 0 else 1

  private def _relative(base: File, file: File): String =
    base.toPath.relativize(file.toPath).toString.replace(File.separatorChar, '/')

  private def _strip_suffix(path: String): String =
    path.replaceFirst("""\.[^.]+$""", "")

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

  private def _page(name: String, title: String, contents: List[Dox]): Page = {
    val meta = DocumentMetaData.create(title, Explanation.empty)
    val dox = Document(Head(metadata = meta), Body(contents))
    Page(name, dox)
  }

  private def _entriesTable(caption: String, xs: Vector[Entry]): Table = {
    val tb = Table.Builder.captionHeaderString(caption, Vector("File", "Type", "Schema"))
    xs.foreach { x =>
      tb.appendString(Vector(
        x.path,
        x.typeOption.getOrElse(""),
        x.schemaOption.getOrElse("")
      ))
    }
    tb()
  }

  private def _fieldTable(entry: Entry): Table = {
    val tb = Table.Builder.headerString("Field", "Value")
    _flatten(entry.json).take(120).foreach {
      case (k, v) => tb.append(k, v)
    }
    tb()
  }

  private def _flatten(json: Json): Vector[(String, String)] =
    _flatten("", json)

  private def _flatten(prefix: String, json: Json): Vector[(String, String)] =
    json.asObject match {
      case Some(obj) =>
        obj.toVector.flatMap {
          case (k, v) => _flatten(_join(prefix, k), v)
        }
      case None =>
        json.asArray match {
          case Some(xs) =>
            if (xs.forall(x => x.asObject.isEmpty && x.asArray.isEmpty))
              Vector(prefix -> xs.map(_scalar).mkString(", "))
            else
              xs.zipWithIndex.toVector.flatMap {
                case (x, i) => _flatten(s"$prefix[$i]", x)
              }
          case None => Vector(prefix -> _scalar(json))
        }
    }

  private def _join(prefix: String, key: String): String =
    if (prefix.isEmpty) key else s"$prefix.$key"

  private def _scalar(json: Json): String =
    json.asString.
      orElse(json.asNumber.map(_.toString)).
      orElse(json.asBoolean.map(_.toString)).
      getOrElse(if (json.isNull) "" else json.noSpaces)
}
