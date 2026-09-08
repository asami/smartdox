package org.smartdox.metadata

import java.io.File

import io.circe.Json
import io.circe.parser
import org.goldenport.config.ConfigLoader
import org.goldenport.io.InputSource
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.StringData
import org.goldenport.values.PathName

private[metadata] object PublishMetadataRegistrySupport {
  def load(publish: Option[File]): Option[PublishMetadata] =
    _directory(publish).flatMap(load)

  def load(publish: File): Option[PublishMetadata] = {
    val entries = _bundle_entries(publish).getOrElse(_metadata_files(publish).map(_entry(publish, _)))
    if (entries.isEmpty)
      None
    else {
      val metadata = PublishMetadata(entries)
      metadata.articleMedia
      Some(metadata)
    }
  }

  def publicRealm(publish: Option[File]): Option[Realm] =
    _directory(publish).flatMap { base =>
      _bundle_entries(base).map { entries =>
        val builder = Realm.Builder()
        entries.foreach { entry =>
          builder.set(PathName(entry.path), StringData(entry.json.spaces2 + "\n"))
        }
        builder.build()
      }
    }

  private def _directory(publish: Option[File]): Option[File] =
    publish.filter(x => x.exists && x.isDirectory)

  private def _metadata_files(base: File): Vector[File] = {
    val files = _files(base).filter { x =>
      val n = x.getName.toLowerCase
      (n.endsWith(".json") || n.endsWith(".yaml") || n.endsWith(".yml")) &&
        (_is_page_source_key(_logical_path(_strip_suffix(_relative_path(base, x)))) || _is_article_media_metadata(x))
    }
    files.groupBy(x => _strip_suffix(_relative_path(base, x))).toVector.sortBy(_._1).flatMap {
      case (_, xs) => xs.sortBy(_priority).headOption
    }
  }

  private def _is_article_media_metadata(file: File): Boolean =
    _parse_metadata(file).hcursor.downField("type").as[String].toOption.contains("article-media-publication")

  private def _bundle_entries(base: File): Option[Vector[PublishMetadata.Entry]] = {
    val bundles = _files(base).filter { x =>
      x.isFile &&
        (x.getName.toLowerCase.endsWith(".json") || x.getName.toLowerCase.endsWith(".yaml") || x.getName.toLowerCase.endsWith(".yml"))
    }.flatMap { file =>
      val json = _parse_metadata(file)
      json.hcursor.downField("type").as[String].toOption match {
        case Some("publication-bundle") => Some(_bundle_entries(file, json))
        case _ => None
      }
    }
    if (bundles.isEmpty)
      None
    else
      Some(bundles.flatten.sortBy(_.path))
  }

  private def _bundle_entries(file: File, json: Json): Vector[PublishMetadata.Entry] =
    json.hcursor.downField("entries").focus.flatMap(_.asArray).getOrElse(Vector.empty).flatMap { entry =>
      for {
        path <- _json_string(entry, "path").map(_validate_relative_metadata_path)
        metadata <- entry.hcursor.downField("metadata").focus
      } yield {
        val key = _json_string(entry, "key").getOrElse(_strip_suffix(path))
        PublishMetadata.Entry(path, key, metadata)
      }
    }

  private def _files(base: File): Vector[File] =
    Option(base.listFiles).toVector.flatten.toVector.flatMap { x =>
      if (x.isDirectory)
        _files(x)
      else
        Vector(x)
    }

  private def _entry(base: File, file: File): PublishMetadata.Entry = {
    val json = _parse_metadata(file)
    val path = _relative_path(base, file)
    PublishMetadata.Entry(path, _strip_suffix(path), json)
  }

  private def _parse_metadata(file: File): Json = {
    val in = InputSource(file)
    if (file.getName.toLowerCase.endsWith(".json"))
      parser.parse(in.asText) match {
        case Right(r) => r
        case Left(l) => throw new IllegalArgumentException(s"Invalid publication metadata JSON: ${file.getPath}: ${l.message}", l)
      }
    else
      ConfigLoader.loadConfigFromYaml[Json](in).fold(
        e => throw new IllegalArgumentException(s"Invalid publication metadata YAML: ${file.getPath}: ${e.message}"),
        identity
      )
  }

  private def _priority(file: File): Int =
    if (file.getName.toLowerCase.endsWith(".json")) 0 else 1

  private def _relative_path(base: File, file: File): String =
    base.toPath.relativize(file.toPath).toString.replace(File.separatorChar, '/')

  private def _validate_relative_metadata_path(path: String): String = {
    val normalized = path.replace('\\', '/').split("/").toVector.filter(_.nonEmpty)
    if (normalized.isEmpty || normalized.contains(".") || normalized.contains("..") || path.startsWith("/") || path.contains("\u0000"))
      throw new IllegalArgumentException(s"Invalid publication metadata path: $path")
    val r = normalized.mkString("/")
    if (!r.startsWith("metadata/"))
      throw new IllegalArgumentException(s"Publication bundle entry must be under metadata/: $path")
    r
  }

  private def _strip_suffix(path: String): String =
    path.replaceFirst("""\.[^.]+$""", "")

  private def _logical_path(path: String): String =
    if (path.startsWith("metadata/"))
      path.substring("metadata/".length)
    else
      path

  private def _is_page_source_key(key: String): Boolean =
    key.startsWith("catalog/") ||
      key.matches("""projects/[^/]+/metadata""") ||
      key.matches("""samples/[^/]+/metadata""") ||
      key.startsWith("publication-pages/") ||
      key.startsWith("source-manifest/") ||
      key.startsWith("artifacts/") ||
      key.startsWith("repository/") ||
      key.startsWith("maven/") ||
      key.startsWith("releases/")

  private def _json_string(json: Json, path: String*): Option[String] =
    path.foldLeft(Option(json)) {
      case (Some(z), x) => z.hcursor.downField(x).focus
      case (None, _) => None
    }.flatMap(_.asString)
}
