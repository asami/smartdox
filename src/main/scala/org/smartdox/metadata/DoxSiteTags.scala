package org.smartdox.metadata

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._

/*
 * @since   Jun. 28, 2026
 * @version Jun. 28, 2026
 * @author  ASAMI, Tomoharu
 */
case class DoxSiteTags(
  tags: Vector[DoxSiteTags.Entry] = Vector.empty
)

object DoxSiteTags {
  implicit val circeconf: Configuration = Configuration.default.withDefaults.withSnakeCaseMemberNames

  val empty: DoxSiteTags = DoxSiteTags()

  case class Ref(
    kind: String,
    title: String,
    sourcePath: String,
    publicPath: String,
    category: Option[String]
  )
  object Ref {
    implicit val refEncoder: Encoder.AsObject[Ref] = deriveConfiguredEncoder
  }

  case class Entry(
    id: String,
    key: String,
    segments: Vector[String],
    namespace: Option[String],
    parent: Option[String],
    slug: String,
    label: String,
    title: Option[String],
    summary: Option[String],
    aliases: Vector[String] = Vector.empty,
    locale: Option[String],
    sourcePath: Option[String],
    publicPath: String,
    bodyHtml: Option[String],
    refs: Vector[Ref] = Vector.empty,
    children: Vector[String] = Vector.empty
  )
  object Entry {
    implicit val entryEncoder: Encoder.AsObject[Entry] = deriveConfiguredEncoder
  }

  implicit val tagsEncoder: Encoder.AsObject[DoxSiteTags] = deriveConfiguredEncoder

  def create(fragments: DoxSiteDocumentFragments): DoxSiteTags = {
    val definitions = fragments.fragments.flatMap(_definition_entry)
    val refs = fragments.fragments.flatMap(_usage_refs)
    val keys = (definitions.map(x => x.key -> x.locale) ++ refs.map(_._1)).distinct.sortBy {
      case (key, locale) => (key, locale.getOrElse(""))
    }
    val childrenbyparent = keys.flatMap {
      case (key, locale) => _parent_key(key).map(parent => (parent, locale) -> key)
    }.groupBy(_._1).map {
      case (parent, children) => parent -> children.map(_._2).distinct.sorted
    }
    val entries = keys.map {
      case tagkey @ (key, locale) =>
        val definition = definitions.find(x => x.key == key && x.locale == locale)
        val usage = refs.filter(_._1 == tagkey).map(_._2).distinct.sortBy(x => (x.kind, x.category.getOrElse(""), x.title, x.publicPath))
        val base = definition.getOrElse(_entry(key, locale, None, None, None))
        base.copy(refs = usage, children = childrenbyparent.getOrElse(tagkey, Vector.empty))
    }
    DoxSiteTags(entries)
  }

  def toJsonString(p: DoxSiteTags): String =
    p.asJson.spaces2 + "\n"

  private def _definition_entry(fragment: DoxSiteDocumentFragments.Fragment): Option[Entry] =
    _definition_key(fragment.sourcePath).map { key =>
      _entry(
        key,
        Some(fragment.locale),
        fragment.title.orElse(fragment.headline),
        fragment.brief,
        Some(fragment.sourcePath),
        if (fragment.bodyHtml.trim.isEmpty) None else Some(fragment.bodyHtml)
      )
    }

  private def _usage_refs(fragment: DoxSiteDocumentFragments.Fragment): Vector[((String, Option[String]), Ref)] =
    fragment.tags.map(_tag_key(_, fragment.category)).filter(_.nonEmpty).distinct.map { key =>
      val title = fragment.headline.orElse(fragment.title).orElse(fragment.brief).getOrElse(fragment.publicPath)
      (key -> Some(fragment.locale)) -> Ref(fragment.kind.getOrElse("article"), title, fragment.sourcePath, fragment.publicPath, fragment.category)
    }

  private def _entry(
    key: String,
    locale: Option[String],
    title: Option[String],
    summary: Option[String],
    sourcepath: Option[String],
    bodyhtml: Option[String] = None
  ): Entry = {
    val segments = key.split('.').toVector.filter(_.nonEmpty)
    val slug = segments.mkString("/")
    Entry(
      id = s"tag:${key}",
      key = key,
      segments = segments,
      namespace = segments.headOption,
      parent = _parent_key(key).map(x => s"tag:${x}"),
      slug = slug,
      label = segments.lastOption.getOrElse(key),
      title = title.orElse(Some(key)),
      summary = summary,
      locale = locale,
      sourcePath = sourcepath,
      publicPath = s"tags/${slug}.html",
      bodyHtml = bodyhtml
    )
  }

  private def _definition_key(sourcePath: String): Option[String] = {
    val normalized = sourcePath.replace('\\', '/')
    if (!normalized.startsWith("tags/"))
      None
    else {
      val body = normalized.stripPrefix("tags/").replaceAll("\\.[^.]+$", "")
      val key = body.split('/').toVector.map(_tag_segment).filter(_.nonEmpty).mkString(".")
      if (key.isEmpty) None else Some(key)
    }
  }

  private def _tag_key(value: String, category: Option[String]): String = {
    val segments = value.trim.split("[./]+").toVector.map(_tag_segment).filter(_.nonEmpty)
    val normalized = segments.mkString(".")
    if (normalized.isEmpty)
      ""
    else if (segments.size > 1)
      normalized
    else
      category.map(c => Vector(_tag_segment(c), normalized).filter(_.nonEmpty).mkString(".")).filter(_.nonEmpty).getOrElse(normalized)
  }

  private def _tag_segment(value: String): String =
    value.trim.toLowerCase(java.util.Locale.ROOT).replaceAll("\\s+", "-").
      replaceAll("[\\\\/]+", "-").
      replaceAll("[^\\p{L}\\p{N}_-]+", "-").
      stripPrefix("-").
      stripSuffix("-")

  private def _parent_key(key: String): Option[String] = {
    val segments = key.split('.').toVector.filter(_.nonEmpty)
    if (segments.size <= 1) None else Some(segments.dropRight(1).mkString("."))
  }
}
