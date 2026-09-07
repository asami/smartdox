package org.smartdox.parser

import java.text.SimpleDateFormat
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.goldenport.context.DateTimeContext
import org.smartdox.Document
import org.smartdox.metadata.DocumentMetaData

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 */
private[parser] object Dox2ParserFrontMatter {
  def split(config: Dox2Parser.Config, in: String): (String, Option[DocumentMetaData]) =
    if (config.style != Dox2Parser.Config.DoxStyle.Markdown)
      (in, None)
    else {
      val lines = Option(in).getOrElse("").linesIterator.toVector
      if (lines.headOption.exists(_.trim == "---")) {
        lines.zipWithIndex.drop(1).find(_._1.trim == "---") match {
          case Some((_, end)) =>
            val yaml = lines.slice(1, end).mkString("\n")
            val body = lines.drop(end + 1).mkString("\n")
            implicit val dtctx: DateTimeContext = DateTimeContext.now()
            val metadata = _parse_markdown_front_matter(yaml)
            (body, metadata)
          case None => (in, None)
        }
      } else {
        (in, None)
      }
    }

  def merge(dox: Document, metadata: DocumentMetaData): Document =
    dox match {
      case Document(head, body, foot, attributes, location) =>
        Document(head.merge(metadata), body, foot, attributes, location)
    }

  private def _parse_markdown_front_matter(yaml: String)(implicit ctx: DateTimeContext): Option[DocumentMetaData] =
    Option(new org.yaml.snakeyaml.Yaml().load[Any](Option(yaml).getOrElse(""))).collect {
      case m: java.util.Map[_, _] =>
        val normalized = new java.util.LinkedHashMap[String, AnyRef]()
        m.asScala.foreach { case (key, value) =>
          normalized.put(key.toString, _normalize_yaml_value(value))
        }
        DocumentMetaData.create(ConfigFactory.parseMap(normalized))
    }

  private def _normalize_yaml_value(value: Any): AnyRef =
    value match {
      case null => ""
      case m: java.util.Map[_, _] =>
        val normalized = new java.util.LinkedHashMap[String, AnyRef]()
        m.asScala.foreach { case (key, value) =>
          normalized.put(key.toString, _normalize_yaml_value(value))
        }
        normalized
      case xs: java.util.List[_] =>
        xs.asScala.map(x => _normalize_yaml_value(x)).asJava
      case d: java.util.Date =>
        new SimpleDateFormat("yyyy-MM-dd").format(d)
      case v: java.lang.Boolean => v
      case v: java.lang.Number => v
      case v: String => v
      case v => v.toString
    }
}
