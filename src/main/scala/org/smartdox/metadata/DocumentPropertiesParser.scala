package org.smartdox.metadata

import scala.collection.JavaConverters._
import scala.util.Try
import com.typesafe.config.{Config => Hocon, ConfigFactory}
import org.yaml.snakeyaml.Yaml
import org.goldenport.context.Consequence

/*
 * @since   Jun.  2, 2026
 *  version Jun.  8, 2026
 * @version Jul.  6, 2026
 * @author  ASAMI, Tomoharu
 */
object DocumentPropertiesParser {
  // SmartDox HEAD `key=value` is a HEAD-specific string shorthand.
  // Design decision: this layer intentionally favors SmartDox authoring safety
  // over raw HOCON textual compatibility. In HEAD, users frequently write simple
  // metadata values such as unquoted URLs; raw HOCON rejects values like
  // `https://...` unless quoted. This parser accepts that convenience form,
  // quotes the complete RHS, and normalizes it into the canonical HOCON-backed
  // DocumentMetaData representation. Do not treat this as Java .properties
  // compatibility or as general HOCON syntax outside SmartDox HEAD.
  private val _yaml_parser = new Yaml()

  def isPropertiesText(p: String): Boolean =
    _is_properties(Option(p).getOrElse(""))

  def parse(p: String): Consequence[Hocon] =
    Consequence {
      val s = Option(p).getOrElse("").trim
      if (s.isEmpty)
        ConfigFactory.empty()
      else
        _parse_properties(s).
          orElse(_parse_yaml_hocon(s)).
          orElse(_parse_hocon(s)).
          getOrElse(ConfigFactory.parseString(s))
    }

  private def _parse_properties(p: String): Option[Hocon] =
    if (_is_properties(p))
      Some(ConfigFactory.parseString(_property_entries(p).map {
        case (key, value) => s"$key = ${_quote_property_value(value)}"
      }.mkString("\n")))
    else
      None

  private def _is_properties(p: String): Boolean = {
    val lines = _property_candidate_lines(p)
    lines.nonEmpty && lines.forall(_is_property_line)
  }

  private def _property_candidate_lines(p: String): Vector[String] =
    p.linesIterator.map(_.trim).filterNot(x => x.isEmpty || x.startsWith("#")).toVector

  private def _is_property_line(p: String): Boolean = {
    val i = p.indexOf('=')
    i > 0 && {
      val key = p.substring(0, i).trim
      key.nonEmpty && key.forall(c => c.isLetterOrDigit || c == '_' || c == '-' || c == '.')
    }
  }

  private def _property_entries(p: String): Vector[(String, String)] = {
    val s = _property_candidate_lines(p).mkString("\n")
    val pattern = """(?<![A-Za-z0-9_.?&/;-])([A-Za-z0-9_.-]+)\s*=""".r
    val matches = pattern.findAllMatchIn(s).toVector
    matches.zipWithIndex.map { case (m, i) =>
      val key = m.group(1)
      val next = matches.lift(i + 1).map(_.start).getOrElse(s.length)
      val value = s.substring(m.end, next).trim
      key -> value
    }
  }

  private def _quote_property_value(p: String): String =
    if ((p.startsWith("\"") && p.endsWith("\"")) || (p.startsWith("'") && p.endsWith("'")))
      p
    else
      "\"" + p.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def _parse_hocon(s: String): Option[Hocon] =
    Try(ConfigFactory.parseString(s)).toOption

  private def _parse_yaml_hocon(p: String): Option[Hocon] =
    _parse_yaml_any(p).collect {
      case m: java.util.Map[_, _] =>
        ConfigFactory.parseMap(m.asInstanceOf[java.util.Map[String, AnyRef]])
    }

  private def _parse_yaml_any(p: String): Option[Any] =
    Try(_yaml_parser.load[Any](Option(p).getOrElse(""))).toOption
}
