package org.smartdox.parser

import org.smartdox.{Dox, Inline, InlineMacro}

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
private[parser] object DoxInlineParserInlineMacro {
  private val _full_input_macro_regex = """^([A-Za-z][A-Za-z0-9_-]*):\[(.*)\]$""".r

  def parse(config: DoxInlineParser.Config, in: String): Option[Dox] =
    if (config.asciidoc.isInlineMacro)
      in match {
        case _full_input_macro_regex(name, contents) => Some(create(config, name, contents))
        case _ => None
      }
    else
      None

  def splitName(p: Vector[Char]): (Option[String], String) = {
    val text = p.mkString
    val index = text.lastIndexWhere(ch => !Character.isLetterOrDigit(ch) && ch != '_' && ch != '-')
    val name = text.drop(index + 1)
    val prefix = if (index < 0) None else Some(text.take(index + 1))
    if (_is_name(name))
      prefix -> name
    else
      None -> ""
  }

  def create(config: DoxInlineParser.Config, name: String, contents: String): Inline =
    name match {
      case "site" => DoxInlineParserSiteLink.create(contents, config.location)
      case _ => Dox.attachLocation(InlineMacro(name, contents), config.location).asInstanceOf[Inline]
    }

  private def _is_name(p: String): Boolean =
    p.nonEmpty && p.head.isLetter && p.forall(ch => Character.isLetterOrDigit(ch) || ch == '_' || ch == '-')
}
