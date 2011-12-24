package org.smartdox.parser

import scala.util.parsing.combinator.RegexParsers
import org.smartdox._

/*
 * @since   Dec. 24, 2011
 * @version Dec. 24, 2011
 * @author  ASAMI, Tomoharu
 */
object DoxParser extends RegexParsers {
  val newline = """(\r\n|\n|\r)"""

  def parseOrgmode(in: String) = parseAll(orgmode, in)

  def orgmode: Parser[Dox] = {
    rep(section1) ^^ {
      case section1 => Document(Head(), Body(section1))
    }
  }

  def section1: Parser[Section] = {
    "* " ~ rep(inline) ~ opt(newline) ^^ {
      case inline~newline => Section(inline._2);
    }
  }

  def inline: Parser[Inline] = (text|bold)

  def text: Parser[Text] = {
    "[^*]+" ^^ {
      case s => Text(s)
    }
  }

  def bold: Parser[Bold] = {
    "*" ~> rep(inline) <~ "*" ^^ {
      case inline => Bold(inline)
    }
  }
}
