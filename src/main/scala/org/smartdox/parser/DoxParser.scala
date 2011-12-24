package org.smartdox.parser

import scala.util.parsing.combinator.RegexParsers
import org.smartdox._
import scalaz._
import Scalaz._

/*
 * @since   Dec. 24, 2011
 * @version Dec. 25, 2011
 * @author  ASAMI, Tomoharu
 */
object DoxParser extends RegexParsers {
  val newline = """(\r\n|\n|\r)"""

  def parseOrgmode(in: String) = parseAll(orgmode, in)

  def parseOrgmodeZ(in: String) = parseOrgmode(in) match {
    case s: Success[_] => s.get.success[String].liftFailNel
    case n: NoSuccess => n.msg.fail[Dox].liftFailNel
  }

  def orgmode: Parser[Dox] = {
    rep(section1) ^^ {
      case section1 => Document(Head(), Body(section1))
    }
  }

  def section1: Parser[Section] = {
    "* " ~ rep(inline) ~ opt(newline) ~ contents ^^ {
      case _~title~_~contents => Section(title, contents, 1);
    }
  }

  def contents: Parser[List[Dox]] = {
    rep(inline)~opt(newline) ^^ {
      case inline~_ => inline
    }
  }

  def inline: Parser[Inline] = text|bold

  def text: Parser[Text] = {
    """[^*\n\r]+""".r ^^ {
      case s => Text(s)
    }
  }

  def bold: Parser[Bold] = {
    "*" ~> rep(inline) <~ "*" ^^ {
      case inline => Bold(inline)
    }
  }
}
