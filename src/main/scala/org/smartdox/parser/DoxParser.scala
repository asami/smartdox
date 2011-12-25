package org.smartdox.parser

import scala.util.parsing.combinator.RegexParsers
import org.smartdox._
import scalaz._
import Scalaz._

/*
 * @since   Dec. 24, 2011
 * @version Dec. 26, 2011
 * @author  ASAMI, Tomoharu
 */
object DoxParser extends RegexParsers {
  override def skipWhitespace = false
  val newline = """(\r\n|\n|\r)""".r

  def parseOrgmode(in: String) = parseAll(orgmode, in)

  def parseOrgmodeZ(in: String) = parseOrgmode(in) match {
    case s: Success[_] => s.get.success[String].liftFailNel
    case n: NoSuccess => n.msg.fail[Dox].liftFailNel
  }

  def orgmode: Parser[Dox] = {
    section1s ^^ {
      case section1 => Document(Head(), Body(section1))
    }
  }

  def section1s: Parser[List[Section]] = rep(section1)

  def section1: Parser[Section] = {
    "* "~rep(inline)~opt(newline)~opt(contents)~rep(section2) ^^ {
      case _~title~_~contents~section2 => {
        println("section1xs#title = " + title)
        println("section1xs#contntents = " + contents)
        println("section1xs#section2 = " + section2)
        Section(title, (contents | nil) ::: section2, 1)
      }
    }
  }

  def section2: Parser[Section] = {
    "** "~rep(inline)~opt(newline)~opt(contents)~rep(section3) ^^ {
      case _~title~_~contents~section3 => 
        println("section2 = " + title + "," + contents);Section(title, (contents | nil) ::: section3, 2)
    }
  }

  def section3: Parser[Section] = {
    "*** "~rep(inline)~opt(newline)~opt(contents)~rep(section4) ^^ {
      case _~title~_~contents~section4 => 
        println("section3 = " + title + "," + contents);Section(title, (contents | nil) ::: section4, 3)
    }
  }

  def section4: Parser[Section] = {
    "**** "~rep(inline)~opt(newline)~opt(contents)~rep(section5) ^^ {
      case _~title~_~contents~section5 =>
        println("section4 = " + title + "," + contents);Section(title, (contents | nil) ::: section5, 3)
    }
  }

  def section5: Parser[Section] = {
    "***** "~rep(inline)~opt(newline)~opt(contents) ^^ {
      case _~title~_~contents =>
        println("section4 = " + title + "," + contents);Section(title, contents | nil, 3)
    }
  }

  def contents: Parser[List[Dox]] = {
    not("*")~rep(inline)~opt(newline) ^^ {
      case _~inline~_ => println("contents = " +inline);inline
    }
  }

  def inline: Parser[Inline] = (text|bold)

  def text: Parser[Text] = {
    """[^*\n\r]+""".r ^^ {
      case s => 
        println("s = " + s);Text(s)
    }
  }

  def bold: Parser[Inline] = {
    "*"~>rep(inline)<~"*" ^^ {
      case inline if (inline.isEmpty) => Text("**")
      case inline => Bold(inline)
    }
  }
}
