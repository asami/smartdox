package org.smartdox.parser

import scala.util.parsing.combinator.RegexParsers
import org.smartdox._
import scalaz._
import Scalaz._
import scala.collection.mutable.ArrayBuffer

/*
 * @since   Dec. 24, 2011
 * @version Dec. 29, 2011
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
    head~body ^^ {
      case head~body => Document(head, body)
    }
  }

  def head: Parser[Head] = {
    val builder = Head.builder
    def title: Parser[Unit] = {
      "#+TITLE: "~>rep(inline)<~opt(newline) ^^ {
        case inline => builder.title = inline
      }
    }
    def author: Parser[Unit] = {
      "#+AUTHOR: "~>rep(inline)<~opt(newline) ^^ {
        case inline => builder.author = inline
      }
    }
    rep(title|author)
    success(builder.build)
  }

  def body: Parser[Body] = {
    section1s ^^ {
      case section1 => Body(section1)
    }
  }

  def section1s: Parser[List[Section]] = rep(section1)

  def section1: Parser[Section] = {
    "* "~>rep(inline)~opt(newline)~opt(contents)~rep(section2) ^^ {
      case title~_~contents~section2 => {
        println("section1xs#title = " + title)
        println("section1xs#contntents = " + contents)
        println("section1xs#section2 = " + section2)
        Section(title, (contents | nil) ::: section2, 1)
      }
    }
  }

  def section2: Parser[Section] = {
    "** "~>rep(inline)~opt(newline)~opt(contents)~rep(section3) ^^ {
      case title~_~contents~section3 => 
        println("section2 = " + title + "," + contents);Section(title, (contents | nil) ::: section3, 2)
    }
  }

  def section3: Parser[Section] = {
    "*** "~>rep(inline)~opt(newline)~opt(contents)~rep(section4) ^^ {
      case title~_~contents~section4 => 
        println("section3 = " + title + "," + contents);Section(title, (contents | nil) ::: section4, 3)
    }
  }

  def section4: Parser[Section] = {
    "**** "~>rep(inline)~opt(newline)~opt(contents)~rep(section5) ^^ {
      case title~_~contents~section5 =>
        println("section4 = " + title + "," + contents);Section(title, (contents | nil) ::: section5, 3)
    }
  }

  def section5: Parser[Section] = {
    "***** "~>rep(inline)~opt(newline)~opt(contents) ^^ {
      case title~_~contents =>
        println("section4 = " + title + "," + contents);Section(title, contents | nil, 3)
    }
  }

  def contents: Parser[List[Dox]] = {
    rep(contentsline) ^^ {
      case contents => contents.flatten
    }
  }

  def contentsline: Parser[List[Dox]] = {
    not("[*]+[ ]".r)~>rep1(block|inline)<~opt(newline) ^^ {
      case inline => println("contentsline = " +inline);inline
    }
  }

  def block: Parser[Block] = ulol

  sealed abstract class ListLine(val indent: Int, val contents: List[ListContent])
  case class UlLine(i: Int, c: List[ListContent]) extends ListLine(i, c)
  case class OlLine(i: Int, c: List[ListContent]) extends ListLine(i, c)
  object ListLine {
    def apply(s: String, c: List[ListContent]): ListLine = {
      val iu = s.indexOf('-')
      if (iu != -1) UlLine(iu, c)
      else {
         val io = s.indexWhere(Character.isDigit(_))
         if (io != -1) OlLine(io, c)
         else sys.error("Unknown list line = " + s)
      }
    }
  }

  def ulol: Parser[Block] = {
    def uoline: Parser[ListLine] = {
      ("[ ]+[-][ ]".r|"""[ ]+\d+[.][ ]""".r)~rep(inline)<~opt(newline) ^^ {
        case indent~contents => ListLine(indent, contents)
      } 
    }
    def lines2lis(lines: List[ListLine]): List[Li] = {
      require (!lines.isEmpty, "ul/ol lines are empty")
      var current = lines
      var previ = current.head.indent

      def parse(): List[Li] = {
        val lis = new ArrayBuffer[Li]
        println("current = " + current)
        do {
          val line = current.head
          if (line.indent == previ) {
            val li = Li(line.contents)
            lis += li
            previ = line.indent
            current = current.tail
            println("currentx = " + current)
          } else if (line.indent > previ) {
            previ = line.indent
            current.head match {
              case _: UlLine => {
                val children = parse()
                val ul = Ul(children)
                val last = lis.last
                lis(lis.length - 1) = last :+ ul
              }
              case _: OlLine => {
                val children = parse()
                val ol = Ol(children)
                val last = lis.last
                lis(lis.length - 1) = last :+ ol                
              }
            }
          } else {
            previ = line.indent
            return lis.toList
          }
        } while (!current.isEmpty)
        lis.toList
      }
      parse()
    }
    rep1(uoline) ^^ {
      case line => line.head match {
        case _: UlLine => Ul(lines2lis(line))
        case _: OlLine => Ol(lines2lis(line))
      }
    }
  }

  def inline: Parser[Inline] = (text|bold|italic|underline|code|pre|bold_xml|italic_xml|underline_xml|code_xml|pre_xml)

  def text: Parser[Text] = {
    """[^*/_=~<>\n\r]+""".r ^^ {
      case s => 
        println("s = " + s);Text(s)
    }
  }

  def bold: Parser[Inline] = {
    "*"~>rep(inline)<~"*" ^^ {
      case inline if (inline.isEmpty) => Text("*")
      case inline => Bold(inline)
    }
  }

  def bold_xml: Parser[Inline] = {
    inline_xml("b") ^^ {
      case elem => Bold(elem.contents)
    }
  }

  case class XElement[T <: Dox](params: List[XParam], contents: List[T])
  case class XParam(name: String, value: String)

  def inline_xml(name: String): Parser[XElement[Inline]] = {
    def xmlparam: Parser[XParam] = {
      " "~opt(whiteSpace)~"""\w""".r~opt(whiteSpace)~"="~opt(whiteSpace)~'"'~"""[^"]*""".r~'"' ^^ {
        case _~_~name~_~_~_~_~value~_ => XParam(name, value)
      }
    }
    def xmlparams: Parser[List[XParam]] = {
      rep(xmlparam)
    }
    "<"~opt(whiteSpace)~name~xmlparams~opt(whiteSpace)~">"~rep(text)~"</"~opt(whiteSpace)~name~opt(whiteSpace)~">" ^^ {
      case _~_~_~params~_~_~contents~_~_~_~_~_ => XElement(params, contents)
    }
  }

  def contents_xml(name: String): Parser[XElement[Dox]] = {
    def xmlparam: Parser[XParam] = {
      " "~opt(whiteSpace)~"""\w""".r~opt(whiteSpace)~"="~opt(whiteSpace)~'"'~"""[^"]*""".r~'"' ^^ {
        case _~_~name~_~_~_~_~value~_ => XParam(name, value)
      }
    }
    def xmlparams: Parser[List[XParam]] = {
      rep(xmlparam)
    }
    "<"~opt(whiteSpace)~name~xmlparams~opt(whiteSpace)~">"~contents~"</"~opt(whiteSpace)~name~opt(whiteSpace)~">" ^^ {
      case _~_~_~params~_~_~contents~_~_~_~_~_ => XElement(params, contents)
    }
  }

  def italic: Parser[Inline] = {
    "/"~>rep(inline)<~"/" ^^ {
      case inline if (inline.isEmpty) => Text("/")
      case inline => Italic(inline)
    }
  }

  def italic_xml: Parser[Inline] = {
    inline_xml("i") ^^ {
      case elem => Italic(elem.contents)
    }
  }

  def underline: Parser[Inline] = {
    "_"~>rep(inline)<~"_" ^^ {
      case inline if (inline.isEmpty) => Text("_")
      case inline => Underline(inline)
    }
  }

  def underline_xml: Parser[Inline] = {
    inline_xml("u") ^^ {
      case elem => Underline(elem.contents)
    }
  }

  def code: Parser[Inline] = {
    "="~>rep(inline)<~"=" ^^ {
      case inline if (inline.isEmpty) => Text("=")
      case inline => Code(inline)
    }
  }

  def code_xml: Parser[Inline] = {
    inline_xml("code") ^^ {
      case elem => Code(elem.contents)
    }
  }

  def pre: Parser[Inline] = {
    "~"~>rep(inline)<~"~" ^^ {
      case inline if (inline.isEmpty) => Text("~")
      case inline => Pre(inline)
    }
  }

  def pre_xml: Parser[Inline] = {
    inline_xml("pre") ^^ {
      case elem => Pre(elem.contents)
    }
  }
}
