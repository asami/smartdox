package org.smartdox.parser

import scala.util.parsing.combinator.RegexParsers
import org.smartdox._
import scalaz._
import Scalaz._
import scala.collection.mutable.ArrayBuffer

/*
 * @since   Dec. 24, 2011
 * @version Dec. 28, 2011
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

  def block: Parser[Block] = ul

  def ul: Parser[Ul] = {
    def ulline: Parser[(String, List[ListContent])] = {
      "[ ]+[-][ ]".r~rep(inline)<~opt(newline) ^^ {
        case indent~contents => (indent, contents)
      } 
    }
    def lines2lis(lines: List[(String, List[ListContent])]): List[Li] = {
      require (!lines.isEmpty, "ul lines is empty")
      var current = lines.map { case (l, r) => (l.indexOf('-'), r) }
      var previ = current.head._1

      def parse(): List[Li] = {
        val lis = new ArrayBuffer[Li]
        println("current = " + current)
        do {
          val (i, c) = current.head
          if (i == previ) {
            val li = Li(c)
            lis += li
            previ = i
            current = current.tail
            println("currentx = " + current)
          } else if (i > previ) {
            previ = i 
            val children = parse()
            val ul = Ul(children)
            val last = lis.last
            lis(lis.length - 1) = last :+ ul
          } else {
            previ = i 
            return lis.toList
          }
        } while (!current.isEmpty)
        lis.toList
      }
      parse()
    }
    rep1(ulline) ^^ {
      case ulline => Ul(lines2lis(ulline))
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
      case inline if (inline.isEmpty) => Text("*")
      case inline => Bold(inline)
    }
  }

  def italic: Parser[Inline] = {
    "/"~>rep(inline)<~"/" ^^ {
      case inline if (inline.isEmpty) => Text("/")
      case inline => Italic(inline)
    }
  }

  def underline: Parser[Inline] = {
    "_"~>rep(inline)<~"_" ^^ {
      case inline if (inline.isEmpty) => Text("_")
      case inline => Underline(inline)
    }
  }

  def code: Parser[Inline] = {
    "="~>rep(inline)<~"=" ^^ {
      case inline if (inline.isEmpty) => Text("=")
      case inline => Code(inline)
    }
  }

  def pre: Parser[Inline] = {
    "~"~>rep(inline)<~"~" ^^ {
      case inline if (inline.isEmpty) => Text("~")
      case inline => Pre(inline)
    }
  }
}
