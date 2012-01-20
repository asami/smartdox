package org.smartdox.parser

import scala.util.parsing.combinator.RegexParsers
import org.smartdox._
import scalaz._
import Scalaz._
import scala.collection.mutable.ArrayBuffer
import java.net.URI
import Dox._
import java.io.Reader

/*
 * @since   Dec. 24, 2011
 * @version Jan. 20, 2012
 * @author  ASAMI, Tomoharu
 */
object DoxParser extends RegexParsers {
  override def skipWhitespace = false
  val newline = """(\r\n|\n|\r)""".r

  def parseOrgmode(reader: Reader) = parseAll(orgmode, reader)
  def parseOrgmode(in: String) = parseAll(orgmode, in)

  def parseOrgmodeZ(reader: Reader) = parseOrgmode(reader) |> _to_validation
  def parseOrgmodeZ(in: String) = parseOrgmode(in) |> _to_validation

  private def _to_validation(result: ParseResult[Dox]): Validation[NonEmptyList[String], Dox] = {
    result match {
      case s: Success[_] => s.get.success[String].liftFailNel
      case n: NoSuccess => n.msg.fail[Dox].liftFailNel
    }
  }

  def orgmode: Parser[Dox] = {
    head~body ^^ {
      case head~body => Document(head, body)
    }
  }

  def head: Parser[Head] = {
    rep(head_slot) ^^ {
      case slots => {
        val builder = Head.builder
        for ((name, value) <- slots) {
          name.toLowerCase match {
            case "title" => builder.title = value
            case "author" => builder.author = value
            case _ => {}
          }
        }
        builder.build
      }
    }
  }

  def head_slot: Parser[(String, InlineContents)] = {
    "#+"~>"[^:]+".r~":[ ]*".r~rep(inline)<~opt(newline) ^^ {
      case name~_~value => (name, value)
    }
  }

  def head1: Parser[Head] = {
    opt(head_title)~opt(head_author) ^^ {
      case title~author => {
        val builder = Head.builder
        builder.title = title | nil
        builder.author = author | nil
        builder.build
      }
    }
  }

  def head_title: Parser[InlineContents] = {
    starter_colon("title")~>rep(inline)<~opt(newline) ^^ {
      case inline => inline
    }
  }

  def head_author: Parser[InlineContents] = {
    starter_colon("author")~>rep(inline)<~opt(newline) ^^ {
      case inline => inline
    }
  }

  def head0: Parser[Head] = {
    val builder = Head.builder
    def title: Parser[Unit] = {
//      starter_colon("title")~>rep(inline)<~opt(newline) ^^ {
      "#+TITLE: "~>rep(inline)<~opt(newline) ^^ {
        case inline => builder.title = inline
      }
    }
    def author: Parser[Unit] = {
      starter_colon("author")~>rep(inline)<~opt(newline) ^^ {
        case inline => builder.author = inline
      }
    }
    rep(title|author)
    println("head: " + builder)
    success(builder.build)
  }

  def body: Parser[Body] = {
    contents~section1s ^^ {
      case contents~section1 => Body(contents ::: section1)
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
    def foldemptyline(a: List[Dox], r: List[Dox], e: Dox) =
      if (r.nonEmpty) (a :+ Paragraph(r), nil) else (a, nil)
    def foldstrongblock(a: List[Dox], r: List[Dox], e: Dox) =
      if (r.nonEmpty) ((a :+ Paragraph(r)) :+ e, nil) else (a :+ e, nil)
    def foldweakblock(a: List[Dox], r: List[Dox], e: Dox) =
      if (r.nonEmpty) (a, r :+ e) else (a :+ e, r) 
    def foldinline(a: List[Dox], r: List[Dox], e: Dox) = (a, r :+ e)
    rep(embedded|contentsline|emptyline) ^^ {
      case contents => {
        val (a, r) = contents.flatten.foldl(Pair(nil[Dox], nil[Dox])) {
          case ((a, r), e) => e match {
            case _: EmptyLine => foldemptyline(a, r, e)
            case _: Inline => foldinline(a, r, e)
            case _: Ul => foldweakblock(a, r, e)
            case _: Ol => foldweakblock(a, r, e)
            case _: Dl => foldweakblock(a, r, e)
            case _ => foldstrongblock(a, r, e)
          }
        }
        if (r.nonEmpty) a :+ Paragraph(r) else a
      }
    }
  }

  def contentsline: Parser[List[Dox]] = {
    not("[*]+[ ]".r)~>rep1(block|inline)<~opt(newline) ^^ {
      case contents => {
        println("contentsline = " + contents)
        contents
      }
    }
  }

  def embedded: Parser[List[Dox]] = rep1(img_dot|img_ditaa|img_sm)

  def block: Parser[Block] = dl|ulol|table|figure|console|program|includeprogram

  def emptyline: Parser[List[EmptyLine]] = {
    newline ^^ {
      case _ => List(EmptyLine())
    }
  } 

  sealed trait ListLine {
    val indent: Int
    val contents: List[ListContent]
  }
  case class UlLine(indent: Int, contents: List[ListContent]) extends ListLine
  case class OlLine(indent: Int, contents: List[ListContent]) extends ListLine
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

  def dl: Parser[Dl] = {
    def dline: Parser[(Dt, Dd)] = {
      "[ ]*[-][ ]".r~>rep(text)~" :: "~rep(inline)<~opt(newline) ^^ {
        case term~_~desc => (Dt(term.toText), Dd(desc))
      }
    }
    rep1(dline) ^^ {
      case line => Dl(line)
    }
  }

  def ulol: Parser[Block] = {
    def uoline: Parser[ListLine] = {
      ("[ ]*[-][ ]".r|"""[ ]*\d+[.][ ]""".r)~rep(inline)<~opt(newline) ^^ {
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

  sealed trait TableLine
  case class FrameTableLine() extends TableLine
  case class DataTableLine(contents: List[List[Inline]]) extends TableLine

  def normalize_space(lines: List[List[Inline]]): List[List[Inline]] = {
    def isspace(inline: Inline) = inline.isInstanceOf[Space]
    for (l <- lines) yield {
      l.dropWhile(isspace).reverse.dropWhile(isspace).reverse
    }
  }

  // 2011-12-31
  sealed trait Attribute
  case class CaptionAttribute(value: List[Inline]) extends Attribute
  case class LabelAttribute(value: String) extends Attribute
  case class HtmlAttribute(value: String) extends Attribute
  case class LatexAttribute(value: String) extends Attribute
  case class CommentAttribute(value: String) extends Attribute 

  def attrcaption: Parser[Attribute] = {
    starter_colon("caption")~>rep(inline)<~opt(newline) ^^ {
      case value => CaptionAttribute(value)
    }
  }

  def attrlabel: Parser[Attribute] = {
    starter_colon("label")~>"[^\n\r]*".r<~opt(newline) ^^ {
      case value => LabelAttribute(value)
    }
  }

  def attrhtml: Parser[Attribute] = {
    starter_colon("attr_html")~>"[^\n\r]*".r<~opt(newline) ^^ {
      case value => HtmlAttribute(value)
    }
  }

  def attrlatex: Parser[Attribute] = {
    starter_colon("attr_latex")~>"[^\n\r]*".r<~opt(newline) ^^ {
      case value => LatexAttribute(value)
    }
  }

  def floatattr=attrcaption|attrlabel|attrhtml|attrlatex

  def table: Parser[Table] = {
    def tableattrs: Parser[List[Attribute]] = rep(floatattr)
    def tableline: Parser[TableLine] = frameline|dataline
    def frameline: Parser[FrameTableLine] = {
      "[|][-][^\n\r]*".r<~opt(newline) ^^ {
        case _ => FrameTableLine()
      }
    }
    def dataline: Parser[DataTableLine] = {
      "|"~>repsep(rep(inline), "|")<~opt(newline) ^^ {
        case lines => {
          val ls = normalize_space(lines)
          if (ls.last.nonEmpty) DataTableLine(ls)
          else DataTableLine(ls.dropRight(1))
        }
      }
    }
    def normalize(lines: List[TableLine]): List[TableLine] = {
      val (_, frameremovedreverse) = lines.foldl((FrameTableLine(): TableLine, nil[TableLine])) {
        case ((prev, a), e) => {
          if (prev.isInstanceOf[FrameTableLine] && e.isInstanceOf[FrameTableLine]) {
            (prev, a)
          } else {
            (e, e :: a)
          }
        }
      }
      frameremovedreverse.dropWhile(_.isInstanceOf[FrameTableLine]).reverse
    }
    def build(lines: List[TableLine]): (Option[THead], TBody, Option[TFoot]) = {
      val aggregatedreverse = lines.foldl(nil[List[TableLine]]) {
        case (a, e) => {
          e match {
            case _: FrameTableLine => nil :: a 
            case d: DataTableLine => if (a == Nil) List(List(d)) else (d :: a.head) :: a.tail
          }
        }
      }
      val aggregated = aggregatedreverse.foldl(nil[List[TableLine]]) {
        case (a, e) => e.reverse :: a
      }
      aggregated.length match {
        case 0 => (None, TBody(Nil), None)
        case 1 => (None, TBody(datarecords(aggregated.head)), None)
        case 2 => (THead(headrecords(aggregated.head)).some,
            TBody(datarecords(aggregated.last)), None)
        case 3 => (THead(headrecords(aggregated(0))).some,
            TBody(datarecords(aggregated(1))),
            TFoot(datarecords(aggregated(2))).some)
        case _ => {
          val data = aggregated.slice(1, aggregated.length - 1).flatten
          (THead(headrecords(aggregated.head)).some,
            TBody(datarecords(data)),
            TFoot(datarecords(aggregated.last)).some)
        }
      }
    }
    def datarecords(lines: List[TableLine]): List[TR] = {
      val data = lines.collect {
        case d: DataTableLine => d
      }
      for (d <- data) yield {
        TR(d.contents.map(TD(_)))
      }
    }
    def headrecords(lines: List[TableLine]): List[TR] = {
      val data = lines.collect {
        case d: DataTableLine => d
      }
      for (d <- data) yield {
        TR(d.contents.map(TH(_)))
      }
    }
    tableattrs~rep1(tableline) ^^ {
      case attrs~lines => {
        val normalized = normalize(lines)
        val caption = attrs.collectFirst {
          case c: CaptionAttribute => c.value
        }.map(Caption)
        val label = attrs.collectFirst {
          case c: LabelAttribute => c.value
        }
        val (head, body, foot) = build(normalized)
        Table(head, body, foot, caption, label)
      }
    }
  }

  def figure: Parser[Figure] = {
    rep1(floatattr)~(img|img_dot|img_ditaa|img_sm)<~opt(newline) ^^ {
      case attrs~img => {
        val caption = attrs.collectFirst {
          case c: CaptionAttribute => c.value
        }.map(Figcaption) | Figcaption(Nil)
        val label = attrs.collectFirst {
          case c: LabelAttribute => c.value
        }
        Figure(img, caption, label)
      }
    } ^^ {
      case fig if fig.caption.contents.nonEmpty => fig
    }
  }

  def program: Parser[Program] = {
    beginend("src") { (params: List[String], contents: String) =>
      val filename = params.head
      Program(contents)
    }
  }

  def beginend[T](name: String)(body: (List[String], String) => T): Parser[T] = {
    val upper = name.toUpperCase
    ("#+BEGIN_" + upper|"#+begin_" + name)~"[ ]+".r~>rep1sep("[^ \n\r]+".r, "[ ]+".r)~newline~embedlines<~("#+END_" + upper|"#+end_" + name)~"[ ]*".r~opt(newline) ^^ {
      case params~_~contents => {
        body(params, contents)
      }
    }    
  }

  def console: Parser[Console] = {
    beginendmatch("src"){ (ps: List[String], contents: String) => 
      ps.headOption.map(_ == "console") | false
    } { (params: List[String], contents: String) =>
      val filename = params.head
      Console(contents)
    }    
  }

  def beginendmatch[T](name: String)(
      p: (List[String], String) => Boolean)
      (body: (List[String], String) => T): Parser[T] = {
    val upper = name.toUpperCase
    ("#+BEGIN_" + upper|"#+begin_" + name)~"[ ]+".r~>rep1sep("[^ \n\r]+".r, "[ ]+".r)~newline~embedlines<~("#+END_" + upper|"#+end_" + name)~"[ ]*".r~opt(newline) ^? {
      case params~_~contents if p(params, contents) => {
        body(params, contents)
      }
    }
  }

  def includeprogram: Parser[Program] = {
    starter_colon("include")~"\""~>"""[^"]+""".r~"\""~"[ ]+".r~rep1sep("[^ \n\r]+".r, "[ ]+".r)<~opt(newline) ^^ {
      case filename~_~_~params => {
        Program("", List("src" -> filename))
      }
    }
  }

  def starter(name: String): Parser[String] = {
    ("(?i)([#][+]" + name + ")").r<~"[ ]+".r
  }

  def starter_colon(name: String): Parser[String] = {
    ("(?i)([#][+]" + name + ":)").r<~"[ ]*".r
//    ("""(?i)\([#][+]""" + name + """:\)""").r<~"[ ]*".r
  }

  def starter0(name: String): Parser[String] = {
    val upper = name.toUpperCase
    ("#+" + upper|"#+" + name)<~"[ ]+".r
  }

  def startercolon0(name: String): Parser[String] = {
    val upper = name.toUpperCase
    ("#+" + upper + ":"|"#+" + name + ":")<~"[ ]*".r
  }
    
  def inline: Parser[Inline] = (space|text|bold|italic|underline|code|pre|del|
      bold_xml|italic_xml|underline_xml|code_xml|pre_xml|del_xml|
      img|hyperlink|hyperlink_xml|hyperlink_literal)

  def space: Parser[Space] = {
    "[ ]+".r ^^ {
      case _ => Space()
    }
  }

  def text: Parser[Text] = {
    """[^*/_=~+<>\[\] :|\n\r]+""".r ^^ {
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
      " "~opt(whiteSpace)~"""\w+""".r~opt(whiteSpace)~"="~opt(whiteSpace)~'"'~"""[^"]*""".r~'"' ^^ {
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
      " "~opt(whiteSpace)~"""\w+""".r~opt(whiteSpace)~"="~opt(whiteSpace)~'"'~"""[^"]*""".r~'"' ^^ {
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
      case inline => Pre(inline.mkString)
    }
  }

  def pre_xml: Parser[Inline] = {
    inline_xml("pre") ^^ {
      case elem => Pre(elem.contents.mkString)
    }
  }

  def del: Parser[Inline] = {
    "+"~>rep(inline)<~"+" ^^ {
      case inline if (inline.isEmpty) => Text("+")
      case inline => Del(inline)
    }
  }

  def del_xml: Parser[Inline] = {
    inline_xml("del") ^^ {
      case elem => Del(elem.contents)
    }
  }

  def hyperlink: Parser[Inline] = {
    def label: Parser[List[Inline]] = {
      "["~>rep(inline)<~"]"
    }
    "[["~>"""[^]]+""".r~"]"~opt(label)<~"]" ^^ {
      case link~_~label => Hyperlink(label | List(Text(link)), new URI(link))
    }
  }

  def hyperlink_xml: Parser[Hyperlink] = {
    def warning = "***No href***"
    inline_xml("a") ^^ {
      case elem => {
        val href = elem.params.find(_.name == "href").map(_.value)
        val txt = href ? elem.contents | elem.contents :+ Text(warning)
        Hyperlink(txt, new URI(href | ""))
      }
    }
  }

  def hyperlink_literal: Parser[Hyperlink] = {
    "http://[^ ]+".r ^^ {
      case uri => Hyperlink(List(Text(uri)), new URI(uri))
    }
  }

  def img: Parser[Img] = {
    "[["~>"""[^]]+[.]""".r~img_suffix<~"]]" ^^ {
      case name~suffix => ReferenceImg(new URI(name+suffix))
    }
  }

  def img_suffix: Parser[String] = {
    "(png|jpeg|jpg|gif|pdf)".r    
  }
  
  def img_dot: Parser[Img] = {
    ("#+BEGIN_DOT"|"#+begin_dot")~"[ ]+".r~>rep1sep("[^ \n\r]+".r, "[ ]+".r)~newline~embedlines<~("#+END_DOT "|"#+end_dot")~"[ ]*".r~opt(newline) ^^ {
      case params~_~contents => {
        val filename = params.head
        DotImg(new URI(filename), contents, params.tail)
      }
    }
  }

  def img_ditaa: Parser[Img] = {
    ("#+BEGIN_DITAA"|"#+begin_ditaa")~"[ ]+".r~>rep1sep("[^ \n\r]+".r, "[ ]+".r)~newline~embedlines<~("#+END_DITAA "|"#+end_ditaa")~"[ ]*".r~opt(newline) ^^ {
      case params~_~contents => {
        val filename = params.head
        DitaaImg(new URI(filename), contents, params.tail)
      }
    }
  }

  def img_sm: Parser[Img] = img_sm_csv|img_sm_org

  def img_sm_org: Parser[Img] = {
    ("#+BEGIN_SM_ORG"|"#+begin_sm_org")~"[ ]+".r~>rep1sep("[^ \n\r]+".r, "[ ]+".r)~newline~embedlines<~("#+END_SM_ORG "|"#+end_sm_org")~"[ ]*".r~opt(newline) ^^ {
      case params~_~contents => {
        val filename = params.head
        SmCsvImg(new URI(filename), contents, params.tail) // SmOrgImg
      }
    }
  }  

  def img_sm_csv: Parser[Img] = {
    ("#+BEGIN_SM_CSV "|"#+begin_sm_csv")~"[ ]+".r~>rep1sep("[^ \n\r]+".r, "[ ]+".r)~newline~embedlines<~("#+END_SM_CSV "|"#+end_sm_csv")~"[ ]*".r~opt(newline) ^^ {
      case params~_~contents => {
        val filename = params.head
        SmCsvImg(new URI(filename), contents, params.tail)
      }
    }
  }

  def embedlines: Parser[String] = {
    rep(embedline) ^^ {
      case lines => lines.mkString("\n")
    }
  }

  def embedline: Parser[String] = {
    not("#+END_"|"#+end_")~>"[^\n\r]*".r<~newline ^^ {
      case contents => contents
    }
  }
}
