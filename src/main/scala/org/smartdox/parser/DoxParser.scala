package org.smartdox.parser

import com.asamioffice.goldenport.text.UPathString
import java.io.{Reader, BufferedReader, InputStreamReader}
import java.net.URI
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import scalaz._, Scalaz._, Validation._, Tree._
import org.smartdox._
import Dox._
import com.asamioffice.goldenport.io.UURL
import org.goldenport.collection.VectorMap

/**
 * @since   Dec. 24, 2011
 *  version Feb. 11, 2012
 *  version Apr. 24, 2012
 *  version Jun.  7, 2012
 *  version Jul. 22, 2012
 *  version Oct. 15, 2012
 *  version Nov. 23, 2012
 *  version Feb.  5, 2014
 *  version Jan.  5, 2015
 *  version Mar. 10, 2016
 *  version Dec. 31, 2018
 * @version Oct. 28, 2024
 * @author  ASAMI, Tomoharu
 */
class DoxParser(
  useUnderline: Boolean = false
) extends RegexParsers {
  override def skipWhitespace = false
  val newline = """(\r\n|\n|\r)""".r

  def parseOrgmode(reader: Reader) = parseAll(orgmode, reader)
  def parseOrgmode(in: String) = parseAll(orgmode, in)
  def parseOrgmode(uri: URI): ParseResult[Dox] = {
    var reader: BufferedReader = null
    try {
      val url = UURL.getURLFromFileOrURLName(uri.toString)
      reader = new BufferedReader(new InputStreamReader(url.openStream, "utf-8"))
      parseAll(orgmode, reader) match {
        case s: Success[_] => {
          val base = _make_base(uri)
          s.map(_resolve_include(base, _))
        }
        case n: NoSuccess => n
      }
    } finally {
      if (reader != null) reader.close
    }
  }
  def parseOrgmodeAutoTitle(reader: Reader) = parseAll(orgmode_auto_title, reader)
  def parseOrgmodeAutoTitle(in: String) = parseAll(orgmode_auto_title, in)

  def parseOrgmodeZ(reader: Reader) = parseOrgmode(reader) |> _to_validation
  def parseOrgmodeZ(in: String) = parseOrgmode(in) |> _to_validation
  def parseOrgmodeZ(uri: URI) = parseOrgmode(uri) |> _to_validation

  private def _to_validation(result: ParseResult[Dox]): Validation[NonEmptyList[String], Dox] = {
    result match {
      case s: Success[_] => s.get.success[String].toValidationNel
      case n: NoSuccess => n.msg.failure[Dox].toValidationNel
    }
  }

  private def _resolve_include(base: String, dox: Dox): Dox = {
    val t = Dox.tree(dox)
    val r = _resolve_include_tree(base, t)
    Dox.untreeE(r)
  }

  private def _resolve_include_tree(base: String, t: Tree[Dox]): Tree[Dox] = {
//    println("DoxParser#_resolve_include_tree = " + _resolve_include_children(base, t.subForest))
    node(t.rootLabel, _resolve_include_children(base, t.subForest))
  }

  private def _resolve_include_children(base: String, xs: Stream[Tree[Dox]]): Stream[Tree[Dox]] = {
//    println("DoxParser#_resolve_include_children = " + xs.toList.map(_.rootLabel))
    val r = xs.flatMap(x => x.rootLabel match {
      case i: IncludeDoc => {
        val uri = _make_uri(base, i.filename)
//        println("DoxParser#_resolve_include_children = " + uri)
        parseOrgmode(uri) match {
          case s: Success[_] => {
//            println("DoxParser success: " + s.get)
            _body_fragment(s.get)
          }
          case n: NoSuccess => { // XXX
            println("DoxParser: " + i.filename + " not found.")
            None
          }
        }
      }
      case _ => _resolve_include_tree(base, x).some
    })
//    println("DoxParser result: " + r.toList.map(_.drawTree))
    r
  }

  private def _body_fragment(t: Dox): Stream[Tree[Dox]] = {
    t.elements(1).elements.map(_.tree).toStream
  }

  /**
   * @return null current directory
   */
  private def _make_base(uri: URI): String = {
    UPathString.getContainerPathname(uri.toString)
  }

  /**
   * @param base null means current directory
   */
  private def _make_uri(base: String, filename: String): URI = {
    new URI(UPathString.concatPathname(base, filename))
  }

  def orgmode: Parser[Dox] = {
    head~body ^^ {
      case head~body => Document(head, body)
    }
  }

  def orgmode_auto_title: Parser[Dox] = {
    head~body ^^ {
      case head~body => {
        if (head.title.isEmpty) {
          body.contents match {
            case (x: Paragraph) :: xs => {
              if (x.contents.isEmpty || !x.contents.head.isInstanceOf[Inline]) {
                Document(head, body)
              } else {
                val cs = x.contents.tail.span { c => 
                  c.isInstanceOf[Inline] && !c.isInstanceOf[Text]
                }
                val is = (x.contents.head :: cs._1) collect { case i: Inline => i }
                val l = if (cs._2.isEmpty) xs else Paragraph(cs._2) :: xs
                Document(head.copy(is), body.copy(l))
              }
            }
            case _ => Document(head, body)
          }
        } else {
          Document(head, body)
        } 
      }
    }
  }

  def head: Parser[Head] = {
    opt(head_slots) ^^ { // rep(head_slot) ^^ {
      case Some(slots) => {
        val builder = Head.builder
        for ((name, value) <- slots) {
          name.toLowerCase match {
            case "title" => builder.title = value
            case "author" => builder.author = value
            case "date" => builder.date = value
            case _ => {}
          }
        }
        builder.build
      }
      case None => Head()
    }
  }

  def head_slots: Parser[List[(String, InlineContents)]] = {
    head_title~rep(head_slot) ^^ {
      case x~xs => ("title", x) :: xs 
    }
  }

  def head_slot: Parser[(String, InlineContents)] = {
    "#+"~>"[^:]+".r~":[ ]*".r~rep(inline)<~opt(newline) ^^ {
      case name~_~value => (name, value.flatten)
    }
  }

  def head1: Parser[Head] = {
    opt(head_title)~opt(head_author) ^^ {
      case title~author => {
        val builder = Head.builder
        builder.title = title | Nil
        builder.author = author | Nil
        builder.build
      }
    }
  }

  def head_title: Parser[InlineContents] = {
    starter_colon("title")~>rep(inline)<~opt(newline) ^^ {
      case inline => inline.flatten
    }
  }

  def head_author: Parser[InlineContents] = {
    starter_colon("author")~>rep(inline)<~opt(newline) ^^ {
      case inline => inline.flatten
    }
  }

  def head0: Parser[Head] = {
    val builder = Head.builder
    def title: Parser[Unit] = {
//      starter_colon("title")~>rep(inline)<~opt(newline) ^^ {
      "#+TITLE: "~>rep(inline)<~opt(newline) ^^ {
        case inline => builder.title = inline.flatten
      }
    }
    def author: Parser[Unit] = {
      starter_colon("author")~>rep(inline)<~opt(newline) ^^ {
        case inline => builder.author = inline.flatten
      }
    }
    rep(title|author)
//    println("head: " + builder)
    success(builder.build)
  }

  def body: Parser[Body] = {
    contents~section1s ^^ {
      case contents~section1 => Body(contents ::: section1)
    }
  }

  def section1s: Parser[List[Section]] = {
    rep(section1) ^^ {
      case sections => _filter_sections(sections)
    }
  }

  private def _filter_sections(sections: List[Section]): List[Section] = {
    sections.flatMap(_filter_section)
  }

  private def _filter_section(section: Section): List[Section] = {
    section.title match {
      case (x: Text) :: _ if x.contents.startsWith("COMMENT") => Nil
      case _ => List(section)
    }
  }

  def section1: Parser[Section] = {
    "* "~>rep(inline)~opt(newline)~opt(contents)~rep(section2) ^^ {
      case title~_~contents~section2 => {
        val sections = _filter_sections(section2)
//        println("section1xs#title = " + title)
//        println("section1xs#contntents = " + contents)
//        println("section1xs#section2 = " + section2)
        Section(title.flatten, (contents | Nil) ::: sections, 1)
      }
    }
  }

  def section2: Parser[Section] = {
    "** "~>rep(inline)~opt(newline)~opt(contents)~rep(section3) ^^ {
      case title~_~contents~section3 => 
        val sections = _filter_sections(section3)
//        println("section2 = " + title + "," + contents);Section(title, (contents | Nil) ::: section3, 2)
        Section(title.flatten, (contents | Nil) ::: sections, 2)
    }
  }

  def section3: Parser[Section] = {
    "*** "~>rep(inline)~opt(newline)~opt(contents)~rep(section4) ^^ {
      case title~_~contents~section4 => 
        val sections = _filter_sections(section4)
//        println("section3 = " + title + "," + contents);Section(title, (contents | Nil) ::: section4, 3)
        Section(title.flatten, (contents | Nil) ::: sections, 3)
    }
  }

  def section4: Parser[Section] = {
    "**** "~>rep(inline)~opt(newline)~opt(contents)~rep(section5) ^^ {
      case title~_~contents~section5 =>
        val sections = _filter_sections(section5)
//        println("section4 = " + title + "," + contents);Section(title, (contents | Nil) ::: section5, 3)
        Section(title.flatten, (contents | Nil) ::: sections, 3)
    }
  }

  def section5: Parser[Section] = {
    "***** "~>rep(inline)~opt(newline)~opt(contents) ^^ {
      case title~_~contents =>
//        println("section4 = " + title + "," + contents);Section(title, contents | Nil, 3)
        Section(title.flatten, contents | Nil, 3)
    }
  }

  def contents: Parser[List[Dox]] = {
    def foldemptyline(a: List[Dox], r: List[Dox], e: Dox) =
      if (r.nonEmpty) (a :+ Paragraph(r), Nil) else (a, Nil)
    def foldstrongblock(a: List[Dox], r: List[Dox], e: Dox) =
      if (r.nonEmpty) ((a :+ Paragraph(r)) :+ e, Nil) else (a :+ e, Nil)
    def foldweakblock(a: List[Dox], r: List[Dox], e: Dox) =
      if (r.nonEmpty) (a, r :+ e) else (a :+ e, r) 
    def foldinline(a: List[Dox], r: List[Dox], e: Dox) = (a, r :+ e)
    rep(commentline|embedded|contentsline|emptyline) ^^ {
      case contents => {
        val (a, r) = contents.flatten.foldLeft(Pair(List.empty[Dox], List.empty[Dox])) {
          case ((a, r), e) => e match {
            case _: EmptyLine => foldemptyline(a, r, e)
            case _: Inline => foldinline(a, r, e)
            case _: Ul => foldweakblock(a, r, e)
            case _: Ol => foldweakblock(a, r, e)
            case _: Dl => foldweakblock(a, r, e)
            case _ => foldstrongblock(a, r, e)
          }
        }
        if (r.nonEmpty) a :+ Paragraph(normalize_paragraph(r)) else a
      }
    }
  }

  def normalize_paragraph(xs: List[Dox]): List[Dox] = {
    def concat(lhs: String, rhs: String): String = {
      if (isWordSeparate(lhs, rhs)) lhs + " " + rhs
      else lhs + rhs
    }
    val a = xs.foldRight((List.empty[Dox], "")) { (x, a) =>
      val (l, s) = a
      x match {
        case t: Text => {
          if (s.isEmpty()) {
            if (isWordSeparate(t.contents, l)) (l, t.contents + " ")
            else (l, t.contents) 
          } 
          else (l, concat(t.contents, s)) 
        }
        case sp: Space => {
          if (s.isEmpty()) (l, "")
          else if (s.charAt(0) == ' ') (l, s)
          else (l, " " + s)
        }
        case _ => {
          if (s.isEmpty()) {
            if (isWordSeparate(x, l)) (x :: Text(" ") :: l, "")            
            else (x :: l, "")  
          } else {
            if (isWordSeparate(x, s)) (x :: Text(" " + s) :: l, "") 
            else (x :: Text(s) :: l, "") 
          }
        }
      }
    }
    a match {
      case (l, s) if s.nonEmpty => Text(s) :: l 
      case (l, s) => l
    }
  }

  def contentsline: Parser[List[Dox]] = {
    not("[*]+[ ]".r)~>rep1(block_as_contents|inline)<~opt(newline) ^^ {
      case contents => contents.flatten.flatMap {
        case fra: Fragment => fra.contents
        case block: Block => List(block)
        case inline => List(inline)
      }
    }
  }

  def embedded: Parser[List[Dox]] = rep1(img_dot|img_ditaa|img_sm)

  def block: Parser[Block] = dl|ulol|table|commentblock|figure|console|program|includeprogram|includedoc|div_xml

  def block_as_contents: Parser[List[Block]] = block.map(x => List(x))

  def emptyline: Parser[List[EmptyLine]] = {
    newline ^^ {
      case _ => List(EmptyLine())
    }
  } 

  def commentline: Parser[List[Dox]] = {
    "[#][^+]".r~>"[^\n\r]*".r<~newline ^^ {
      case _ => Nil
    }
  } 

  sealed trait ListLine {
    val indent: Int
    val contents: List[ListContent]
  }
  case class UlLine(indent: Int, contents: List[ListContent]) extends ListLine
  case class OlLine(indent: Int, contents: List[ListContent]) extends ListLine
  case class ContListLine(contents: List[ListContent]) extends ListLine {
    val indent = 0
  }
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
        case term~_~desc => (Dt(term.flatten.toText), Dd(desc.flatten))
      }
    }
    rep1(dline) ^^ {
      case line => Dl(line)
    }
  }

  def ulol: Parser[Block] = {
    def uoline: Parser[ListLine] = {
      ("[ ]*[-][ ]".r|"""[ ]*\d+[.][ ]""".r)~rep(inline)<~opt(newline) ^^ {
        case indent~contents => ListLine(indent, contents.flatten)
      } 
    }
    def lines2lis(lines: List[ListLine]): List[Li] = {
      require (!lines.isEmpty, "ul/ol lines are empty")
      var current = lines
      var previ = current.head.indent

      def parse(): List[Li] = {
        val lis = new ArrayBuffer[Li]
//        println("current = " + current)
        do {
          val line = current.head
          if (line.indent == previ) {
            val li = Li(line.contents)
            lis += li
            previ = line.indent
            current = current.tail
//            println("currentx = " + current)
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
              case _ => sys.error("not reached.")
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
    def contline: Parser[ContListLine] = {
      "[ ]+".r~>rep(inline)<~(newline) ^^ {
        case inline => ContListLine(inline.flatten)
      }
    }
    def concat(l: List[ListContent], r: List[ListContent]): List[ListContent] = {
      normalize_paragraph(l ::: r) collect { case x: ListContent => x }
    }
    uoline~rep(uoline|contline) ^^ {
      case head~tail => {
        val a = tail.foldLeft(List(head)) {
          (a, x) => {
            x match {
              case c: ContListLine => a.head match {
                case l: UlLine => l.copy(l.indent, concat(l.contents, c.contents)) :: a.tail
                case o: OlLine => o.copy(o.indent, concat(o.contents, c.contents)) :: a.tail
                case _ => sys.error("not reached")
              }
              case _ => x :: a
            }
          }
        }
        val b = a.reverse
        b.head match {
          case _: UlLine => Ul(lines2lis(b))
          case _: OlLine => Ol(lines2lis(b))
          case _ => sys.error("not reached")
        }
      }
//      case line => line.head match {
//        case _: UlLine => Ul(lines2lis(line))
//        case _: OlLine => Ol(lines2lis(line))
//      }
    }
  }

  sealed trait TableLine
  case class FrameTableLine() extends TableLine
  case class DataTableLine(contents: List[List[Inline]]) extends TableLine
  case class IncludeTableLine(uri: String, params: List[String]) extends TableLine

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
    starter_colon("caption")~>rep(inline)<~newline ^^ {
      case value => CaptionAttribute(value.flatten)
    }
  }

  def attrlabel: Parser[Attribute] = {
    starter_colon("label")~>"[^\n\r]*".r<~newline ^^ {
      case value => LabelAttribute(value)
    }
  }

  def attrhtml: Parser[Attribute] = {
    starter_colon("attr_html")~>"[^\n\r]*".r<~newline ^^ {
      case value => HtmlAttribute(value)
    }
  }

  def attrlatex: Parser[Attribute] = {
    starter_colon("attr_latex")~>"[^\n\r]*".r<~newline ^^ {
      case value => LatexAttribute(value)
    }
  }

  def floatattr=attrcaption|attrlabel|attrhtml|attrlatex

  def attr_slot: Parser[(String, String)] = {
    "#+"~>"[^:]+".r~"[:][ ]*".r~"[^\n\r]*".r<~newline ^^ {
      case name~_~value => (name.trim.toLowerCase, value) // ensuring{x => println("attr_slot:" + x);true}
    }
  }

  def table: Parser[TableBlock] = {
    def tableattrs: Parser[List[Attribute]] = rep(floatattr)
    def tableattrs0: Parser[List[Attribute]] = {
      rep(attr_slot) ^^ {
        case attrs => {
          val a = for ((name, value) <- attrs) yield {
            name match {
              case "caption" => CaptionAttribute(List(Text(value))).some
              case "label" => LabelAttribute(value).some
              case "attr_html" => HtmlAttribute(value).some
              case "attr_latex" => LatexAttribute(value).some
              case _ => None
            }
          }
          a.flatten
        }
      }
    }
    def tablelines: Parser[List[TableLine]] = {
      rep1(frameline|dataline|includetable)
    }
    def tableline: Parser[TableLine] = frameline|dataline
    def frameline: Parser[FrameTableLine] = {
      "[|][-][^\n\r]*".r<~opt(newline) ^^ {
        case _ => FrameTableLine()
      }
    }
    def dataline: Parser[DataTableLine] = {
      "|"~>repsep(rep(inline_table), "|")<~opt(newline) ^^ {
        case lines => {
          val ls = normalize_space(lines.map(_.flatten))
          if (ls.last.nonEmpty) DataTableLine(ls)
          else DataTableLine(ls.dropRight(1))
        }
      }
    }
    def normalize(lines: List[TableLine]): List[TableLine] = {
      val (_, frameremovedreverse) = lines.foldLeft((FrameTableLine(): TableLine, List.empty[TableLine])) {
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
    def build(lines: List[TableLine]): Either[TTable, (Option[THead], TBody, Option[TFoot])] = {
      val aggregatedreverse = lines.foldLeft(List.empty[List[TableLine]]) {
        case (a, e) => {
          e match {
            case _: FrameTableLine => Nil :: a 
            case d: DataTableLine => if (a == Nil) List(List(d)) else (d :: a.head) :: a.tail
            case t: IncludeTableLine => if (a == Nil) List(List(t)) else (t :: a.head) :: a.tail
          }
        }
      }
      val aggregated = aggregatedreverse.foldLeft(List.empty[List[TableLine]]) {
        case (a, e) => e.reverse :: a
      }
      val ttabled = aggregated.headOption.flatMap(_.headOption) collect {
        case x: IncludeTableLine => x 
      }
      ttabled match {
        case Some(x) => Left(TTable(x.uri, x.params))
        case None => Right(maketable(aggregated))
      }
    }
    def datarecords(lines: List[TableLine]): List[TRecord] = {
      lines.collect {
        case d: DataTableLine => TR(d.contents.map(TD(_)))
        case d: IncludeTableLine => TTable(d.uri, d.params)
      }
    }
    def headrecords(lines: List[TableLine]): List[TRecord] = {
      lines.collect {
        case d: DataTableLine => TR(d.contents.map(TH(_)))
        case d: IncludeTableLine => TTable(d.uri, d.params)
      }
    }
    def datarecords0(lines: List[TableLine]): List[TR] = {
      val data = lines.collect {
        case d: DataTableLine => d
      }
      for (d <- data) yield {
        TR(d.contents.map(TD(_)))
      }
    }
    def headrecords0(lines: List[TableLine]): List[TR] = {
      val data = lines.collect {
        case d: DataTableLine => d
      }
      for (d <- data) yield {
        TR(d.contents.map(TH(_)))
      }
    }
    def maketable(aggregated: List[List[TableLine]]) = {
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
//    println("try table")
    tableattrs~tablelines ^^ {
      case attrs~lines => {
//        println("attrs: " + attrs)
        val normalized = normalize(lines)
        val caption = attrs.collectFirst {
          case c: CaptionAttribute => c.value
        }.map(Caption(_))
        val label = attrs.collectFirst {
          case c: LabelAttribute => c.value
        }
        build(normalized) match {
          case Right((head, body, foot)) =>
            Table(head, body, foot, None, None, caption, label)
          case Left(ttable) => ttable.copy(caption = caption, label = label)
        }
      }
    }
  }

  def figure: Parser[Figure] = {
    rep1(floatattr)~(img|img_dot|img_ditaa|img_sm)<~opt(newline) ^^ {
      case attrs~img => {
        val caption = attrs.collectFirst {
          case c: CaptionAttribute => c.value
        }.map(Figcaption(_)) | Figcaption(Nil)
        val label = attrs.collectFirst {
          case c: LabelAttribute => c.value
        }
        Figure(img, caption, label)
      }
    } ^^ {
      case fig if fig.caption.contents.nonEmpty => fig
    }
  }

  def includetable: Parser[TableLine] = {
    starter_colon("table")~"\""~>"""[^"]+""".r~"\""~"[ ]*".r~repsep("[^ \n\r]+".r, "[ ]+".r)<~opt(newline) ^^ {
      case filename~_~_~params => {
        IncludeTableLine(filename, params)
      }
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

  def beginendnop[T](name: String)(body: => T): Parser[T] = {
    val upper = name.toUpperCase
    ("#+BEGIN_" + upper|"#+begin_" + name)~"[^ \n\r]*".r~>newline~embedlines<~("#+END_" + upper|"#+end_" + name)~"[ ]*".r~opt(newline) ^^ {
      case _~contents => {
        body
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
        Program("", VectorMap("src" -> filename))
      }
    }
  }

  def includedoc: Parser[IncludeDoc] = {
    starter_colon("include")~"\""~>"""[^"]+""".r<~"\""~opt(newline) ^^ {
      case filename => {
        IncludeDoc(filename)
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

  def commentblock: Parser[Block] = {
    beginendnop("comment") {
      Fragment(Nil)
    }
  }

  def startercolon0(name: String): Parser[String] = {
    val upper = name.toUpperCase
    ("#+" + upper + ":"|"#+" + name + ":")<~"[ ]*".r
  }

  def div_xml: Parser[Div] = {
    inline_xml("div") ^^ {
      case elem => Div(elem.contents)
    }
  }

  def inline: Parser[InlineContents] = (special_literals|space|text|inline_elements)

  def inline_table: Parser[InlineContents] = (special_literals|space|text_table|inline_elements)

  def inline_hyperlink: Parser[InlineContents] = (space|text_hyperlink|inline_elements) // inline_hyperlink

  def inline_elements: Parser[InlineContents] = (bold|italic|underline|code|pre|del|
      literal_inline|
      span_xml|bold_xml|italic_xml|underline_xml|code_xml|pre_xml|del_xml|
      tt_xml|t_xml|
      img_as_contents|not_hyperlink|hyperlink|hyperlink_xml)

  def special_literals: Parser[InlineContents] = {
    hyperlink_literal | file_literal // | greater_than | less_than
  }

  def space: Parser[InlineContents] = {
    "[ ]+".r ^^ {
      case _ => List(Space())
    }
  }

  def text: Parser[List[Text]] = {
    // special charactors: :|]
//    """[^*/_=~+<>\[\] :|\n\r]+""".r ^^ {
    """[^*/_=~+<\[ \n\r]+""".r ^^ {
      case s => 
//        println("s = " + s);Text(s)
        List(Text(s))
    }
  }

  def text_table: Parser[List[Text]] = {
    """[^*/_=~+<\[ |\n\r]+""".r ^^ {
      case s => 
//        println("s = " + s);Text(s)
        List(Text(s))
    }
  }

  def text_hyperlink: Parser[List[Text]] = {
    """[^*/_=~+<\[\] |\n\r]+""".r ^^ {
      case s => {
//        println("s = " + s);Text(s)
        List(Text(s))
      }
    }
  }

  def literal_inline: Parser[List[Text]] = {
    "<["~>"""[^]]*""".r<~"]>" ^^ {
      case text => {
        List(Text(text))
      } 
    }
  }

  def span_xml: Parser[InlineContents] = {
    inline_xml("span") ^^ {
      case elem => List(Span(elem.contents))
    }
  }

  def bold: Parser[InlineContents] = text_markup("*", Bold(_))

  def bold0: Parser[Inline] = {
    "*"~>text_until("*")~opt("*"|newline) ^^ {
      case text~mark => {
        mark match {
          case Some(m) if m == "*" => Bold(Text(text))
          case _ => Text("*" + text)
        }
      } 
    }
  }

  def text_markup0(delim: String, elem: Text => Inline): Parser[InlineContents] = {
    " "~delim~>text_until(delim)~delim~"""[ \n\r.]""".r ^^ {
      case text~_~post => {
        post match {
          case "." => List(elem(Text(text)), Text(" "))
          case _ => List(elem(Text(text)))
        }
      } 
    }
  }

  def text_markup(delim: String, elem: Text => Inline): Parser[InlineContents] = {
    delim~>text_until(delim)~opt(delim) ^^ {
      case text~mark => {
        mark match {
          case Some(m) if m == delim => List(elem(Text(text)))
          case _ => List(Text(delim + text))
        }
      } 
    }
  }

  def text_until(delim: String): Regex = {
    ("""[^%s\]<|\n\r ]*""".format(delim)).r
  }

  def bold_xml: Parser[InlineContents] = {
    inline_xml("b") ^^ {
      case elem => List(Bold(elem.contents))
    }
  }

  case class XElement[T <: Dox](params: List[XParam], contents: List[T])
  case class XParam(name: String, value: String)

  def inline_xml(name: String): Parser[XElement[Inline]] = {
    "<"~opt(whiteSpace)~name~xml_params~opt(whiteSpace)~">"~rep(inline)~"</"~opt(whiteSpace)~name~opt(whiteSpace)~">" ^^ {
      case _~_~_~params~_~_~contents~_~_~_~_~_ => XElement(params, contents.flatten)
    }
  }

  def contents_xml(name: String): Parser[XElement[Dox]] = {
    def xmlparams: Parser[List[XParam]] = {
      rep(xml_param)
    }
    "<"~opt(whiteSpace)~name~xml_params~opt(whiteSpace)~">"~contents~"</"~opt(whiteSpace)~name~opt(whiteSpace)~">" ^^ {
      case _~_~_~params~_~_~contents~_~_~_~_~_ => XElement(params, contents)
    }
  }

  def text_xml(name: String): Parser[(List[XParam], String)] = {
    "<"~opt(whiteSpace)~name~xml_params~opt(whiteSpace)~">"~"""[^<]*""".r~"</"~opt(whiteSpace)~name~opt(whiteSpace)~">" ^^ {
      case _~_~_~params~_~_~contents~_~_~_~_~_ => (params, contents)
    }
  }

  def xml_param: Parser[XParam] = {
    " "~opt(whiteSpace)~"""\w+""".r~opt(whiteSpace)~"="~opt(whiteSpace)~'"'~"""[^"]*""".r~'"' ^^ {
      case _~_~name~_~_~_~_~value~_ => XParam(name, value)
    }
  }

  def xml_params: Parser[List[XParam]] = {
      rep(xml_param)
  }

  def italic: Parser[InlineContents] = text_markup("/", Italic(_))

  def italic0: Parser[Inline] = {
    "/"~>rep(inline)<~"/" ^^ {
      case inline if (inline.isEmpty) => Text("/")
      case inline => Italic(inline.flatten)
    }
  }

  def italic_xml: Parser[InlineContents] = {
    inline_xml("i") ^^ {
      case elem => List(Italic(elem.contents))
    }
  }

  def underline: Parser[InlineContents] = {
    if (useUnderline)
      text_markup("_", Underline(_))
    else
      text_markup("_____", Underline(_)) // XXX
  }

  def underline0: Parser[Inline] = {
    "_"~>rep(inline)<~"_" ^^ {
      case inline if (inline.isEmpty) => Text("_")
      case inline => Underline(inline.flatten)
    }
  }

  def underline_xml: Parser[InlineContents] = {
    inline_xml("u") ^^ {
      case elem => List(Underline(elem.contents))
    }
  }

  def code: Parser[InlineContents] = text_markup("=", Code(_))
  
  def code0: Parser[Inline] = {
    "="~>rep(inline)<~"=" ^^ {
      case inline if (inline.isEmpty) => Text("=")
      case inline => Code(inline.flatten)
    }
  }

  def code_xml: Parser[InlineContents] = {
    inline_xml("code") ^^ {
      case elem => List(Code(elem.contents))
    }
  }

  def pre: Parser[InlineContents] = text_markup("~", (t => Pre(t.contents)))

  def pre0: Parser[Inline] = {
    "~"~>rep(inline)<~"~" ^^ {
      case inline if (inline.isEmpty) => Text("~")
      case inline => Pre(inline.mkString)
    }
  }

  def pre_xml: Parser[InlineContents] = {
    inline_xml("pre") ^^ {
      case elem => List(Pre(elem.contents.mkString))
    }
  }

  def del: Parser[InlineContents] = text_markup("+", Del(_))

  def del0: Parser[Inline] = {
    "+"~>rep(inline)<~"+" ^^ {
      case inline if (inline.isEmpty) => Text("+")
      case inline => Del(inline.flatten)
    }
  }

  def del_xml: Parser[InlineContents] = {
    inline_xml("del") ^^ {
      case elem => List(Del(elem.contents))
    }
  }

  def tt_xml: Parser[InlineContents] = {
    inline_xml("tt") ^^ {
      case elem => List(Tt(elem.contents))
    }
  }

  def t_xml: Parser[InlineContents] = {
    text_xml("t") ^^ {
      case (params, contents) => List(Text(contents))
    }
  }

  def not_hyperlink: Parser[InlineContents] = {
    "["~not("[") ^^ {
      case _ => List(Text("["))
    }
  }

  def hyperlink0: Parser[Inline] = (hyperlink1|hyperlink1x|hyperlink2)

  def hyperlink1: Parser[Inline] = {
    "[["~>"""[^]]+""".r~"]["~rep(inline_hyperlink)<~"]]" ^^ {
      case link~_~label => {
        if (label.isEmpty) Hyperlink(List(Text(link)), new URI(link))
        else Hyperlink(label.flatten, new URI(link))
      }
    }
  }

  def hyperlink1x: Parser[Inline] = {
    "[["~>"""[^]]+""".r~"]["~rep(inline_hyperlink)<~"]" ^^ {
      case link~_~label => {
        if (label.isEmpty) Hyperlink(List(Text(link)), new URI(link))
        else Hyperlink(label.flatten, new URI(link))
      }
    }
  }

  def hyperlink2: Parser[Inline] = {
    "[["~>"""[^]]+""".r<~"]]" ^^ {
      case link => Hyperlink(List(Text(link)), new URI(link))
    }
  }

  def hyperlink: Parser[InlineContents] = {
    def label: Parser[List[InlineContents]] = {
      "["~>rep(inline_hyperlink)<~"]"
    }
    "[["~>"""[^]]+""".r~"]"~opt(label)<~opt("]") ^^ {
      case link~_~label => {
        val s = label | List(List(Text(link)))
        List(Hyperlink(s.flatten, new URI(link)))
      }
    }
  }

  def hyperlink_xml: Parser[List[Hyperlink]] = {
    def warning = "***No href***"
    inline_xml("a") ^^ {
      case elem => {
        val href = elem.params.find(_.name == "href").map(_.value)
        val txt = href ? elem.contents | elem.contents :+ Text(warning)
        List(Hyperlink(txt, new URI(href | "")))
      }
    }
  }

  def hyperlink_literal0: Parser[Hyperlink] = {
    log("http://")("hyperlink_literal") ^^ {
      case l => val uri = l + "";Hyperlink(List(Text(uri)), new URI(uri))
    }
  }

  def hyperlink_literal: Parser[InlineContents] = {
    """(http|https)://[^ \n\r]*""".r ^^ {
      case uri if _is_image(uri) => List(ReferenceImg(new URI(uri)))
      case uri => List(Hyperlink(List(Text(uri)), new URI(uri)))
    }
  }

  def file_literal: Parser[InlineContents] = {
    def adjust(s: String) = {
      s.substring("file:".length)
    }
    """(file):[^ \n\r]*""".r ^^ {
      case uri if _is_image(uri) => {
        List(ReferenceImg(new URI(adjust(uri))))
      }
      case uri => {
        val u = adjust(uri)
        List(Hyperlink(List(Text(u)), new URI(u)))
      }
    }
  }

  private def _is_image(s: String) = {
    List("png", "gif", "jpeg", "jpg").exists(x => s.endsWith("." + x))
  }

  def hyperlink_literal1: Parser[Hyperlink] = {
    "http://www.yahoo.com/" ^^ {
      case uri => Hyperlink(List(Text(uri)), new URI(uri))
    }
  }

  @deprecated("unused", "20120607")
  def greater_than: Parser[Text] = {
    ">>" ^^ {
      case _ => Text(">&&")
    }
  }

  @deprecated("unused", "20120607")
  def less_than: Parser[Text] = {
    "<<" ^^ {
      case _ => Text("<%%")
    }
  }
  
  def img: Parser[Img] = {
    "[["~>"""[^]]+[.]""".r~img_suffix<~"]]" ^^ {
      case name~suffix => ReferenceImg(new URI(name+suffix))
    }
  }

  def img_as_contents: Parser[List[Img]] = img.map(x => List(x))

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

  // derived from USmartDoc
  def isWordSeparateLang(c: Char): Boolean = {
    val ub = Character.UnicodeBlock.of(c)
    ub.equals(Character.UnicodeBlock.BASIC_LATIN) ||
    ub.equals(Character.UnicodeBlock.LATIN_1_SUPPLEMENT) ||
    ub.equals(Character.UnicodeBlock.LATIN_EXTENDED_A) ||
    ub.equals(Character.UnicodeBlock.LATIN_EXTENDED_B)
  }   

  def isWordSeparate(before: Dox, after: Dox): Boolean = {
    isWordSeparate(before.toText, after.toText)
  }

  def isWordSeparate(before: String, after: Dox): Boolean = {
    isWordSeparate(before, after.toText)
  }

  def isWordSeparate(before: Dox, after: String): Boolean = {
    isWordSeparate(before.toText, after)
  }

  def isWordSeparate(before: String, after: String): Boolean = {
      if (before.length() == 0) {
        return (false)
      }
      if (after.length() == 0) {
        return (false)
      }
      val bc = before.charAt(before.length() - 1)
      val ac = after.charAt(0)
      bc != ' ' && ac != ' ' &&
      bc != '.' && ac != '.' &&
      bc != '[' && ac != ']' &&
      isWordSeparateLang(bc) &&
      isWordSeparateLang(ac) // ensuring{x => println("isWordSeparate(%s, %s) = %s".format(before, after, x));true}
    }
}

object DoxParser extends DoxParser(true) // false
