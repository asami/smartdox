package org.smartdox.parser

import scalaz._, Scalaz._
import java.io.Reader
import java.net.URI
import scala.xml.{Node => XNode, _}
import org.smartdox._
import Dox._, Doxes._

/**
 * @since   Dec.  5, 2012
 * @version Dec.  5, 2012
 * @author  ASAMI, Tomoharu
 */
object PureParser {
  def parse(reader: Reader): Validation[NonEmptyList[String], Dox] = {
    val elem = XML.load(reader)
//    println("PureParser#parse = " + elem)
    build(elem).success
  }

  def build(elem: XNode): Dox = {
//    println("PureParser#build = " + elem)
    println("... = " + elem.label)
    elem.label match {
      case "html" => buildDocument(elem)
      case "head" => buildHead(elem)
      case "body" => buildBody(elem)
      case "section" => buildSection(elem)
      case "div" => buildDiv(elem)
      case "text" => buildText(elem)
      case "b" => buildBold(elem)
      case "i" => buildItalic(elem)
      case "u" => buildUnderline(elem)
      case "code" => buildCode(elem)
      case "ul" => buildUl(elem)
      case "ol" => buildOl(elem)
      case "li" => buildLi(elem)
      case "del" => buildDel(elem)
      case "a" => buildHyperlink(elem)
      case "img" => buildReferenceImg(elem)
      case "table" => buildTable(elem)
      case "thead" => buildTHead(elem)
      case "tbody" => buildTBody(elem)
      case "tfoot" => buildTFoot(elem)
      case "tr" => buildTR(elem)
      case "td" => buildTD(elem)
      case "th" => buildTH(elem)
      case "space" => buildSpace(elem)
      case "dl" => buildDl(elem)
      case "dt" => buildDt(elem)
      case "dd" => buildDd(elem)
      case "fragment" => buildFragment(elem)
      case "caption" => buildCaption(elem)
      case "figure" => buildFigure(elem)
      case "figcaption" => buildFigcaption(elem)
      case "emptyline" => buildEmptyLine(elem)
      case "newline" => buildNewline(elem)
//      case "img" => buildImg(elem)
      case "program" => buildProgram(elem)
      case "pre" => buildProgram(elem)
      case "console" => buildConsole(elem)
      case "smartdoc" => buildSmartdoc(elem)
      case "tt" => buildTt(elem)
      case "span" => buildSpan(elem)
      case "includedoc" => buildIncludeDoc(elem)
      case "#PCDATA" => buildText(elem)
      case _ => sys.error("bad element = " + elem)
    }
  }

  def buildDocument(elem: XNode): org.smartdox.Document = {
//    println("PureParser#buildDocument = " + elem)
//    println("... = " + elem.child.find(_.label == "head"))
    val head: Head = elem.child.find(_.label == "head").map(buildHead) | Head()
    val body: Body = elem.child.find(_.label == "body").map(buildBody) | Body()
    Document(head, body)
  }

  def buildHead(elem: XNode): Head = {
    Head(getTitle(elem), getAuthor(elem), getDate(elem))
  }

  def getTitle(elem: XNode): InlineContents = {
    getInline("title", elem)
  }

  def getAuthor(elem: XNode): InlineContents = {
    getInline("author", elem)
  }

  def getDate(elem: XNode): InlineContents = {
    getInline("date", elem)
  }

  def getInline(label: String, elem: XNode): InlineContents = {
    elem.child.find(_.label == label).map(buildInline) | Nil
  }

  def buildInline(elem: XNode): InlineContents = {
    elem.child.map(build).toList collect {
      case i: Inline => i
    }
  }

  def buildBody(elem: XNode): Body = {
    Body(buildChildren(elem))
  }

  def buildChildren(elem: XNode): List[Dox] = {
    elem.child.map(build).toList
  }

  def buildSection(elem: XNode): Section = {
//    println("PureParser#buildSection = " + elem)
    val header = getHeader(elem)
    val contents = elem.child.filter(x => {
      val label = x.label
      if (label.startsWith("h1") ||
          label.startsWith("h2") ||
          label.startsWith("h3") ||
          label.startsWith("h4") ||
          label.startsWith("h5") ||
          label.startsWith("h6") ||
          label.startsWith("h7") ||
          label.startsWith("h8") ||
          label.startsWith("h9")) false
      else true
    }).map(build)
    Section(header._2, contents.toList, header._1)
  }

  def getHeader(elem: XNode): (Int, InlineContents) = {
    elem.child.collectFirst {
      case x if x.label == "h1" => (0, buildInline(x))
      case x if x.label == "h2" => (1, buildInline(x))
      case x if x.label == "h3" => (2, buildInline(x))
      case x if x.label == "h4" => (3, buildInline(x))
      case x if x.label == "h5" => (4, buildInline(x))
      case x if x.label == "h6" => (5, buildInline(x))
      case x if x.label == "h7" => (6, buildInline(x))
      case x if x.label == "h8" => (7, buildInline(x))
      case x if x.label == "h9" => (8, buildInline(x))
    } match {
      case Some(x) => x
      case None => sys.error("???")
    }
  }

  def buildDiv(elem: XNode): Div = {
    Div(build(elem))
  }

  def buildText(elem: XNode): org.smartdox.Text = {
    org.smartdox.Text(elem.text)
  }

  def buildBold(elem: XNode): Bold = {
    Bold(buildInline(elem))
  }

  def buildItalic(elem: XNode): Italic = {
    Italic(buildInline(elem))
  }

  def buildUnderline(elem: XNode): Underline = {
    Underline(buildInline(elem))
  }

  def buildCode(elem: XNode): Code = {
    Code(buildInline(elem))
  }

  def buildPre(elem: XNode): Pre = {
    Pre(elem.text, getAttributes(elem))
  }

  def getAttributes(elem: XNode): List[(String, String)] = {
    elem.attributes.map(x => (x.key, x.value.text)).toList
  }

  def buildUl(elem: XNode): Ul = {
    Ul(buildLis(elem))
  }

  def buildLis(elem: XNode): List[Li] = {
    elem.withFilter(_.label == "li").map(buildLi).toList
  }

  def buildOl(elem: XNode): Ol = {
    Ol(buildLis(elem))
  }

  def buildLi(elem: XNode): Li = {
    Li(buildList(elem))
  }

  def buildList(elem: XNode): List[ListContent] = {
    elem.map(build).toList collect {
      case l: ListContent => l
    }
  }

  def buildDel(elem: XNode): Del = {
    Del(buildInline(elem))
  }

  def buildHyperlink(elem: XNode): Hyperlink = {
    val href = getAttribute(elem, "href") | ""
    Hyperlink(buildInline(elem), new URI(href))
  }

  def getAttribute(elem: XNode, name: String): Option[String] = {
    elem.attribute(name).map(_.text)
  }

  def buildReferenceImg(elem: XNode): ReferenceImg = {
    val src = getAttribute(elem, "src") | ""
    ReferenceImg(new URI(src))
  }

  def buildTable(elem: XNode): Table = {
    val head = getTHead(elem)
    val body = getTBody(elem)
    val foot = getTFoot(elem)
    val caption = getCaption(elem)
    val label = getLabel(elem)
    Table(head, body, foot, caption, label)
  }

  def getTHead(elem: XNode): Option[THead] = {
    elem.child.find(_.label == "thead").map(buildTHead)
  }

  def getTBody(elem: XNode): TBody = {
    elem.child.find(_.label == "tbody").map(buildTBody) | TBody(Nil) // XXX
  }

  def getTFoot(elem: XNode): Option[TFoot] = {
    elem.child.find(_.label == "tfoot").map(buildTFoot)
  }

  def getCaption(elem: XNode): Option[Caption] = {
    elem.child.find(_.label == "caption").map(buildCaption)
  }

  def getLabel(elem: XNode): Option[String] = {
    println("PureParser#getLabel = " + getAttribute(elem, "id"))
    getAttribute(elem, "id")
  }

  def buildTHead(elem: XNode): THead = {
    THead(buildRecords(elem))
  }

  def buildTBody(elem: XNode): TBody = {
    TBody(buildRecords(elem))
  }

  def buildTFoot(elem: XNode): TFoot = {
    TFoot(buildRecords(elem))
  }

  def buildRecords(elem: XNode): List[TRecord] = {
    elem.child.map(build).collect {
      case x: TRecord => x
    } toList
  }

  def buildTR(elem: XNode): TR = {
    TR(buildFields(elem))
  }

  def buildFields(elem: XNode): List[TField] = {
    elem.child.map(build).collect {
      case x: TField => x
    } toList
  }

  def buildTD(elem: XNode): TD = {
    TD(buildInline(elem))
  }

  def buildTH(elem: XNode): TH = {
    TH(buildInline(elem))
  }

  def buildSpace(elem: XNode): Space = {
    Space()
  }

  def buildDl(elem: XNode): Dl = {
    Dl(buildDlContents(elem))
  }

  def buildDlContents(elem: XNode): List[(Dt, Dd)] = {
    val xs = elem.child.filter(_ match {
      case _: Dt => true
      case _: Dd => true
      case _ => false
    })
    buildDlContents(xs, Vector()).toList
  }

  def buildDlContents(xs: Seq[XNode], r: Vector[(Dt, Dd)]): Vector[(Dt, Dd)] = {
    xs match {
      case Nil => r
      case (t: Dt) :: (d: Dd) :: xs => buildDlContents(xs, r :+ (t, d))
      case (t: Dt) :: (t2: Dt) :: xs => buildDlContents(t2 :: xs, r :+ (t, Dd(Nil)))
      case (t: Dt) :: Nil => r :+ (t, Dd(Nil))
      case (d: Dd) :: xs => buildDlContents(xs, r :+ (Dt(""), d))
    }
  }

  def buildDt(elem: XNode): Dt = {
    Dt(elem.text)
  }

  def buildDd(elem: XNode): Dd = {
    Dd(buildInline(elem))
  }

  def buildFragment(elem: XNode): Fragment = {
    Fragment(elem.child.map(build).toList)
  }

  def buildCaption(elem: XNode): Caption = {
    Caption(buildInline(elem))
  }

  def buildFigure(elem: XNode): Figure = {
    val img: Option[Img] = getContent("img", elem)
    val caption: Option[Figcaption] = getContent("figcaption", elem)
    val label: Option[String] = getLabel(elem)
    Figure(img.get, caption.get, label) // XXX
  }

  def getContent[T <: Dox](label: String, elem: XNode): Option[T] = {
    elem.child.find(_.label == label).map(build).map(_.asInstanceOf[T])
  }

  def buildFigcaption(elem: XNode): Figcaption = {
    Figcaption(buildInline(elem))
  }

  def buildEmptyLine(elem: XNode): EmptyLine = {
    EmptyLine()
  }

  def buildNewline(elem: XNode): Newline = {
    Newline()
  }

  def buildImg(elem: XNode): Img = {
    throw new UnsupportedOperationException()
  }

  def buildProgram(elem: XNode): Program = {
//    println("PureParser#buildProgram = " + elem)
    Program(elem.text, getAttributes(elem))
  }

  def buildConsole(elem: XNode): Console = {
    Console(elem.text, getAttributes(elem))
  }

  def buildSmartdoc(elem: XNode): SmartDoc = {
    val name = getAttribute(elem, "name") | ""
    val attrs = getAttributes(elem)
    val cs = elem.child.map(build)
    SmartDoc(name, attrs, cs.toList)
  }

  def buildTt(elem: XNode): Tt = {
    Tt(buildInline(elem))
  }

  def buildSpan(elem: XNode): Span = {
    Span(buildInline(elem))
  }

  def buildIncludeDoc(elem: XNode): IncludeDoc = {
    val fn = getAttribute(elem, "filename") | ""
    IncludeDoc(fn)
  }
}
