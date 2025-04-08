package org.smartdox.transformers

import scalaz._, Scalaz._
import java.net.URI
import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.Parsers
import scala.collection.mutable.ArrayBuffer
import org.w3c.dom.{Node, Element, Comment}
import org.w3c.dom.{Text => DomText}
import org.goldenport.RAISE
import org.goldenport.xml.{XmlAttributes, XmlAttribute}
import org.goldenport.xml.dom.DomFactory
import org.goldenport.value._
import org.goldenport.hocon.HoconUtils
import org.goldenport.util.AnyUtils
import org.smartdox._
import Dox._
import org.smartdox.generator.Context
import org.smartdox.transformer._

/*
 * @since   Nov.  3, 2020
 *  version Nov. 29, 2020
 *  version Dec. 27, 2020
 *  version Jan. 17, 2021
 *  version Feb.  8, 2021
 * @version Apr.  6, 2025
 * @author  ASAMI, Tomoharu
 */
class Dox2DomHtmlTransformer(
  context: Context,
  rule: Dox2DomHtmlTransformer.Rule
) extends DoxTransformer {
  type Out = Node

  private lazy val _rule_context = new Dox2DomHtmlTransformer.Rule.Context(this)
  private[transformers] val _factory = DomFactory.createHtml()
  private val _newline = "\n" // TODO
  private val _base_section_header = rule.sectionBaseNumber getOrElse 1 // H1
  private var _section_depth = 0

  def doxOut(p: Dox) = p match {
    case m: Document => documentOut(m)
    case m: Div => divOut(m)
    case m => RAISE.notImplementedYetDefect
  }

  def documentOut(d: Document) = {
    // println(s"Dox2DomHtmlTransform#documentOut: $d")
    val doc = _factory.document
    val title = _get_inline(d.head.title)
    val h = headOut(d.head)
    val b = bodyOut(d.body, title)
    val root = _factory.element("html")
    root.appendChild(h)
    root.appendChild(b)
    doc.appendChild(root)
    // println(s"Dox2DomHtmlTransform#documentOut: ${org.goldenport.xml.dom.DomUtils.toString(doc)}")
    doc
  }

  def headOut(p: Head): Out = {
    val xs1 = Vector(
      _head_title(p),
      _head_author(p),
      _head_date(p),
      _head_css(p)
    ).flatten ++ _head_styles(p)
    val properties = HoconUtils.toFlattenVector(p.properties).map {
      case (k, v) =>
        val attrs = Map("name" -> k, "content" -> AnyUtils.toPrint(v))
        create_element("meta", attrs)
    }
    val xs = xs1 ++ properties
    create_element("head", p.attributes, xs)
  }

  private def _head_title(p: Head): Option[Element] =
    _get_inline(p.title).map(create_element("title", _))

  private def _head_author(p: Head): Option[Element] =
    _get_inline(p.author).map(create_element("author", _))

  private def _head_date(p: Head): Option[Element] =
    _get_inline(p.date).map(create_element("date", _))

  private def _head_css(p: Head): Option[Element] =
    p.css.orElse {
      if (p.csslink.isEmpty && rule.isDefaultCssIfRequired)
        Some(Dox2DomHtmlTransformer.defaultCss)
      else
        None
    }.map { css =>
      val x = s"""${_newline}${css}${_newline}"""
      create_element("style", Map("type" -> "text/css"), create_comment(x))
    }

  private def _head_styles(p: Head): Option[Element] =
    p.csslink.map(style =>
      create_element("link", Map("rel" -> "stylesheet", "type" -> "text/css", "href" -> style))
    )

  def bodyOut(p: Body, title: Option[Node]): Out = {
    val t = title.map { x =>
      _count_up
      val level = _calc_h_level
      val tag = s"h${level}"
      create_element(tag, x)
    }
    val attrs = p.attributeMap
    val children = _children(p)
    val xs = t.toVector ++ children
    val article = create_element("article", xs)
    t.foreach { _ =>
      _count_down
    }
    create_element("body", attrs, List(article))
  }

  def sectionOut(p: Section): Out = _section(p)

  def divOut(p: Div): Out = _block(p)

  def paragraphOut(p: Paragraph): Out = _block(p)

  def textOut(p: Text): Out = _inline(p)

  def boldOut(p: Bold): Out = _inline(p)

  def italicOut(p: Italic): Out = _inline(p)

  def underlineOut(p: Underline): Out = _inline(p)

  def codeOut(p: Code): Out = _inline(p)

  def preOut(p: Pre): Out = _inline(p)

  def ulOut(p: Ul): Out = RAISE.unsupportedOperationFault

  def olOut(p: Ol): Out = RAISE.unsupportedOperationFault

  def liOut(p: Li): Out = RAISE.unsupportedOperationFault

  def delOut(p: Del): Out = _inline(p)

  def hyperlinkOut(p: Hyperlink): Out = _inline(p)

  def referenceImgOut(p: ReferenceImg): Out = _inline(p)

  def tableOut(p: Table): Out = RAISE.unsupportedOperationFault

  def spaceOut(p: Space): Out = RAISE.unsupportedOperationFault

  def dlOut(p: Dl): Out = RAISE.unsupportedOperationFault

  def dtOut(p: Dt): Out = RAISE.unsupportedOperationFault

  def ddOut(p: Dd): Out = RAISE.unsupportedOperationFault

  def fragmentOut(p: Fragment): Out = RAISE.unsupportedOperationFault

  def figureOut(p: Figure): Out = RAISE.unsupportedOperationFault

  def dotImgOut(p: DotImg): Out = RAISE.unsupportedOperationFault

  def ditaaImgOut(p: DitaaImg): Out = RAISE.unsupportedOperationFault

  protected final def create_element(name: String): Element =
    _factory.element(name)

  protected final def create_element(name: String, attrs: Map[String, String]): Element =
    _factory.element(name, attrs)

  protected final def create_element(name: String, attrs: Map[String, String], child: Node): Element =
    _factory.element(name, attrs, child)

  protected final def create_element(name: String, children: Seq[Node]): Element =
    _factory.element(name, Map.empty, children)

  protected final def create_element(name: String, attrs: Map[String, String], children: Seq[Node]): Element =
    _factory.element(name, attrs, children)

  protected final def create_element(name: String, node: Node): Element =
    _factory.element(name, node)

  protected final def create_comment(p: String): Comment = _factory.comment(p)

  private def _children(p: Dox): Seq[Node] = {
    p.elements.map(_dox)
  }

  private def _dox(p: Dox): Node = {
    import Dox2DomHtmlTransformer.Rule._
    rule.convert(_rule_context, p).map {
      case CompleteConversion(r) => r
      case ElementConversion(e) => _element(p, e)
      case AttributeOverwriteConversion(attrs) => _attributes_overwrite(p, attrs)
      case AttributeAppendConversion(attrs) => _attributes_append(p, attrs)
    }.getOrElse(_dox_node(p))
  }

  private def _element(p: Dox, elem: Element) = {
    for (x <- p.elements) {
      val n = _dox(x)
      elem.appendChild(n)
    }
    elem
  }

  private def _attributes_overwrite(p: Dox, attrs: XmlAttributes) = {
    val a = _dox_node(p)
    _factory.overwriteAttributes(a, attrs)
  }

  private def _attributes_append(p: Dox, attrs: XmlAttributes) = {
    val a = _dox_node(p)
    _factory.appendAttributes(a, attrs)
  }

  private def _dox_node(p: Dox): Node = p match {
    case EmptyDox => _factory.empty()
    case m: Fragment => _fragment(m)
    case m: Head => _head(m)
    case m: Body => _node(m)
    case m: Section => _section(m)
    case m: Table => _table(m)
    case m: Li => _node(m)
    case m: Inline => _inline(m)
    case m: Block => _block(m)
  }

  private def _get_inline(ps: InlineContents): Option[Node] = ps match {
    case Nil => None
    case xs => Some(_inline(ps))
  }

  private def _inline(ps: InlineContents): Node = ps match {
    case Nil => _factory.empty()
    case x :: Nil => _inline(x)
    case xs => _factory.fragment(xs.map(_inline))
  }

  private def _inline(p: Inline): Node = p match {
    case m: Text => _factory.text(m.contents)
    case m => _node(p)
  }

  private def _block(p: Block): Node = _node(p)

  private def _fragment(p: Fragment): Node = {
    val xs = p.contents.map(_dox)
    _factory.fragment(xs)
  }

  private def _head(p: Head): Node = {
    val attrs = p.attributeMap
    val children = p.elements.map(_dox)
    _factory.element("head", attrs, children)
  }

  private def _node(p: Dox): Node =
    p.getHtmlTag.map(tag =>
      _factory.element(tag, p.attributeMap, p.elements.map(_dox))
    ).getOrElse(RAISE.noReachDefect(s"$p"))

  private def _section(p: Section): Node = {
    _count_up
    val h = _make_h(p)
    val tag = p.takeHtmlTag
    val attrs = p.attributeMap
    val cs = p.elements.map(_dox)
    val xs = h +: cs
    _count_down
    _factory.element(tag, attrs, xs)
  }

  private def _table(p: Table): Node = {
    val caption = _caption(p)
    val thead = _thead(p)
    val tfoot = _tfoot(p)
    val tbody = _tbody(p)
    val elements: List[Node] = List(caption, thead, tfoot).flatten :+ tbody
    _factory.element("TABLE", elements)
  }

  private def _caption(p: Table): Option[Node] = {
    val a = p.caption.map(_.contents) orElse p.label.map(x => List(Dox.text(x)))
    a map { x =>
      _factory.element("CAPTION", _dox(x))
    }
  }

  private def _thead(p: Table): Option[Node] = p.head map { h =>
    val trs = h.records.map(_trh(p.width, _))
    _factory.element("THEAD", trs)
  }

  private def _tfoot(p: Table): Option[Node] = p.foot map { f =>
    val trs = f.records.map(_trh(p.width, _))
    _factory.element("TFOOT", trs)
  }

  private def _tbody(p: Table): Node = {
    val trs = p.body.records.map(_trd(p.width, _))
    _factory.element("TBODY", trs)
  }

  private def _trh(width: Int, p: TRecord): Node = _tr(width, "TH", p)

  private def _trd(width: Int, p: TRecord): Node = _tr(width, "TD", p)

  private def _tr(width: Int, tag: String, p: TRecord): Node = {
    val stubs = List.fill(width - p.length)(_factory.element(tag))
    val thds = p.fields.map(_thd) ++ stubs
    _factory.element("TR", thds)
  }

  private def _thd(p: TField): Node = p match {
    case m: TD => _factory.element("TD", p.contents.map(_dox))
    case m: TH => _factory.element("TH", p.contents.map(_dox))
  }

  private def _count_up {
    _section_depth = _section_depth + 1
  }

  private def _count_down {
    _section_depth = _section_depth - 1
  }

  private def _make_h(p: Section): Node = {
    val level = _calc_h_level
    val tag = s"h${level}"
    _factory.element(tag, _inline(p.title))
  }

  private def _calc_h_level: Int = _section_depth + _base_section_header - 1
}

object Dox2DomHtmlTransformer {
  val defaultCss = """
h1 { border-bottom: 5px solid black; width: 80%; font-size: 2em }
h2 { border-bottom: 4.5px solid black; width: 60%; font-size: 1.5em }
h3 { border-bottom: 4px solid black; width: 40%; font-size: 1.4em }
h4 { border-bottom: 3.5px solid gray; width: 30%; font-size: 1.3em }
h5 { border-bottom: 3px solid darkgray; width: 20%; font-size: 1.2em }
h6 { border-bottom: 2.5px solid lightgray; width: 10%; font-size: 1.1em }
table { background: black; float: center }
thead { border: none }
tbody { border: none }
th { color: white; background: darkgray }
td { background: white; border: 1px }
pre.program { background: lightgray; border: 1px }
pre.console { color: white; background: black; border: 1px }
pre { background: lightgray; border: 1px }
"""

  case class Rule(
    sectionBaseNumber: Option[Int] = None,
    tacticses: Vector[Rule.Tactics] = Vector.empty,
    isDefaultCssIfRequired: Boolean = true
  ) {
    def convert(ctx: Rule.Context, p: Dox): Option[Rule.ConversionResult] = tacticses.toStream.flatMap(_.convert(ctx, p)).headOption
  }
  object Rule {
    val empty = Rule()

    class Context(transformer: Dox2DomHtmlTransformer) {
      private def _factory = transformer._factory

      def element(tag: HtmlTag, attrs: XmlAttributes): Element =
        _factory.element(tag.name, attrs)

      def text(p: String): DomText = _factory.text(p)
    }

    sealed trait NodeKind extends NamedValueInstance {
      def isAccept(p: Dox): Boolean
    }
    object NodeKind extends EnumerationClass[NodeKind] {
      val elements = Vector()
    }
    case object DProgram extends NodeKind {
      val name = "program"

      def isAccept(p: Dox): Boolean = p.isInstanceOf[Program]
    }
    case object DConsole extends NodeKind {
      val name = "console"

      def isAccept(p: Dox): Boolean = p.isInstanceOf[Console]
    }

    sealed trait HtmlTag extends NamedValueInstance {
    }
    object HtmlTag extends EnumerationClass[NodeKind] {
      val elements = Vector()
    }
    case object HPre extends HtmlTag {
      val name = "pre"
    }

    sealed trait ConversionResult
    case class CompleteConversion(result: Node) extends ConversionResult
    case class ElementConversion(element: Element) extends ConversionResult
    case class AttributeOverwriteConversion(attributes: XmlAttributes) extends ConversionResult
    case class AttributeAppendConversion(attributes: XmlAttributes) extends ConversionResult

    trait Tactics {
      def convert(ctx: Context, p: Dox): Option[ConversionResult]
    }
    object Tactics {
      def apply(g: Guard, a: Action): Tactics = GuardActionTactics(g, a)

      def apply(pf: PartialFunction[Dox, Action]): Tactics = PartialFunctionTactics(pf)
    }

    case class GuardActionTactics(guard: Guard, action: Action) extends Tactics {
      def convert(ctx: Context, p: Dox): Option[ConversionResult] =
        if (guard.isAccept(p))
          action.convert(ctx, p)
        else
          None
    }

    case class PartialFunctionTactics(pf: PartialFunction[Dox, Action]) extends Tactics {
      def convert(ctx: Context, p: Dox): Option[ConversionResult] =
        if (pf.isDefinedAt(p))
          pf(p).convert(ctx, p)
        else
          None
    }

    trait Guard {
      def isAccept(p: Dox): Boolean
    }

    case class NodeKindGuard(kinds: Vector[NodeKind]) extends Guard {
      def isAccept(p: Dox) = kinds.exists(_.isAccept(p))
    }
    object NodeKindGuard {
      def apply(p: NodeKind, ps: NodeKind*): NodeKindGuard = NodeKindGuard(p +: ps.toVector)
    }

    trait Action {
      def convert(ctx: Context, p: Dox): Option[ConversionResult]
    }

    case class ElementAction(
      tag: HtmlTag,
      attributes: XmlAttributes
    ) extends Action {
      def convert(ctx: Context, p: Dox): Option[ConversionResult] = {
        val r = ctx.element(tag, attributes)
        Some(ElementConversion(r))
      }
    }
    object ElementAction {
      def apply(tag: HtmlTag, p: Tuple2[String, String], ps: Tuple2[String, String]*): ElementAction =
        ElementAction(tag, XmlAttributes.create(p +: ps.toVector))
    }

    case class TextAction(text: String) extends Action {
      def convert(ctx: Context, p: Dox): Option[ConversionResult] = {
        val r = ctx.text(text)
        Some(CompleteConversion(r))
      }
    }

    def apply(p: Tactics, ps: Tactics*): Rule = Rule(tacticses = p +: ps.toVector)
  }
}
