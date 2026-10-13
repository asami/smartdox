package org.smartdox.parser

import java.net.URI
import scala.collection.JavaConverters._
import scala.collection.mutable.{ArrayBuffer, Map => MutableMap}
import org.commonmark.ext.gfm.tables.TablesExtension
import org.commonmark.node.{HtmlBlock, HtmlInline, Image => MarkdownImage, Node => MarkdownNode}
import org.commonmark.renderer.html.{CoreHtmlNodeRenderer, HtmlNodeRendererContext, HtmlNodeRendererFactory, HtmlRenderer}
import org.goldenport.collection.VectorMap
import org.jsoup.nodes.{Comment, DataNode, Element, Node, TextNode}
import org.jsoup.parser.{Parser => HtmlParser, Tag}
import org.goldenport.i18n.I18NString
import org.smartdox._

/*
 * @since   Oct.  6, 2026
 * @version Oct.  6, 2026
 */
private[smartdox] object MarkdownDocumentAst {
  def convert(config: Dox2Parser.Config, parsed: MarkdownNode): Document = {
    val buffer = new java.lang.StringBuilder
    val images = MutableMap.empty[Int, RenderedImage]
    val rawspans = ArrayBuffer.empty[(Int, Int)]
    val factory = new HtmlNodeRendererFactory {
      override def create(context: HtmlNodeRendererContext): CoreHtmlNodeRenderer =
        new CoreHtmlNodeRenderer(context) {
          override def getNodeTypes(): java.util.Set[Class[_ <: MarkdownNode]] = {
            val types = new java.util.HashSet[Class[_ <: MarkdownNode]]
            types.add(classOf[MarkdownImage])
            types.add(classOf[HtmlBlock])
            types.add(classOf[HtmlInline])
            types
          }

          override def visit(image: MarkdownImage): Unit = {
            val start = buffer.length
            super.visit(image)
            images.put(start, RenderedImage(buffer.length, image))
          }

          override def visit(htmlBlock: HtmlBlock): Unit = {
            val start = buffer.length
            super.visit(htmlBlock)
            rawspans += ((start, buffer.length))
          }

          override def visit(htmlInline: HtmlInline): Unit = {
            val start = buffer.length
            super.visit(htmlInline)
            rawspans += ((start, buffer.length))
          }
        }
    }
    val renderer = HtmlRenderer.builder().extensions(List(TablesExtension.create()).asJava).
      nodeRendererFactory(factory).build()
    renderer.render(parsed, buffer)
    val parser = HtmlParser.htmlParser().setTrackPosition(true)
    val context = new Element(Tag.valueOf("body"), "")
    val fragment = parser.parseFragmentInput(buffer.toString, context, "").asScala.toList
    val inlineconfig = config.linesConfig._with_source_identity(config.file).inlineConfig
    val conversion = new Conversion(inlineconfig, images.toMap, rawspans.toList)
    val contents = conversion.convertRoot(fragment)
    val location = inlineconfig.location
    Document(Head(location = location), Body(contents, location = location), location = location)
  }

  private case class RenderedImage(end: Int, image: MarkdownImage)

  private final class Conversion(
    config: DoxInlineParser.Config,
    images: Map[Int, RenderedImage],
    rawspans: List[(Int, Int)]
  ) {
    private val _location = config.location
    private val _images = images
    private val _raw_spans = rawspans

    def convertRoot(nodes: List[Node]): List[Dox] = {
      val roots = ArrayBuffer.empty[Dox]
      var sections = List.empty[SectionContents]
      def _append_(dox: Dox): Unit = sections.headOption match {
        case Some(section) => section.contents += dox
        case None => roots += dox
      }
      def _close_(): Unit = {
        val section = sections.head
        sections = sections.tail
        _append_(Section(section.title, section.contents.toList, section.level, section.attributes, _location))
      }
      nodes.foreach {
        case element: Element if _heading_level(element).nonEmpty =>
          val level = _heading_level(element).get
          while (sections.headOption.exists(_.level >= level))
            _close_()
          sections = new SectionContents(_inlines(element), level, _attributes(element)) :: sections
        case node => _convert(node, separator = true).foreach(_append_)
      }
      while (sections.nonEmpty)
        _close_()
      roots.toList
    }

    private def _heading_level(element: Element): Option[Int] = element.normalName() match {
      case "h1" => Some(1)
      case "h2" => Some(2)
      case "h3" => Some(3)
      case "h4" => Some(4)
      case "h5" => Some(5)
      case "h6" => Some(6)
      case _ => None
    }

    private def _attributes(element: Element): VectorMap[String, String] =
      VectorMap(element.attributes().asList().asScala.map(x => x.getKey -> x.getValue).toVector)

    private def _children(element: Element, separator: Boolean = false): List[Dox] =
      element.childNodes().asScala.toList.flatMap(x => _convert(x, separator))

    private def _inlines(element: Element): List[Inline] =
      _children(element).map {
        case inline: Inline => inline
        case block: Html5 => Html5Inline(block.name, block.attributes, block.contents, block.location)
      }

    private def _convert(node: Node, separator: Boolean, raw: Boolean = false): Option[Dox] = node match {
      case _: Comment => None
      case text: TextNode =>
        val contents = text.getWholeText
        if (separator && contents.forall(Character.isWhitespace)) None
        else Some(Text(contents, _location))
      case data: DataNode => Some(Text(data.getWholeData, _location))
      case element: Element => Some(if (raw) _raw_element(element) else _element(element))
      case _ => None
    }

    private def _element(element: Element): Dox = {
      val attributes = _attributes(element)
      element.normalName() match {
        case _ if _raw_origin(element) => _raw_element(element)
        case "p" => Paragraph(_children(element), attributes, location = _location)
        case "strong" => Strong(_inlines(element), attributes, _location)
        case "em" => Em(_inlines(element), attributes, _location)
        case "code" => Code(_inlines(element), attributes, location = _location)
        case "ul" => Ul(_children(element, true).collect { case x: Li => x }, attributes, _location)
        case "ol" => Ol(_children(element, true).collect { case x: Li => x }, attributes, _location)
        case "li" => Li(_children(element).map { case x: ListContent => x }, attributes, _location)
        case "blockquote" => Quotation.SimpleQuote(_children(element, true), _location)
        case "table" =>
          val children = _children(element, true)
          Table(
            children.collectFirst { case x: THead => x },
            children.collectFirst { case x: TBody => x }.getOrElse(TBody(Nil, location = _location)),
            None, None, None, None, attributes.get("id"), attributes, _location
          )
        case "thead" => THead(_children(element, true).collect { case x: TRecord => x }, attributes, _location)
        case "tbody" => TBody(_children(element, true).collect { case x: TRecord => x }, attributes, _location)
        case "tr" => TR(_children(element, true).collect { case x: TField => x }, attributes, _location)
        case "th" => TH(_inlines(element), attributes, _location)
        case "td" => TD(_children(element), attributes, _location)
        case "a" => Hyperlink(
          _inlines(element), new URI(element.attr("href")),
          attributes.get("title").map(I18NString.apply), attributes, location = _location
        )
        case "img" => _image(element, attributes)
        case "pre" => Html5("pre", attributes, _children(element), _location)
        case "br" => Html5Inline("br", attributes, Nil, _location)
        case "hr" => HorizontalRule(_location)
        case name if element.tag().isBlock => Html5(name, attributes, _children(element), _location)
        case name => Html5Inline(name, attributes, _children(element), _location)
      }
    }

    private def _raw_origin(element: Element): Boolean = {
      val start = element.sourceRange().start().pos()
      _raw_spans.exists { case (begin, end) => begin <= start && start < end }
    }

    private def _raw_element(element: Element): Dox = {
      val attributes = _attributes(element)
      if (element.normalName() == "img")
        _image(element, attributes)
      else {
        val children = element.childNodes().asScala.toList.flatMap(x => _convert(x, separator = false, raw = true))
        if (element.tag().isBlock)
          Html5(element.normalName(), attributes, children, _location)
        else
          Html5Inline(element.normalName(), attributes, children, _location)
      }
    }

    private def _image(element: Element, attributes: VectorMap[String, String]): Inline =
      _images.get(element.sourceRange().start().pos()) match {
        case Some(rendered) =>
          val destination = rendered.image.getDestination
          val alt = element.attr("alt")
          val source = s"![$alt]($destination)"
          val admitted = MarkdownImageAdmission.admit(config, alt, source, destination)
          val retained = VectorMap(attributes.toVector.filterNot(x => x._1 == "src" || x._1 == "alt"))
          admitted.copy(attributes = retained)
        case None => Html5Inline("img", attributes, Nil, _location)
      }
  }

  private final class SectionContents(
    val title: List[Inline],
    val level: Int,
    val attributes: VectorMap[String, String]
  ) {
    val contents: ArrayBuffer[Dox] = ArrayBuffer.empty
  }
}
