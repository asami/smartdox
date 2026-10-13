package org.smartdox.parser

import java.net.URI
import scala.collection.JavaConverters._
import org.commonmark.node.{Node, Heading, BulletList, OrderedList, ListBlock, ListItem,
  BlockQuote, ThematicBreak, FencedCodeBlock, IndentedCodeBlock, HtmlBlock, HtmlInline,
  Text => MarkdownText, Paragraph => MarkdownParagraph, Emphasis, StrongEmphasis,
  Code => MarkdownCode, Link, Image, LinkReferenceDefinition, SoftLineBreak, HardLineBreak}
import org.commonmark.parser.{Parser, IncludeSourceSpans}
import org.commonmark.renderer.html.HtmlRenderer
import org.commonmark.internal.util.Escaping
import org.commonmark.ext.gfm.tables.{TablesExtension, TableBlock, TableHead,
  TableBody, TableRow, TableCell}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Node => HtmlNode, Element, TextNode, DataNode}
import org.goldenport.context.Consequence
import org.goldenport.collection.VectorMap
import org.goldenport.i18n.I18NString
import org.goldenport.parser.ParseLocation
import org.smartdox._

/*
 * @since   Oct.  1, 2026
 * @version Oct. 13, 2026
 * @author  ASAMI, Tomoharu
 */
private[smartdox] object MarkdownDocumentParser {
  def parseC(config: Dox2Parser.Config, source: String): Consequence[Document] =
    Consequence.execute { parse(config, source) }

  def parse(config: Dox2Parser.Config, source: String): Document = {
    val (body, metadata) = Dox2ParserFrontMatter.split(config, source)
    val extensions = java.util.Collections.singletonList(TablesExtension.create())
    val parser = Parser.builder().extensions(extensions).build()
    val document = MarkdownDocumentAst.convert(config, parser.parse(body))
    metadata.fold(document)(Dox2ParserFrontMatter.merge(document, _))
  }

  private val _extensions = java.util.Collections.singletonList(TablesExtension.create())
  private val _parser = Parser.builder().extensions(_extensions).
    includeSourceSpans(IncludeSourceSpans.BLOCKS_AND_INLINES).build()
  private val _html_renderer = HtmlRenderer.builder().extensions(_extensions).softbreak(" ").build()

  def parse(context: Dox2Parser.ParseContext, body: String): Document = {
    val root = _parser.parse(body)
    new Projection(context, body).document(root)
  }

  private class Projection(context: Dox2Parser.ParseContext, body: String) {
    private val _config = context.config
    private val _source_lines = body.split("\r\n|\n|\r", -1)

    def document(root: Node): Document = Document(Head(), Body(_blocks(_children(root))))

    private def _children(node: Node): List[Node] = {
      val result = List.newBuilder[Node]
      var child = node.getFirstChild
      while (child != null) {
        result += child
        child = child.getNext
      }
      result.result()
    }

    private def _location(node: Node): Option[ParseLocation] =
      if (!_config.isLocation) None
      else node.getSourceSpans.asScala.headOption.map { span =>
        ParseLocation(_config.file, Some(span.getLineIndex + 1), Some(span.getColumnIndex + 1))
      }.orElse(_config.linesConfig.inlineConfig.location)

    private def _source(node: Node): String =
      node.getSourceSpans.asScala.map { span =>
        _source_lines(span.getLineIndex).slice(span.getColumnIndex, span.getColumnIndex + span.getLength)
      }.mkString("\n")

    private def _blocks(nodes: List[Node]): List[Dox] = nodes match {
      case (heading: Heading) :: tail =>
        val (inside, rest) = tail.span {
          case next: Heading => next.getLevel > heading.getLevel
          case _ => true
        }
        Section(_inlines(_children(heading), heading), _blocks(inside), heading.getLevel,
          location = _location(heading)) :: _blocks(rest)
      case node :: tail => _block(node) ::: _blocks(tail)
      case Nil => Nil
    }

    private def _block(node: Node): List[Dox] = {
      val location = _location(node)
      node match {
        case _: LinkReferenceDefinition => Nil
        case paragraph: MarkdownParagraph =>
          List(Paragraph(_inlines(_children(paragraph), paragraph), location = location))
        case list: BulletList =>
          List(Ul(_list_items(list), location = location))
        case list: OrderedList =>
          val start = list.getMarkerStartNumber
          val attributes = if (start == 1) VectorMap.empty[String, String]
            else VectorMap("start" -> start.toString)
          List(Ol(_list_items(list), attributes, location))
        case quote: BlockQuote =>
          List(Quotation.BlockQuote(_blocks(_children(quote)), None, None, location))
        case _: ThematicBreak => List(HorizontalRule(location))
        case code: FencedCodeBlock =>
          val kind = Option(code.getInfo).toList.flatMap(_.trim.split("\\s+").headOption).filter(_.nonEmpty)
          val attributes = kind.headOption.fold(VectorMap.empty[String, String])(x => VectorMap("kind" -> x))
          List(Program(code.getLiteral, attributes, location, Dox.Callouts.empty))
        case code: IndentedCodeBlock =>
          List(Program(code.getLiteral, location = location, callouts = Dox.Callouts.empty))
        case table: TableBlock => List(_table(table))
        case html: HtmlBlock =>
          Jsoup.parseBodyFragment(html.getLiteral).body().childNodes().asScala.toList.
            flatMap(_html_block(_, location, html.getLiteral))
        case other => throw new IllegalArgumentException(s"Unsupported CommonMark block: ${other.getClass.getName}")
      }
    }

    private def _list_items(list: ListBlock): List[Li] =
      _children(list).map { node =>
        val children = _children(node)
        val blocks = _blocks(children)
        val contents = if (list.isTight) blocks.flatMap {
          case paragraph: Paragraph => paragraph.contents
          case child => List(child)
        } else blocks
        Li(contents.map {
          case content: ListContent => content
          case other => throw new IllegalArgumentException(s"Unsupported list content: $other")
        }, location = _location(node))
      }

    private def _table(table: TableBlock): Table = {
      val compartments = _children(table)
      val headnodes = compartments.collect { case head: TableHead => head }
      val bodynodes = compartments.collect { case rows: TableBody => rows }
      val headercells = headnodes.flatMap(_children).flatMap(_children).collect { case cell: TableCell => cell }
      val cols = headercells.map { cell =>
        val align = Option(cell.getAlignment).map {
          case TableCell.Alignment.LEFT => Table.Align.Left
          case TableCell.Alignment.CENTER => Table.Align.Center
          case TableCell.Alignment.RIGHT => Table.Align.Right
        }
        Col(align = align, location = _location(cell))
      }
      val colgroup = if (cols.exists(_.align.nonEmpty)) Some(Colgroup(cols, _location(table))) else None
      val head = headnodes.headOption.map(x => THead(_children(x).map(_table_row), location = _location(x)))
      val rows = bodynodes.flatMap(_children).map(_table_row)
      Table(head, TBody(rows, location = _location(table)), None, None, colgroup, None, None,
        location = _location(table))
    }

    private def _table_row(node: Node): TR =
      TR(_children(node).collect { case cell: TableCell =>
        val contents = _inlines(_children(cell), cell)
        if (cell.isHeader) TH(contents, location = _location(cell))
        else TD(contents, location = _location(cell))
      }, location = _location(node))

    private def _inlines(nodes: List[Node], container: Node): List[Inline] =
      if (nodes.exists(_.isInstanceOf[HtmlInline])) {
        val fragment = nodes.map(_html_renderer.render).mkString
        Jsoup.parseBodyFragment(fragment).body().childNodes().asScala.toList.
          flatMap(_html_inline(_, _location(container), _source(container)))
      } else nodes.map(_inline)

    private def _inline(node: Node): Inline = {
      val location = _location(node)
      node match {
        case text: MarkdownText => Text(text.getLiteral, location)
        case em: Emphasis => Em(_inlines(_children(em), em), location = location)
        case strong: StrongEmphasis => Strong(_inlines(_children(strong), strong), location = location)
        case code: MarkdownCode => Code(List(Text(code.getLiteral, location)), location = location)
        case link: Link => Hyperlink(_inlines(_children(link), link), new URI(Escaping.percentEncodeUrl(link.getDestination)),
          Option(link.getTitle).map(I18NString(_)), location = location)
        case image: Image =>
          _image(_plain(_children(image)), Escaping.percentEncodeUrl(image.getDestination), Option(image.getTitle), location, _source(image))
        case _: SoftLineBreak => Text(" ", location)
        case _: HardLineBreak => Html5Inline("br", VectorMap.empty, Nil, location)
        case other => throw new IllegalArgumentException(s"Unsupported CommonMark inline: ${other.getClass.getName}")
      }
    }

    private def _plain(nodes: List[Node]): String = nodes.map {
      case text: MarkdownText => text.getLiteral
      case code: MarkdownCode => code.getLiteral
      case _: SoftLineBreak | _: HardLineBreak => " "
      case other => _plain(_children(other))
    }.mkString

    private def _image(alt: String, destination: String, title: Option[String],
      location: Option[ParseLocation], source: String): ReferenceImg = {
      val config = _config.linesConfig.inlineConfig.withLocation(location)
      val image = MarkdownImageInlineParser.fromMarkdownAst(config, alt, destination, source)
      image.copy(attributes = title.fold(image.attributes)(x => image.attributes + ("title" -> x)))
    }

    private def _attributes(element: Element): VectorMap[String, String] =
      VectorMap(element.attributes().asList().asScala.map(x => x.getKey -> x.getValue).toVector)

    private def _html_block(node: HtmlNode, location: Option[ParseLocation], source: String): List[Dox] = node match {
      case text: TextNode => List(Text(text.getWholeText, location))
      case data: DataNode => List(Text(data.getWholeData, location))
      case element: Element if element.normalName() == "a" || element.normalName() == "img" =>
        _html_inline(element, location, source)
      case element: Element =>
        List(Html5(element.normalName(), _attributes(element),
          element.childNodes().asScala.toList.flatMap(_html_block(_, location, source)), location))
      case _ => Nil // HTML comments have no publication content.
    }

    private def _html_inline(node: HtmlNode, location: Option[ParseLocation], source: String): List[Inline] = node match {
      case text: TextNode => List(Text(text.getWholeText, location))
      case data: DataNode => List(Text(data.getWholeData, location))
      case element: Element =>
        val attributes = _attributes(element)
        val contents = element.childNodes().asScala.toList.flatMap(_html_inline(_, location, source))
        val result: Inline = element.normalName() match {
          case "strong" => Strong(contents, attributes, location)
          case "em" => Em(contents, attributes, location)
          case "code" => Code(contents, attributes, location = location)
          case "a" => Hyperlink(contents, new URI(Escaping.percentEncodeUrl(element.attr("href"))),
            if (element.hasAttr("title")) Some(I18NString(element.attr("title"))) else None,
            attributes, location = location)
          case "img" =>
            val image = _image(element.attr("alt"), Escaping.percentEncodeUrl(element.attr("src")),
              if (element.hasAttr("title")) Some(element.attr("title")) else None, location, source)
            image.copy(attributes = attributes - "src" - "alt")
          case name => Html5Inline(name, attributes, contents, location)
        }
        List(result)
      case _ => Nil
    }
  }
}
