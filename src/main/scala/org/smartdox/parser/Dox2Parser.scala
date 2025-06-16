package org.smartdox.parser

import scalaz._, Scalaz._, Validation._, Tree._
import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.context._
import org.goldenport.parser._
import org.goldenport.config.ConfigLoader
import org.goldenport.collection.VectorMap
import org.goldenport.i18n.I18NElement
import org.goldenport.io.InputSource
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData
import Dox._

/*
 * @since   Jul.  1, 2017
 *  version Aug. 28, 2017
 *  version Oct. 14, 2018
 *  version Nov. 17, 2018
 *  version Dec. 24, 2018
 *  version Jan. 26, 2019
 *  version Apr. 18, 2019
 *  version Oct.  2, 2019
 *  version Nov. 16, 2019
 *  version Jan. 11, 2021
 *  version Sep. 18, 2021
 *  version Oct. 14, 2023
 *  version Oct. 24, 2024
 *  version Nov. 23, 2024
 *  version Jan.  1, 2025
 *  version Feb.  7, 2025
 *  version Mar.  2, 2025
 *  version Apr.  6, 2025
 *  version May. 24, 2025
 * @version Jun. 16, 2025
 * @author  ASAMI, Tomoharu
 */
class Dox2Parser(context: Dox2Parser.ParseContext) {
  import Dox2Parser._

  def config = context.config
  implicit def dateTimeContext = context.dateTimeContext

  def apply(s: String): ParseResult[Document] = {
    val blocks = LogicalBlocks.parse(config.blocksConfig, s)
    apply(blocks)
  }

  def apply(in: LogicalBlock): ParseResult[Document] = {
    val blocks = LogicalBlocks(in)
    apply(blocks)
  }

  def apply(blocks: LogicalBlocks): ParseResult[Document] = {
    val xs = _blocks(context, blocks)
    // println(s"Dox2Parser#apply ${blocks} => $xs")
    _create(xs)
  }

  private def _create(ps: Vector[Dox]) = {
    case class Z(
      head: Head = Head(),
      elements: Vector[Dox] = Vector.empty
    ) {
      def r = ParseSuccess(Document(head, Body(elements.toList)))

      def +(rhs: Dox) = rhs match {
        case m: Head => copy(head = head.merge(m))
        case m => copy(elements = elements :+ m)
      }
    }
    ps.foldLeft(Z())(_+_).r
  }

  private def _blocks(ctx: ParseContext, p: LogicalBlocks): Vector[Dox] =
    p.blocks.flatMap(_block(ctx, _))

  private def _block(
    ctx: ParseContext,
    p: LogicalBlock
  ): Vector[Dox] = p match {
    case StartBlock => Vector.empty
    case EndBlock => Vector.empty
    case m: LogicalSection => _sections(ctx.levelUp, m)
    case m: LogicalParagraph => Vector(_paragraph(ctx, m))
    case m: LogicalVerbatim => Vector(_vervatim(ctx, m))
  }

  private def _sections(ctx: ParseContext, p: LogicalSection): Vector[Dox] =
    config.style match {
      case Config.DoxStyle.SmartDox => _section_smartdox(ctx, p)
      case Config.DoxStyle.Markdown => Vector(_section(ctx, p))
      case Config.DoxStyle.OrgMode => Vector(_section(ctx, p))
    }

  private def _section_smartdox(
    ctx: ParseContext,
    p: LogicalSection
  ): Vector[Dox] =
    p.mark match {
      case Some("=") =>
        val xs = _blocks(ctx, p.blocks)
        val (xs1, meta) = _section_head(p, xs)
        val head = Head(metadata = meta)
        head +: xs1
      case _ => Vector(_section(ctx.levelUp, p))
    }

  private def _section_head(
    p: LogicalSection,
    xs: Vector[Dox]
  ): (Vector[Dox], DocumentMetaData) = {
    val title = List(_to_dox(p.title))
    val (xs1, props) = _distill_props(xs)
    val meta0 = props.fold(DocumentMetaData.empty)(DocumentMetaData.create(_))
    val meta = meta0.withTitle(title)
    (xs1, meta)
  }

  private def _distill_props(ps: Vector[Dox]): (Vector[Dox], Option[Hocon]) =
    ps match {
      case Vector() => (ps, None)
      case Vector(x, xs @ _*) =>
        _parse_properties(x).toOption match {
          case Some(s) => (xs.toVector, Some(s))
          case None => (ps, None)
        }
    }

  private def _parse_properties(p: Dox): Option[Hocon] =
    p match {
      case m: Paragraph => 
        val in = InputSource(m.toData)
        ConfigLoader.loadConfigHocon(in).toOption
      case _ => None
    }

  private def _section(
    ctx: ParseContext,
    p: LogicalSection
  ): Section = {
    val dox = _blocks(ctx, p.blocks)
    val level = ctx.level
    val title = List(_to_dox(p.title))
    Section(title, dox.toList, level)
  }

  private def _to_dox(p: I18NElement) = Text(p.toI18NString.en) // TODO

  private def _to_list(p: Dox): List[Dox] = p match {
    case m: Div => _normalize(m.contents)
    case m: Span => _normalize(m.contents)
    case m => List(m)
  }

  private def _normalize(ps: List[Dox]): List[Dox] = ps.flatMap(_to_list)

  private def _paragraph(ctx: ParseContext, p: LogicalParagraph): Dox = {
    DoxLinesParser.parse(ctx.config.linesConfig, p)
  }

  private def _vervatim(ctx: ParseContext, p: LogicalVerbatim): Dox = {
    p.mark match {
      case m: LogicalBlock.RawBackquoteMark => _program(p)
      case m: DoxLinesParser.BeginSrcAnnotation => _program(p)
      case m: DoxLinesParser.BeginExampleAnnotation => _program(p)
      case m: DoxLinesParser.GenericBeginAnnotation => _program(p)
      case m => _program(p)
    }
  }

  private def _program(p: LogicalVerbatim) = {
    val cs = p.lines.text
    val attrs = VectorMap.empty[String, String]
    Program(cs, attrs, p.location)
  }
}

object Dox2Parser {
  type Transition = (ParseMessageSequence, ParseResult[Dox], DoxSectionParseState)

  case class Config(
    isDebug: Boolean,
    isLocation: Boolean,
    blocksConfig: LogicalBlocks.Config,
    linesConfig: DoxLinesParser.Config,
    style: Config.DoxStyle
  ) extends ParseConfig {
  }
  object Config {
    import DoxLinesParser.{Config => _, _}

    sealed trait DoxStyle {
    }
    object DoxStyle {
      case object SmartDox extends DoxStyle
      case object Markdown extends DoxStyle
      case object OrgMode extends DoxStyle

      val default = SmartDox
    }

    val verbatims = Vector(
      BeginSrcAnnotationClass,
      BeginExampleAnnotationClass,
      GenericBeginAnnotationClass
    )
    val blocksConfig = LogicalBlocks.Config.easyHtml.addVerbatims(verbatims)

    val default = Config(
      false,
      true,
      Config.blocksConfig,
      DoxLinesParser.Config.default,
      DoxStyle.SmartDox
    )
    val debug = default.copy(true)
    val smartdox = default
    val orgmodeInline = default.copy(
      linesConfig = DoxLinesParser.Config.orgmode,
      style = DoxStyle.OrgMode
    )
    val orgmode = orgmodeInline
    val markdown = default.copy(
      linesConfig = DoxLinesParser.Config.markdown,
      style = DoxStyle.Markdown
    )
    val literateModel = default.copy(
      blocksConfig = LogicalBlocks.Config.literateModel,
      linesConfig = DoxLinesParser.Config.literateModel
    )
  }

  case class ParseContext(
    config: Config,
    dateTimeContext: DateTimeContext,
    level: Int = 0
  ) {
    def levelUp = copy(level = level + 1)
  }
  object ParseContext {
    def now(): ParseContext = now(Config.default)

    def now(c: Config): ParseContext = ParseContext(
      c,
      DateTimeContext.now()
    )
  }

  def parse(in: String): Dox = parse(Config.default, in)

  def parseWithFilename(filename: String, in: String): Dox = {
    def _default_ = Dox2Parser.Config.default

    val cfg = StringUtils.getSuffix(filename).fold(_default_) {
      case "dox" => Dox2Parser.Config.smartdox
      case "org" => Dox2Parser.Config.orgmode
      case "md" => Dox2Parser.Config.markdown
      case "markdown" => Dox2Parser.Config.markdown
      case _ => _default_
    }
    parse(cfg, in)
  }

  def parse(config: Config, in: String): Dox = {
    val ctx = ParseContext.now(config)
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parse(config: Config, in: LogicalBlock): Dox = {
    val ctx = ParseContext.now(config)
    parse(ctx, in)
  }

  def parse(ctx: ParseContext, in: LogicalBlock): Dox = {
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseOne(in: String): Dox = parseOne(Config.default, in)

  def parseOne(config: Config, in: String): Dox = {
    val ctx = ParseContext.now(config)
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox.body.elements match {
        case Nil => EmptyDox
        case x :: Nil => x
        case xs => SyntaxErrorFault("${xs.mkstring}").RAISE
      }
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseFragment(config: Config, in: String): Fragment = {
    val ctx = ParseContext.now(config)
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => Fragment(dox.body.elements)
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parse(config: Config, in: LogicalBlocks): Dox =
    parse(ParseContext.now(config), in)

  def parse(ctx: ParseContext, in: LogicalBlocks): Dox = {
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseSection(config: Config, in: LogicalSection): Section =
    parseSection(ParseContext.now(config), in)

  def parseSection(ctx: ParseContext, in: LogicalSection): Section = {
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox.body.elements.headOption.map {
        case m: Section => m
        case m => RAISE.notImplementedYetDefect
      }.getOrElse(RAISE.notImplementedYetDefect)
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parse2(config: Config, in: String): Dox = {
    val parser = LogicalBlockReaderWriterStateClass(config, RootState.init)
    val (messages, result, state) = parser.apply(in)
    result match {
      case ParseSuccess(_, _) => state.result
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  trait DoxSectionParseState extends LogicalBlockReaderWriterState[Config, Dox] {
    def result: Dox
  }

  case class RootState(
    head: Head,
    body: Vector[Dox]
  ) extends DoxSectionParseState {
    private val _empty: (ParseMessageSequence, ParseResult[Dox], LogicalBlockReaderWriterState[Config, Dox]) = (ParseMessageSequence.empty, ParseSuccess(Dox.empty), this)

    def result: Dox = Document(head, Body(body.toList))

    def apply(config: Config, block: LogicalBlock): (ParseMessageSequence, ParseResult[Dox], LogicalBlockReaderWriterState[Config, Dox]) = {
      block match {
        case StartBlock => _empty
        case EndBlock => _empty
        case m: LogicalSection => _section(config, m)
        case m: LogicalParagraph => _paragraph(config, m)
        case m: LogicalVerbatim => _verbatim(config, m)
      }
    }

    private def _section(config: Config, p: LogicalSection): (ParseMessageSequence, ParseResult[Dox], LogicalBlockReaderWriterState[Config, Dox]) = {
      val dox = ??? // _to_list(DoxInlineParser.parse(p))
      val level = 1 // TODO
      val title = List(_to_dox(p.title))
      val section = Section(title, dox, level)
      (ParseMessageSequence.empty, ParseSuccess(Dox.empty), copy(body = body :+ section))
    }

    private def _to_dox(p: I18NElement) = Text(p.toI18NString.en) // TODO

    private def _to_list(p: Dox): List[Dox] = p match {
      case m: Div => _normalize(m.contents)
      case m: Span => _normalize(m.contents)
      case m => List(m)
    }

    private def _normalize(ps: List[Dox]): List[Dox] = ps.flatMap(_to_list)

    private def _paragraph(config: Config, p: LogicalParagraph): (ParseMessageSequence, ParseResult[Dox], LogicalBlockReaderWriterState[Config, Dox]) = {
      val dox = DoxLinesParser.parse(config.linesConfig, p)
      (ParseMessageSequence.empty, ParseSuccess(Dox.empty), copy(body = body :+ dox))
    }

    private def _verbatim(config: Config, p: LogicalVerbatim): (ParseMessageSequence, ParseResult[Dox], LogicalBlockReaderWriterState[Config, Dox]) = {
      ???
    }
  }
  object RootState {
    val init = RootState(Head(), Vector.empty)
  }

  // case class ParseError()
  // case class ParseWarning()

  // case class ParseResult(
  //   errors: Vector[ParseError],
  //   warnings: Vector[ParseWarning],
  //   doc: Dox
  // )

  // sealed trait ParseState {

  // }
}
