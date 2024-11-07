package org.smartdox.parser

import scalaz._, Scalaz._, Validation._, Tree._
import org.goldenport.RAISE
import org.goldenport.parser._
import org.goldenport.collection.VectorMap
import org.goldenport.i18n.I18NElement
import org.smartdox._
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
 * @version Oct. 24, 2024
 * @author  ASAMI, Tomoharu
 */
class Dox2Parser(config: Dox2Parser.Config) {
  import Dox2Parser._

  def apply(s: String): ParseResult[Document] = {
    val blocks = LogicalBlocks.parse(config.blocksConfig, s)
    apply(blocks)
  }

  def apply(in: LogicalBlock): ParseResult[Document] = {
    val blocks = LogicalBlocks(in)
    apply(blocks)
  }

  def apply(blocks: LogicalBlocks): ParseResult[Document] = {
    val ctx = ParseContext(config, 0)
    val xs = _blocks(ctx, blocks)
    // println(s"Dox2Parser#apply ${blocks} => $xs")
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
    xs./:(Z())(_+_).r
  }

  private def _blocks(ctx: ParseContext, p: LogicalBlocks): Vector[Dox] =
    p.blocks.flatMap(_block(ctx, _))

  private def _block(
    ctx: ParseContext,
    p: LogicalBlock
  ): Vector[Dox] = p match {
    case StartBlock => Vector.empty
    case EndBlock => Vector.empty
    case m: LogicalSection => Vector(_section(ctx.levelUp, m))
    case m: LogicalParagraph => Vector(_paragraph(ctx, m))
    case m: LogicalVerbatim => Vector(_vervatim(ctx, m))
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
    linesConfig: DoxLinesParser.Config
  ) extends ParseConfig {
  }
  object Config {
    import DoxLinesParser.{Config => _, _}

    val verbatims = Vector(
      BeginSrcAnnotationClass,
      BeginExampleAnnotationClass,
      GenericBeginAnnotationClass
    )
    val blocksConfig = LogicalBlocks.Config.easyhtml.addVerbatims(verbatims)

    val default = Config(
      false,
      true,
      Config.blocksConfig,
      DoxLinesParser.Config.default
    )
    val debug = default.copy(true)
    val orgmodeInline = default.copy(
      linesConfig = DoxLinesParser.Config.orgmodeFull
    )
    val markdown = default.copy(
      linesConfig = DoxLinesParser.Config.orgmodeFull
    )
  }

  case class ParseContext(config: Config, level: Int) {
    def levelUp = copy(level = level + 1)
  }

  def parse(in: String): Dox = parse(Config.default, in)

  def parse(config: Config, in: String): Dox = {
    val parser = new Dox2Parser(config)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parse(config: Config, in: LogicalBlock): Dox = {
    val parser = new Dox2Parser(config)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parse(config: Config, in: LogicalBlocks): Dox = {
    val parser = new Dox2Parser(config)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseSection(config: Config, in: LogicalSection): Section = {
    val parser = new Dox2Parser(config)
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
