package org.smartdox.parser

import scalaz._, Scalaz._, Validation._, Tree._
import org.goldenport.RAISE
import org.goldenport.parser._
import org.goldenport.i18n.I18NElement
import org.smartdox._
import Dox._

/*
 * @since   Jul.  1, 2017
 *  version Aug. 28, 2017
 *  version Oct. 14, 2018
 *  version Nov. 17, 2018
 * @version Dec. 24, 2018
 * @author  ASAMI, Tomoharu
 */
class Dox2Parser(config: Dox2Parser.Config) {
  import Dox2Parser._

  def apply(s: String): ParseResult[Dox] = {
    val ctx = ParseContext(config, 0)
    val blocks = LogicalBlocks.parse(s)
    val head = Head()
    val xs = _blocks(ctx, blocks)
    println(s"Dox2Parser#apply ${blocks} => $xs")
    ParseSuccess(Document(head, Body(xs.toList)))
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
}

object Dox2Parser {
  type Transition = (ParseMessageSequence, ParseResult[Dox], DoxSectionParseState)

  case class Config(
    isDebug: Boolean = false,
    linesConfig: DoxLinesParser.Config = DoxLinesParser.Config.default
  ) extends ParseConfig {
  }
  object Config {
    val default = Config()
    val debug = Config(true)
    val orgmodeInline = default.copy(
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
