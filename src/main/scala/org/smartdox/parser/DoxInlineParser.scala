package org.smartdox.parser

import java.net.URI
import org.goldenport.RAISE
import org.goldenport.parser._
import org.goldenport.io.MimeType
import org.smartdox._

/*
 * @since   Oct. 14, 2018
 *  version Nov. 18, 2018
 *  version Dec. 30, 2018
 *  version Oct.  2, 2019
 *  version Nov. 29, 2020
 *  version Dec.  5, 2020
 *  version Feb.  8, 2021
 *  version Nov. 22, 2024
 *  version Jan.  1, 2025
 * @version Jun. 10, 2025
 * @author  ASAMI, Tomoharu
 */
object DoxInlineParser {
  type Transition = (ParseMessageSequence, ParseResult[Dox], DoxInlineParseState)

  // def parse(p: LogicalParagraph): Dox = parse(p.lines)

  // def parse(p: LogicalLines): Dox = toDox(p.lines.map(parse))

  def parse(p: LogicalLine): Dox = parse(p.text)

  def parse(in: String): Dox = parse(Config.default, in)

  def parse(config: Config, in: String): Dox = {
    // println(s"Inline($config): $in")
    val (messages, result, state) = apply(config, in)
    result match {
      case ParseSuccess(dox, _) => Dox.toDox(dox)
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  // def apply(in: String): (ParseMessageSequence, ParseResult[Vector[Dox]], DoxInlineParseState) =
  //   apply(Config.default, in)

  def apply(config: Config, in: String): (ParseMessageSequence, ParseResult[Vector[Dox]], DoxInlineParseState) = {
    val parser = ParseReaderWriterStateClass[Config, Dox](config, NormalState.init(config))
    val (msgs, result, state) = parser.apply(in)
    // println(s"DoxInlineParseState#apply $result")
    assume (result.toOption.map(_.forall(_ != null)).getOrElse(true), s"should not be null: input[$in]")
    val s: DoxInlineParseState = state.asInstanceOf[DoxInlineParseState]
    (msgs, result, s)
  }

  // def toDox(ps: Seq[Dox]): Dox = ps.filter {
  //   case m: Div if m.contents.isEmpty => false
  //   case m: Span if m.contents.isEmpty => false
  //   case _ => true
  // }.toList match {
  //   case Nil => Dox.empty
  //   case x :: Nil => x
  //   case xs => Div(xs)
  // }

  case class Config(
    isDebug: Boolean = false,
    isLocation: Boolean = true,
    markdown: Config.MarkDown = Config.MarkDown.none,
    orgmode: Config.OrgMode = Config.OrgMode.none
  ) extends ParseConfig {
    def isSpace(c: Char): Boolean = Character.isWhitespace(c)

    def useAngleBracket: Boolean = true
    def useBracket: Boolean = true
    def useBrace: Boolean = false
    def useAsterisc: Boolean = markdown.isBold || markdown.isBold2 || orgmode.isBold
    def useUnderscore: Boolean = markdown.isItalic || markdown.isItalic2 || orgmode.isUnderline
    def useBackQuote: Boolean = false
    def useTilde: Boolean = orgmode.isVerbatim
    def useColon: Boolean = false
    def useEqual: Boolean = orgmode.isCode
    def usePlus: Boolean = orgmode.isStrikeThrough
    def useSlash: Boolean = orgmode.isItalic
    def useDallor: Boolean = false

    def isImageFile(uri: URI): Boolean = MimeType.isImageFile(uri)
    def isImageFile(filename: String): Boolean = MimeType.isImageFile(filename)
  }
  object Config {
    val default = Config()
    val debug = Config(true)
    val smartdox = default
    val orgmode = default.copy(orgmode = Config.OrgMode.full)
    val markdown = default.copy(markdown = Config.MarkDown.full)
    val literateModel = default.copy(
      orgmode = Config.OrgMode.model,
      markdown = Config.MarkDown.model
    )

    case class MarkDown(
      isBold: Boolean, // *bold*
      isBold2: Boolean, // **bold**
      isItalic: Boolean, // _italic_
      isItalic2: Boolean, // __italic__
      isCode: Boolean, // `code`
      isStrikeThrough: Boolean, // ~~strike through~~
      isEmoji: Boolean // :emoji:
    )
    object MarkDown {
      val none = MarkDown(false, false, false, false, false, false, false)
      val full = MarkDown(true, true, true, true, true, true, true)
      val model = none
    }

    case class OrgMode(
      isBold: Boolean, // *bold*
      isItalic: Boolean, // /italics/
      isCode: Boolean, // =code=
      isVerbatim: Boolean, // ~verbatim~
      isStrikeThrough: Boolean, // +strike through+
      isUnderline: Boolean // _underline_
    )
    object OrgMode {
      val none = OrgMode(false, false, false, false, false, false)
      val full = OrgMode(true, true, true, true, true, true)
      val model = none
    }
  }

  trait DoxInlineParseState extends ParseReaderWriterState[Config, Dox] {
    def config: Config
    protected def is_Debug: Boolean = false

    def apply(config: Config, evt: ParseEvent): Transition = {
      //      println(s"in($this): $evt")
      if (is_Debug)
        show_Before(config, evt)
      val r = handle_event(evt)
      if (is_Debug)
        show_After(config, evt, r)
//      println(s"out($this): $r")
      r
    }

    protected def show_Before(config: Config, evt: ParseEvent): Unit = {
      // println(s"DoxInlineParseState[${getClass.getSimpleName}]#show_Before $evt") // TODO trace
    }

    protected def show_After(config: Config, evt: ParseEvent, transition: Transition): Unit = {
      // println(s"DoxInlineParseState[${getClass.getSimpleName}]#show_After $evt, $transition") // TODO trace
    }

    def returnEndResult: ParseResult[Dox] = RAISE.noReachDefect(this, "returnEnd")

    def returnFrom(c: Char): DoxInlineParseState = RAISE.noReachDefect(this, "returnFrom")

    def returnFrom(dox: Seq[Dox]): DoxInlineParseState = RAISE.noReachDefect(this, "returnFrom")

    def returnInlineFrom(dox: Seq[Inline]): DoxInlineParseState = returnFrom(dox)

    protected final def handle_event(evt: ParseEvent): Transition =
      evt match {
        case StartEvent => handle_start()
        case EndEvent => handle_end()
        case m: LineEndEvent => handle_line_end(m)
        case m: CharEvent => handle_char_event(m)
      }

    protected def is_space(c: Char): Boolean = config.isSpace(c)
    protected def use_space: Boolean = true
    protected def use_parenthesis: Boolean = false
    protected def use_angle_bracket: Boolean = config.useAngleBracket
    protected def use_bracket: Boolean = config.useBracket
    protected def use_brace: Boolean = config.useBrace
    protected def use_asterisc: Boolean = config.useAsterisc
    protected def use_underscore: Boolean = config.useUnderscore
    protected def use_back_quote: Boolean = config.useBackQuote
    protected def use_tilde: Boolean = config.useTilde
    protected def use_colon: Boolean = config.useColon
    protected def use_equal: Boolean = config.useEqual
    protected def use_plus: Boolean = config.usePlus
    protected def use_slash: Boolean = config.useSlash
    protected def use_dallor: Boolean = config.useDallor

    protected def handle_char_event(p: CharEvent): Transition = p.c match {
      case c if use_space && is_space(c) => handle_space(p)
      case '(' if use_parenthesis => handle_open_parenthesis(p)
      case ')' if use_parenthesis => handle_close_parenthesis(p)
      case '<' if use_angle_bracket => handle_open_angle_bracket(p)
      case '>' if use_angle_bracket => handle_close_angle_bracket(p)
      case '[' if use_bracket => handle_open_bracket(p)
      case ']' if use_bracket => handle_close_bracket(p)
      case '{' if use_brace => handle_open_brace(p)
      case '}' if use_brace => handle_close_brace(p)
      case '*' if use_asterisc => handle_asterisc(p)
      case '_' if use_underscore => handle_underscore(p)
      case '`' if use_back_quote => handle_backquote(p)
      case '~' if use_tilde => handle_tilde(p)
      case ':' if use_colon => handle_colon(p)
      case '=' if use_equal => handle_equal(p)
      case '+' if use_plus => handle_plus(p)
      case '/' if use_slash => handle_slash(p)
      case '$' if use_dallor => handle_dallor(p)
      case _ => handle_character(p)
    }

    protected final def handle_start(): Transition =
      handle_Start()

    protected def handle_Start(): Transition =
      (ParseMessageSequence.empty, start_Result(), start_State())

    protected def start_Result(): ParseResult[Dox] =
      EmptyParseResult()

    protected def start_State(): DoxInlineParseState = this

    protected final def handle_end(): Transition =
      handle_End()

    protected def handle_End(): Transition =
      (ParseMessageSequence.empty, end_Result(), end_State())

    protected def end_Result(): ParseResult[Dox] =
      RAISE.notImplementedYetDefect(s"end_Result(${getClass.getSimpleName})")

    protected def end_State(): DoxInlineParseState = EndState(config)

    protected final def handle_line_end(evt: LineEndEvent): Transition =
      handle_Line_End(evt)

    protected def handle_Line_End(evt: LineEndEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, line_End_State(evt))

    protected def line_End_State(evt: LineEndEvent): DoxInlineParseState = RAISE.notImplementedYetDefect

    protected final def handle_space(evt: CharEvent): Transition =
      handle_Space(evt)

    protected def handle_Space(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, space_State(evt))

    protected def space_State(evt: CharEvent): DoxInlineParseState =
      space_State(evt.c)

    protected def space_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect(s"spaceState(${getClass.getSimpleName}): $c")

    protected final def handle_open_parenthesis(evt: CharEvent): Transition =
      handle_Open_Parenthesis(evt)

    protected def handle_Open_Parenthesis(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Parenthesis_State(evt))

    protected def open_Parenthesis_State(evt: CharEvent): DoxInlineParseState =
      open_Parenthesis_State(evt.c)

    protected def open_Parenthesis_State(c: Char): DoxInlineParseState =
      LinkState(config, this)

    protected final def handle_close_parenthesis(evt: CharEvent): Transition =
      handle_Close_Parenthesis(evt)

    protected def handle_Close_Parenthesis(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Parenthesis_State(evt))

    protected def close_Parenthesis_State(evt: CharEvent): DoxInlineParseState =
      close_Parenthesis_State(evt.c)

    protected def close_Parenthesis_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect

    protected final def handle_open_angle_bracket(evt: CharEvent): Transition =
      handle_Open_Angle_Bracket(evt)

    protected def handle_Open_Angle_Bracket(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Angle_Bracket_State(evt))

    protected def open_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
      open_Angle_Bracket_State(evt.c)

    protected def open_Angle_Bracket_State(c: Char): DoxInlineParseState =
      OpenTagState(config, this)

    protected final def handle_close_angle_bracket(evt: CharEvent): Transition = {
      // println(s"${getClass.getSimpleName}#handle_close_angle_bracket: $evt")
      handle_Close_Angle_Bracket(evt)
    }

    protected def handle_Close_Angle_Bracket(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Angle_Bracket_State(evt))

    protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
      close_Angle_Bracket_State(evt.c)

    protected def close_Angle_Bracket_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect

    protected final def handle_open_bracket(evt: CharEvent): Transition =
      handle_Open_Bracket(evt)

    protected def handle_Open_Bracket(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Bracket_State(evt))

    protected def open_Bracket_State(evt: CharEvent): DoxInlineParseState =
      open_Bracket_State(evt.c)

    protected def open_Bracket_State(c: Char): DoxInlineParseState =
      LinkState(config, this)

    protected final def handle_close_bracket(evt: CharEvent): Transition =
      handle_Close_Bracket(evt)

    protected def handle_Close_Bracket(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Bracket_State(evt))

    protected def close_Bracket_State(evt: CharEvent): DoxInlineParseState =
      close_Bracket_State(evt.c)

    protected def close_Bracket_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect

    protected final def handle_open_brace(evt: CharEvent): Transition =
      handle_Open_Brace(evt)

    protected def handle_Open_Brace(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Brace_State(evt))

    protected def open_Brace_State(evt: CharEvent): DoxInlineParseState =
      open_Brace_State(evt.c)

    protected def open_Brace_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect

    protected final def handle_close_brace(evt: CharEvent): Transition =
      handle_Close_Brace(evt)

    protected def handle_Close_Brace(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Brace_State(evt))

    protected def close_Brace_State(evt: CharEvent): DoxInlineParseState =
      close_Brace_State(evt.c)

    protected def close_Brace_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect

    protected final def handle_asterisc(evt: CharEvent): Transition =
      handle_Asterisc(evt)

    protected def handle_Asterisc(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, asterisc_State(evt))

    protected def asterisc_State(evt: CharEvent): DoxInlineParseState =
      asterisc_State(evt.c)

    protected def asterisc_State(c: Char): DoxInlineParseState =
      InlineState(BoldState(config, this), '*')

    protected final def handle_underscore(evt: CharEvent): Transition =
      handle_Underscore(evt)

    protected def handle_Underscore(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, underscore_State(evt))

    protected def underscore_State(evt: CharEvent): DoxInlineParseState =
      underscore_State(evt.c)

    protected def underscore_State(c: Char): DoxInlineParseState =
      if (config.markdown.isItalic || config.markdown.isItalic2)
        InlineState(ItalicState(config, this), '_')
      else if (config.orgmode.isUnderline)
        InlineState(UnderlineState(config, this), '_')
      else
        character_State(c)

    protected final def handle_backquote(evt: CharEvent): Transition =
      handle_Backquote(evt)

    protected def handle_Backquote(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, backquote_State(evt))

    protected def backquote_State(evt: CharEvent): DoxInlineParseState =
      backquote_State(evt.c)

    protected def backquote_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect

    protected final def handle_tilde(evt: CharEvent): Transition =
      handle_Tilde(evt)

    protected def handle_Tilde(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, tilde_State(evt))

    protected def tilde_State(evt: CharEvent): DoxInlineParseState =
      tilde_State(evt.c)

    protected def tilde_State(c: Char): DoxInlineParseState = PreState(config, this, '~')

    protected final def handle_colon(evt: CharEvent): Transition =
      handle_Colon(evt)

    protected def handle_Colon(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, colon_State(evt))

    protected def colon_State(evt: CharEvent): DoxInlineParseState =
      colon_State(evt.c)

    protected def colon_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect

    protected final def handle_equal(evt: CharEvent): Transition =
      handle_Equal(evt)

    protected def handle_Equal(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, equal_State(evt))

    protected def equal_State(evt: CharEvent): DoxInlineParseState =
      equal_State(evt.c)

    protected def equal_State(c: Char): DoxInlineParseState =
      InlineState(CodeState(config, this), '=')

    protected final def handle_plus(evt: CharEvent): Transition =
      handle_Plus(evt)

    protected def handle_Plus(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, plus_State(evt))

    protected def plus_State(evt: CharEvent): DoxInlineParseState =
      plus_State(evt.c)

    protected def plus_State(c: Char): DoxInlineParseState =
      InlineState(StrikeThroughState(config, this), '+')

    protected final def handle_slash(evt: CharEvent): Transition =
      handle_Slash(evt)

    protected def handle_Slash(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, slash_State(evt))

    protected def slash_State(evt: CharEvent): DoxInlineParseState =
      slash_State(evt.c)

    protected def slash_State(c: Char): DoxInlineParseState =
      InlineState(ItalicState(config, this), '/')

    protected final def handle_dallor(evt: CharEvent): Transition =
      handle_Dallor(evt)

    protected def handle_Dallor(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, dallor_State(evt))

    protected def dallor_State(evt: CharEvent): DoxInlineParseState =
      dallor_State(evt.c)

    protected def dallor_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect

    protected final def handle_character(evt: CharEvent): Transition =
      handle_Character(evt)

    protected def handle_Character(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, character_State(evt))

    protected def character_State(evt: CharEvent): DoxInlineParseState =
      character_State(evt.c)

    protected def character_State(c: Char): DoxInlineParseState =
      RAISE.notImplementedYetDefect(this, s"character_State: $c")

    //
    protected final def make_text(ps: Seq[Dox]): String = ps.map(_.toText).mkString
  }

  trait ChildDoxInlineParseState extends DoxInlineParseState {
    def parent: DoxInlineParseState

    protected def leave_end: ParseResult[Dox] = parent.returnEndResult

    protected def leave_none: DoxInlineParseState = parent

    protected def leave_to(c: Char): DoxInlineParseState = parent.returnFrom(c)

    protected def leave_inline_to(dox: Inline): DoxInlineParseState = parent.returnInlineFrom(Vector(dox))
    protected def leave_inline_to(doxes: Seq[Inline]): DoxInlineParseState = parent.returnInlineFrom(doxes)

    protected def leave_to(dox: Dox): DoxInlineParseState = parent.returnFrom(Vector(dox))
    protected def leave_to(doxes: Seq[Dox]): DoxInlineParseState = parent.returnFrom(doxes)
  }

  case class EndState(config: Config) extends DoxInlineParseState {
  }

  case class NormalState(
    config: Config,
    doxes: Vector[Dox] = Vector.empty,
    cs: Vector[Char] = Vector.empty,
    isInSpace: Boolean = false
  ) extends DoxInlineParseState with InlineFeature {
    override def is_Debug = true

    override def returnEndResult: ParseResult[Dox] =
      ParseSuccess(Text(cs.mkString))

    override def returnFrom(c: Char): DoxInlineParseState = character_State(c)

    override def returnFrom(ps: Seq[Dox]): DoxInlineParseState =
      (cs.isEmpty, isInSpace) match {
        case (true, true) =>
          copy(
            doxes = (doxes :+ Text(" ")) ++ ps,
            isInSpace = false
          )
        case (true, false) =>
          copy(
            doxes = doxes ++ ps,
            isInSpace = false
          )
        case (false, true) =>
        copy(
          cs = Vector.empty,
          doxes = (doxes :+ Text(cs.mkString :+ ' ')) ++ ps,
          isInSpace = false
        )
        case (false, false) => 
        copy(
          cs = Vector.empty,
          doxes = (doxes :+ Text(cs.mkString)) ++ ps
        )
      }

    override protected def end_Result(): ParseResult[Dox] =
      if (cs.isEmpty)
        ParseSuccess(Dox.toDox(doxes))
      else
        ParseSuccess(Dox.toDox(doxes :+ Text(cs.mkString)))

    override protected def space_State(c: Char): DoxInlineParseState =
      if (doxes.isEmpty && cs.isEmpty)
        this
      else
        copy(isInSpace = true)

    override protected def character_State(c: Char): DoxInlineParseState =
      if (isInSpace)
        copy(cs = cs :+ ' ' :+ c, isInSpace = false)
      else
        copy(cs = cs :+ c)
  }
  object NormalState {
    def init(config: Config) = NormalState(config)
  }

  trait RawFeature { self: DoxInlineParseState =>
    override protected def use_space: Boolean = false
    override protected def use_parenthesis: Boolean = false
    override protected def use_angle_bracket: Boolean = false 
    override protected def use_bracket: Boolean = false 
    override protected def use_brace: Boolean = false 
    override protected def use_asterisc: Boolean = false 
    override protected def use_underscore: Boolean = false 
    override protected def use_back_quote: Boolean = false 
    override protected def use_tilde: Boolean = false 
    override protected def use_colon: Boolean = false 
    override protected def use_equal: Boolean = false 
    override protected def use_plus: Boolean = false 
    override protected def use_slash: Boolean = false 
    override protected def use_dallor: Boolean = false 
  }

  trait InlineFeature { self: DoxInlineParseState =>
    def doxes: Vector[Dox]
    def cs: Vector[Char]

    protected def make_dox: Vector[Dox] =
      if (cs.isEmpty)
        doxes
      else
        doxes :+ Text(cs.mkString)

    protected def make_inline_dox: Vector[Inline] =
      make_dox.map {
        case m: Inline => m
        case m => ???
      }

    protected def make_dox(c: Char): Vector[Dox] = 
      doxes :+ Text((cs :+ c).mkString)
  }

  case class InlineState(
    config: Config,
    parent: DoxInlineParseState,
    // matchp: CharEvent => Boolean,
    closeChar1: Char,
    closeChar2: Option[Char] = None,
    doxes: Vector[Dox] = Vector.empty,
    cs: Vector[Char] = Vector.empty,
    isInSpace: Boolean = false
  ) extends ChildDoxInlineParseState with InlineFeature {
    protected lazy val is_match = closeChar2.map(x =>
      (evt: CharEvent) => evt.c == closeChar1 && evt.next == Some(x)
    ).getOrElse(
      (evt: CharEvent) => evt.c == closeChar1
    )

    override def returnFrom(c: Char): DoxInlineParseState = character_State(c)

    override def returnFrom(ps: Seq[Dox]): DoxInlineParseState = 
      if (cs.isEmpty)
        copy(
          doxes = doxes ++ ps,
          isInSpace = false
        )
      else if (isInSpace)
        copy(
          cs = Vector.empty,
          doxes = (doxes :+ Text(cs.mkString :+ ' ')) ++ ps,
          isInSpace = false
        )
      else
        copy(
          cs = Vector.empty,
          doxes = (doxes :+ Text(cs.mkString)) ++ ps
        )

    protected def leave_to_inline_dox = {
      val r = closeChar2.
        map(x =>
          SkipOneState(leave_inline_to(make_inline_dox), x)
        ).getOrElse(
          leave_inline_to(make_inline_dox)
        )
      // println(s"leave_to_inline_dox: $r")
      r
    }

    override protected def end_Result(): ParseResult[Dox] =
      RAISE.notImplementedYetDefect(s"end_Result(${getClass.getSimpleName}): $this")

    override protected def open_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState = {
      val r = if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)
      r
    }

    override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
      character_State(evt.c) // XXX warn?

    override protected def close_Bracket_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def close_Brace_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def asterisc_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def underscore_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def backquote_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def tilde_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def colon_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def equal_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def plus_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def slash_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

    override protected def space_State(c: Char): DoxInlineParseState =
      if (doxes.isEmpty && cs.isEmpty)
        this
      else
        copy(isInSpace = true)

    override protected def character_State(c: Char): DoxInlineParseState = {
      val r = if (isInSpace)
        copy(cs = cs :+ ' ' :+ c, isInSpace = false)
      else
        copy(cs = cs :+ c)
      // println(s"${getClass.getSimpleName}#character_State: $c => $r")
      r
    }
  }
  object InlineState {
    def apply(
      parent: DoxInlineParseState,
      closeChar: Char
    ): InlineState = InlineState(parent.config, parent, closeChar)

    def apply(
      parent: DoxInlineParseState,
      closeChar1: Char,
      closeChar2: Char
    ): InlineState = InlineState(parent.config, parent, closeChar1, Some(closeChar2))

    // def create(
    //   parent: DoxInlineParseState,
    //   closeChar1: Char,
    //   closeChar2: Char
    // ): DoxInlineParseState = SkipOneState(InlineState(parent, closeChar1, closeChar2), closeChar2)
  }

  case class SkipSpaceState(
    config: Config,
    parent: DoxInlineParseState
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def use_space: Boolean = true

    override protected def space_State(c: Char): DoxInlineParseState = this

    override protected def character_State(c: Char): DoxInlineParseState =
      leave_to(c)
  }

  case class SkipSpaceStartState(
    config: Config,
    parent: DoxInlineParseState,
    startChar: Char
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def use_space: Boolean = true

    override protected def space_State(c: Char): DoxInlineParseState = this

    override protected def character_State(c: Char): DoxInlineParseState =
      if (c == startChar)
        leave_none
      else
        RAISE.noReachDefect(this, s"SkipSpaceStartState#character_State($this): $c")
  }

  case class SkipOneState(
    config: Config,
    parent: DoxInlineParseState,
    skipChar: Char
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def character_State(c: Char): DoxInlineParseState = {
      // println(s"SkipOneState: $c")
      if (c == skipChar)
        leave_none
      else
        RAISE.noReachDefect(this, s"SkipOneState#character_State($this): $c")
    }
  }
  object SkipOneState {
    def apply(
      parent: DoxInlineParseState,
      skipChar: Char
    ): SkipOneState = SkipOneState(parent.config, parent, skipChar)
  }

  // case class AfterSpaceState(
  //   parent: DoxInlineParseState
  // ) extends ChildDoxInlineParseState {
  //   override protected def end_Result(): ParseResult[Dox] =
  //     leave_end

  //   override protected def space_State(c: Char): DoxInlineParseState = this

  //   override protected def character_State(c: Char): DoxInlineParseState =
  //     leave_to(c)
  // }

  /*
   * Markdown: [example](http://www.example.com "Title")
   * Org-mode: [[http://www.example.com][example]]
   */
  case class LinkState(
    config: Config,
    parent: DoxInlineParseState
  ) extends ChildDoxInlineParseState {
    override def returnFrom(doxes: Seq[Dox]) = ???

    override protected def end_Result(): ParseResult[Dox] =
      leave_end

    override protected def space_State(c: Char): DoxInlineParseState = this

    override protected def open_Bracket_State(c: Char): DoxInlineParseState =
      InlineState(OrgModeLinkUrnState(config, parent), ']')

    override protected def close_Bracket_State(c: Char): DoxInlineParseState =
      ???

    override protected def character_State(c: Char): DoxInlineParseState =
      InlineState(this, ']') // XXX
  }

  // case class OrgModeLinkState(
  //   config: Config,
  //   parent: DoxInlineParseState
  // ) extends ChildDoxInlineParseState {
  //   override protected def use_bracket = true

  //   override def returnFrom(doxes: Seq[Dox]) = ???

  //   override protected def open_Bracket_State(c: Char): DoxInlineParseState =
  //     ???

  //   override protected def close_Bracket_State(c: Char): DoxInlineParseState =
  //     ???

  //   override protected def character_State(c: Char): DoxInlineParseState =
  //     InlineState(OrgModeLinkUrnState(this), ']')
  // }

  case class OrgModeLinkUrnState(
    config: Config,
    parent: DoxInlineParseState,
    urn: Seq[Inline] = Vector.empty
  ) extends ChildDoxInlineParseState {
    override protected def use_bracket = true

    override def returnInlineFrom(doxes: Seq[Inline]) = copy(urn = doxes)

    override protected def open_Bracket_State(c: Char): DoxInlineParseState =
      InlineState(OrgModeLinkLabelState(config, parent, urn), ']', ']')

    override protected def close_Bracket_State(c: Char): DoxInlineParseState = {
      // XXX annotation, block, figure
      val uri = make_text(urn)
      if (config.isImageFile(uri))
        leave_to(ReferenceImg(uri))
      else
        leave_to(Hyperlink(urn, uri))
    }
  }
  object OrgModeLinkUrnState {
    def apply(p: DoxInlineParseState): OrgModeLinkUrnState = OrgModeLinkUrnState(
      p.config,
      p
    )
  }

  case class OrgModeLinkLabelState(
    config: Config,
    parent: DoxInlineParseState,
    urn: Seq[Inline]
  ) extends ChildDoxInlineParseState {
    override def returnInlineFrom(doxes: Seq[Inline]) = {
      val location = None // TODO
      val uri = make_text(urn)
      if (config.isImageFile(uri))
        leave_to(ReferenceImg(uri))
      else
        leave_to(Hyperlink(doxes, uri, location))
    }
  }

  case class BoldState(
    config: Config,
    parent: DoxInlineParseState
  ) extends ChildDoxInlineParseState {
    override def returnInlineFrom(dox: Seq[Inline]): DoxInlineParseState =
      leave_to(Bold(dox.toList))
  }

  case class ItalicState(
    config: Config,
    parent: DoxInlineParseState
  ) extends ChildDoxInlineParseState {
    override def returnInlineFrom(dox: Seq[Inline]): DoxInlineParseState =
      leave_to(Italic(dox.toList))
  }

  case class UnderlineState(
    config: Config,
    parent: DoxInlineParseState
  ) extends ChildDoxInlineParseState {
    override def returnInlineFrom(dox: Seq[Inline]): DoxInlineParseState =
      leave_to(Underline(dox.toList))
  }

  case class PreState(
    config: Config,
    parent: DoxInlineParseState,
    closeChar: Char,
    cs: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def character_State(c: Char): DoxInlineParseState =
      if (c == closeChar)
        leave_to(Pre(cs.mkString))
      else
        copy(cs = cs :+ c)
  }

  case class CodeState(
    config: Config,
    parent: DoxInlineParseState
  ) extends ChildDoxInlineParseState {
    override def returnInlineFrom(dox: Seq[Inline]): DoxInlineParseState =
      leave_to(Code(dox.toList))
  }

  case class StrikeThroughState(
    config: Config,
    parent: DoxInlineParseState
  ) extends ChildDoxInlineParseState {
    override def returnInlineFrom(dox: Seq[Inline]): DoxInlineParseState =
      leave_to(Del(dox.toList))
  }

  case class OpenTagState(
    config: Config,
    parent: DoxInlineParseState,
    name: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    def resultOpenClose(ps: Vector[(String, String)]) =
      InlineState(CloseTagState(config, parent, name.mkString, ps), '<', '/')

    def resultOpenEnd(ps: Vector[(String, String)]) = ???

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '>' => InlineState(CloseTagState(config, parent, name.mkString, Vector.empty), '<', '/')
        case ' ' => SkipSpaceState(config, TagAttributeListState(config, this))
        case m => copy(name = name :+ m)
      }
  }

  case class TagAttributeListState(
    config: Config,
    parent: OpenTagState,
    attrs: Vector[(String, String)] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    override def returnFrom(c: Char): DoxInlineParseState =
      TagAttributeState(config, this, Vector(c))

    def resultFrom(key: String, value: String) = copy(attrs = attrs :+ (key, value))

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '>' => parent.resultOpenClose(attrs)
        case '/' => SkipOneState(config, parent.resultOpenEnd(attrs), '>')
        case ' ' => SkipSpaceState(config, TagAttributeState(config, this))
      }
  }

  case class TagAttributeState(
    config: Config,
    parent: TagAttributeListState,
    key: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    def resultFrom(p: String): DoxInlineParseState = parent.resultFrom(key.mkString, p)

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '>' => ???
        case '/' => ???
        case ' ' => this
        case '=' => SkipSpaceStartState(config, TagAttributeValueState(config, this), '"')
        case m => copy(key = key :+ m)
      }
  }

  case class TagAttributeValueState(
    config: Config,
    parent: TagAttributeState,
    value: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '"' => parent.resultFrom(value.mkString)
        case m => copy(value = value :+ m)
      }
  }

  // case class InTagState(
  //   config: Config,
  //   parent: DoxInlineParseState,
  //   name: String,
  //   attrs: Vector[(String, String)],
  //   dox: Vector[Dox] = Vector.empty
  // ) extends ChildDoxInlineParseState {
    
  // }

  case class CloseTagState(
    config: Config,
    parent: DoxInlineParseState,
    name: String,
    attrs: Vector[(String, String)],
    dox: Vector[Dox] = Vector.empty,
    closeName: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    private val _ctx = Dox2Parser.ParseContext.now() // TODO
    implicit def dtctx = _ctx.dateTimeContext

    override def returnFrom(ps: Seq[Dox]): DoxInlineParseState = copy(dox = ps.toVector)

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '>' =>
          if (closeName.mkString != name)
            RAISE.syntaxErrorFault(s"Tag name unmatch: $name != ${closeName.mkString}")
          leave_to(Dox.create(name, attrs, dox))
        case ' ' => this
        case m => copy(closeName = closeName :+ m)
      }
  }
}
