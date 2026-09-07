package org.smartdox.parser

import java.net.URI
import java.nio.file.Path
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.parser._
import org.goldenport.io.MimeType
import org.smartdox._
import org.smartdox.diagnostics.{StructuredRenderingDiagnostic, StructuredRenderingDiagnosticException}

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
 *  version Jun. 10, 2025
 *  version Jul. 29, 2025
 *  version Sep.  9, 2025
 *  version Oct. 26, 2025
 *  version Nov.  5, 2025
 *  version Apr. 20, 2026
 *  version Jun. 29, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
object DoxInlineParser {
  type Transition = (ParseMessageSequence, ParseResult[Dox], DoxInlineParseState)

  private[smartdox] sealed trait ResourceOrigin
  private[smartdox] object ResourceOrigin {
    final case class Physical(rootpath: Path) extends ResourceOrigin
    final case class Virtual(parentsegments: Vector[String]) extends ResourceOrigin
    case object Absent extends ResourceOrigin
  }
  // def parse(p: LogicalParagraph): Dox = parse(p.lines)

  // def parse(p: LogicalLines): Dox = toDox(p.lines.map(parse))

  def parse(p: LogicalLine): Dox = parse(Config.default.withLocation(p.location), p.text)

  def parse(in: String): Dox = parse(Config.default, in)

  def parse(config: Config, in: String): Dox =
    DoxInlineParserInlineMacro.parse(config, in).getOrElse {
      // println(s"Inline($config): $in")
      val (messages, result, state) = apply(config, in)
      result match {
        case ParseSuccess(dox, _) => _to_dox(dox)
        case ParseFailure(_, _) => RAISE.notImplementedYetDefect
        case EmptyParseResult() => RAISE.notImplementedYetDefect
      }
    }

  // Keep parser-emitted empty generic nodes local to this parser.  Global
  // Dox normalization intentionally removes empty spans and must remain
  // unchanged for callers outside this parser.
  private def _to_dox(ps: Seq[Dox]): Dox = ps.toList match {
    case Nil => Dox.empty
    case x :: Nil => x
    case xs => Fragment(xs)
  }

  // def apply(in: String): (ParseMessageSequence, ParseResult[Vector[Dox]], DoxInlineParseState) =
  //   apply(Config.default, in)

  def apply(config: Config, in: String): (ParseMessageSequence, ParseResult[Vector[Dox]], DoxInlineParseState) = {
    val parserconfig = config._with_token_context(in)
    val parser = ParseReaderWriterStateClass[Config, Dox](parserconfig, NormalState.init(parserconfig))
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
    orgmode: Config.OrgMode = Config.OrgMode.none,
    asciidoc: Config.Asciidoc = Config.Asciidoc.none,
    location: Option[ParseLocation] = None
  ) extends ParseConfig {
    private var _resource_origin: ResourceOrigin = ResourceOrigin.Absent
    private var _source_identity: Option[String] = None
    private var _token_context: Option[String] = None
    def copy(
      isDebug: Boolean = this.isDebug,
      isLocation: Boolean = this.isLocation,
      markdown: Config.MarkDown = this.markdown,
      orgmode: Config.OrgMode = this.orgmode,
      asciidoc: Config.Asciidoc = this.asciidoc,
      location: Option[ParseLocation] = this.location
    ): Config = {
      val result = Config(isDebug, isLocation, markdown, orgmode, asciidoc, location)
      result._resource_origin = _resource_origin
      result._source_identity = _source_identity
      result._token_context = _token_context
      result
    }

    def withLocation(p: Option[ParseLocation]): Config =
      copy(location = p)
    def withLocation(p: ParseLocation): Config = withLocation(p.toOption)

    def withResourceRoot(p: Path): Config = {
      val result = copy()
      result._resource_origin = ResourceOrigin.Physical(p.toAbsolutePath.normalize)
      result
    }
    private[smartdox] def _with_virtual_resource_parent(parent: String): Config = {
      val result = copy()
      result._resource_origin = Config._normalize_virtual_parent(parent).
        map(ResourceOrigin.Virtual).
        getOrElse(ResourceOrigin.Absent)
      result
    }
    private[parser] def _resource_origin_context: ResourceOrigin = _resource_origin
    private[parser] def _with_source_identity(p: Option[URI]): Config = {
      val result = copy()
      result._source_identity = p.map(_.toString)
      result
    }
    private[parser] def _source_identity_option: Option[String] = _source_identity
    private[parser] def _with_token_context(p: String): Config = {
      val result = copy()
      result._token_context = Some(p)
      result
    }
    private[parser] def _token_context_option: Option[String] = _token_context
    private[parser] def _with_resource_origin(origin: ResourceOrigin): Config = {
      val result = copy()
      result._resource_origin = origin
      result
    }
    private[parser] def _resource_root_option: Option[Path] = _resource_origin match {
      case ResourceOrigin.Physical(rootpath) => Some(rootpath)
      case _ => None
    }
    def isSpace(c: Char): Boolean = Character.isWhitespace(c)

    def useAngleBracket: Boolean = true
    def useBracket: Boolean = true
    def useBrace: Boolean = false
    def useAsterisc: Boolean = markdown.isBold || markdown.isBold2 || orgmode.isBold
    def useUnderscore: Boolean = markdown.isItalic || markdown.isItalic2 || orgmode.isUnderline
    def useBackQuote: Boolean = markdown.isBackQuote
    def useTilde: Boolean = orgmode.isVerbatim
    def useColon: Boolean = asciidoc.isInlineMacro
    def useEqual: Boolean = orgmode.isCode
    def usePlus: Boolean = orgmode.isStrikeThrough
    def useSlash: Boolean = orgmode.isItalic
    def useDallor: Boolean = false
    def useMarkdownImage: Boolean = markdown != Config.MarkDown.none

    def isImageFile(uri: URI): Boolean = MimeType.isImageFile(uri)
    def isImageFile(filename: String): Boolean = MimeType.isImageFile(filename)
  }
  object Config {
    val default = Config()
    val debug = Config(true)
    val smartdox = default.copy(
      orgmode = Config.OrgMode.smartdox,
      markdown = Config.MarkDown.smartdox,
      asciidoc = Config.Asciidoc.smartdox
    )
    val orgmode = default.copy(orgmode = Config.OrgMode.full)
    val markdown = default.copy(markdown = Config.MarkDown.full)
    val literateModel = default.copy(
      orgmode = Config.OrgMode.model,
      markdown = Config.MarkDown.model
    )

    private[smartdox] def _normalize_virtual_parent(path: String): Option[Vector[String]] = {
      path.split("/", -1).foldLeft(Option(Vector.empty[String])) { (state, segment) =>
        state.flatMap { parentsegments =>
          segment match {
            case "" | "." => Some(parentsegments)
            case ".." =>
              if (parentsegments.nonEmpty)
                Some(parentsegments.dropRight(1))
              else
                None
            case _ => Some(parentsegments :+ segment)
          }
        }
      }
    }

    case class MarkDown(
      isBold: Boolean, // *bold*
      isBold2: Boolean, // **bold**
      isItalic: Boolean, // _italic_
      isItalic2: Boolean, // __italic__
      isCode: Boolean, // `code`
      isStrikeThrough: Boolean, // ~~strike through~~
      isEmoji: Boolean // :emoji:
    ) {
      def isBackQuote = isCode
    }
    object MarkDown {
      val none = MarkDown(false, false, false, false, false, false, false)
      val full = MarkDown(true, true, true, true, true, true, true)
      val smartdox = none.copy(isCode = true)
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
      val smartdox = none.copy(isVerbatim = true)
      val model = none
    }

    case class Asciidoc(
      isInlineMacro: Boolean = false
    )
    object Asciidoc {
      val none = Asciidoc()
      val full = Asciidoc(isInlineMacro = true)
      val smartdox = Asciidoc(isInlineMacro = true)
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

    def returnCharsFrom(cs: Seq[Char]): DoxInlineParseState = RAISE.noReachDefect(this, "returnCharsFrom")

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
    protected def use_markdown_image: Boolean = config.useMarkdownImage

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
      case '!' if use_markdown_image => handle_markdown_image(p)
      case _ => handle_character(p)
    }
    protected final def handle_markdown_image(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, markdown_image_state(evt))

    protected def markdown_image_state(evt: CharEvent): DoxInlineParseState = MarkdownImageInlineParser.state(config, this, evt)

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
      RAISE.notImplementedYetDefect(s"close_Angle_Bracket_State(${getClass.getSimpleName}): $c")

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
      InlineState(BoldState(config, this), '*', evt.location)

    protected def asterisc_State(c: Char): DoxInlineParseState =
      InlineState(BoldState(config, this), '*')

    protected final def handle_underscore(evt: CharEvent): Transition =
      handle_Underscore(evt)

    protected def handle_Underscore(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, underscore_State(evt))

    protected def underscore_State(evt: CharEvent): DoxInlineParseState =
      if (config.markdown.isItalic || config.markdown.isItalic2)
        InlineState(ItalicState(config, this), '_', evt.location)
      else if (config.orgmode.isUnderline)
        InlineState(UnderlineState(config, this), '_', evt.location)
      else
        character_State(evt.c)

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
      RawState(CodeState(config, this), '`')

    protected final def handle_tilde(evt: CharEvent): Transition =
      handle_Tilde(evt)

    protected def handle_Tilde(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, tilde_State(evt))

    protected def tilde_State(evt: CharEvent): DoxInlineParseState =
      InlineState(CodeState.console(config, this), '~', evt.location)

    protected def tilde_State(c: Char): DoxInlineParseState =
      InlineState(CodeState.console(config, this), '~')

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
      RawState(CodeState(config, this), '=')

    protected final def handle_plus(evt: CharEvent): Transition =
      handle_Plus(evt)

    protected def handle_Plus(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, plus_State(evt))

    protected def plus_State(evt: CharEvent): DoxInlineParseState =
      InlineState(StrikeThroughState(config, this), '+', evt.location)

    protected def plus_State(c: Char): DoxInlineParseState =
      InlineState(StrikeThroughState(config, this), '+')

    protected final def handle_slash(evt: CharEvent): Transition =
      handle_Slash(evt)

    protected def handle_Slash(evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, slash_State(evt))

    protected def slash_State(evt: CharEvent): DoxInlineParseState =
      InlineState(ItalicState(config, this), '/', evt.location)

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
    protected final def make_text(ps: Seq[Dox]): String = ps.map(_.toPlainText).mkString

    protected final def to_transition(p: DoxInlineParseState) =
      (ParseMessageSequence.empty, ParseResult.empty, p)
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

    protected final def leave_to_chars(cs: Seq[Char]): DoxInlineParseState =
      parent.returnCharsFrom(cs)

    protected final def leave_to_urn(urn: Seq[Inline]): DoxInlineParseState = {
      // XXX annotation, block, figure
      val uri = make_text(urn)
      Consequence(new URI(uri)) match {
        case Consequence.Success(x, _) =>
          if (config.isImageFile(uri))
            leave_to(Dox.attachLocation(ReferenceImg(uri), config.location))
          else
            leave_to(Dox.attachLocation(Hyperlink(urn, x), config.location))
        case m: Consequence.Error[_] => leave_to(Text(m.message))
      }
    }

    protected final def leave_to_urn(urn: Seq[Inline], label: Seq[Inline]): DoxInlineParseState = {
      // XXX annotation, block, figure
      val uri = make_text(urn)
      if (config.isImageFile(uri))
        leave_to(Dox.attachLocation(ReferenceImg(uri), config.location))
      else
        leave_to(Dox.attachLocation(Hyperlink(label, uri), config.location))
    }
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
      ParseSuccess(_result_dox(doxes, cs))

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

    override def returnCharsFrom(cs0: Seq[Char]): DoxInlineParseState = {
      if (cs0.isEmpty) {
        this
      } else {
        // Treat returned characters as plain text continuation
        val s = cs0.mkString
        if (isInSpace) {
          copy(
            cs = cs ++ (' ' +: s.toVector),
            isInSpace = false
          )
        } else {
          copy(
            cs = cs ++ s.toVector
          )
        }
      }
    }

    override protected def end_Result(): ParseResult[Dox] =
      ParseSuccess(_result_dox(doxes, cs))

    private def _result_dox(values: Seq[Dox], chars: Seq[Char]): Dox = {
      val result = if (chars.isEmpty) values else values :+ Text(chars.mkString)
      result.toList match {
        case Nil => Dox.empty
        case x :: Nil => x
        case xs => Fragment(xs)
      }
    }

    override protected def close_Angle_Bracket_State(c: Char): DoxInlineParseState =
      copy(cs = cs :+ c)

    override protected def open_Bracket_State(evt: CharEvent): DoxInlineParseState =
      if (cs.lastOption.contains(':'))
        _inline_macro_state_from_open_bracket(evt)
      else
        super.open_Bracket_State(evt)

    override protected def colon_State(evt: CharEvent): DoxInlineParseState =
      if (evt.next.contains('['))
        _inline_macro_state(evt)
      else
        character_State(evt.c)

    private def _inline_macro_state(evt: CharEvent): DoxInlineParseState = {
      val (prefix, name) = DoxInlineParserInlineMacro.splitName(cs)
      if (name.isEmpty)
        character_State(evt.c)
      else {
        val base = prefix match {
          case Some(s) if s.nonEmpty => copy(doxes = doxes :+ Text(s), cs = Vector.empty, isInSpace = false)
          case _ => copy(cs = Vector.empty, isInSpace = false)
        }
        SkipOneState(config, InlineMacroState(config, base, name), '[')
      }
    }

    private def _inline_macro_state_from_open_bracket(evt: CharEvent): DoxInlineParseState = {
      val (prefix, name) = DoxInlineParserInlineMacro.splitName(cs.dropRight(1))
      if (name.isEmpty)
        super.open_Bracket_State(evt)
      else {
        val base = prefix match {
          case Some(s) if s.nonEmpty => copy(doxes = doxes :+ Text(s), cs = Vector.empty, isInSpace = false)
          case _ => copy(cs = Vector.empty, isInSpace = false)
        }
        InlineMacroState(config, base, name)
      }
    }

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

  case class SkipOneState(
    config: Config,
    parent: DoxInlineParseState,
    skipChar: Char
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def character_State(c: Char): DoxInlineParseState =
      if (c == skipChar)
        leave_none
      else
        RAISE.noReachDefect(this, s"SkipOneState#character_State($this): $c")
  }
  object SkipOneState {
    def apply(parent: DoxInlineParseState, skipChar: Char): SkipOneState =
      SkipOneState(parent.config, parent, skipChar)
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
    override protected def use_markdown_image: Boolean = false
  }

  trait InlineFeature { self: DoxInlineParseState =>
    def doxes: Vector[Dox]
    def cs: Vector[Char]

    protected def make_dox: Vector[Dox] =
      if (cs.isEmpty)
        doxes
      else
        doxes :+ _parse(cs.mkString)

    protected def make_inline_dox: Vector[Inline] =
      make_dox.map {
        case m: Inline => m
        case m => RAISE.illegalStateFault(s"No inline: $m")
      }

    protected def make_dox(c: Char): Vector[Dox] = 
      doxes :+ _parse((cs :+ c).mkString)

    private def _parse(p: String): Dox = DoxInlineParser.parse(config, p)
  }

  case class InlineState(
    config: Config,
    parent: DoxInlineParseState,
    // matchp: CharEvent => Boolean,
    closeChar1: Char,
    closeChar2: Option[Char] = None,
    doxes: Vector[Dox] = Vector.empty,
    cs: Vector[Char] = Vector.empty,
    isInSpace: Boolean = false,
    private val openinglocation: ParseLocation = ParseLocation.start
  ) extends ChildDoxInlineParseState with InlineFeature {
    override protected def use_parenthesis: Boolean = true

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
      throw new StructuredRenderingDiagnosticException(
        StructuredRenderingDiagnostic.documentSyntaxInvalid(
          sourceIdentity = config._source_identity_option.getOrElse("<inline-input>"),
          line = _source_line,
          column = _source_column,
          tokenContext = _token_context
        )
      )

    private def _source_line: Int =
      (config.location.flatMap(_.line), openinglocation.line) match {
        case (Some(origin), Some(relative)) => origin + relative - 1
        case (Some(origin), None) => origin
        case (None, Some(relative)) => relative
        case (None, None) => 1
      }

    private def _source_column: Int =
      (openinglocation.line, config.location.flatMap(_.offset), openinglocation.offset) match {
        case (Some(1), Some(origin), Some(relative)) => origin + relative - 1
        case (_, _, Some(relative)) => relative
        case (_, Some(origin), None) => origin
        case _ => 1
      }

    private def _token_context: String =
      config._token_context_option.map { input =>
        CharEvent.make(input).
          dropWhile(_.location != openinglocation).
          map(_.c).
          mkString.
          take(160)
      }.filter(_.nonEmpty).getOrElse(closeChar1.toString)

    override protected def open_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState = {
      val r = if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)
      r
    }

    override protected def open_Parenthesis_State(evt: CharEvent): DoxInlineParseState =
      character_State(evt.c)

    override protected def close_Parenthesis_State(evt: CharEvent): DoxInlineParseState =
      if (is_match(evt))
        leave_to_inline_dox
      else
        character_State(evt.c)

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
      closeChar: Char,
      openingLocation: ParseLocation
    ): InlineState = InlineState(
      parent.config,
      parent,
      closeChar,
      openinglocation = openingLocation
    )

    def apply(
      parent: DoxInlineParseState,
      closeChar1: Char,
      closeChar2: Char
    ): InlineState = InlineState(parent.config, parent, closeChar1, Some(closeChar2))

    def apply(
      parent: DoxInlineParseState,
      closeChar1: Char,
      closeChar2: Char,
      openingLocation: ParseLocation
    ): InlineState = InlineState(
      parent.config,
      parent,
      closeChar1,
      Some(closeChar2),
      openinglocation = openingLocation
    )

    def createCloseFirst(
      parent: DoxInlineParseState,
      closeChar: Char,
      firstChar: Char
    ): InlineState = InlineState(parent.config, parent, closeChar, cs = Vector(firstChar))

    def createCloseFirst(
      parent: DoxInlineParseState,
      closeChar: Char,
      firstChar: Char,
      openingLocation: ParseLocation
    ): InlineState = InlineState(
      parent.config,
      parent,
      closeChar,
      cs = Vector(firstChar),
      openinglocation = openingLocation
    )

    // def create(
    //   parent: DoxInlineParseState,
    //   closeChar1: Char,
    //   closeChar2: Char
    // ): DoxInlineParseState = SkipOneState(InlineState(parent, closeChar1, closeChar2), closeChar2)
  }

  case class RawState(
    config: Config,
    parent: DoxInlineParseState,
    closeChar1: Char,
    closeChar2: Option[Char] = None,
    cs: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    private def _is_close(evt: CharEvent) =
      evt.c == closeChar1 && closeChar2.fold(true) { c =>
        evt.next.fold(true)(_ == c)
      }

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      if (_is_close(evt))
        leave_inline_to(List(Text(cs.mkString)))
      else
        copy(cs = cs :+ evt.c)
  }
  object RawState {
    def apply(
      parent: DoxInlineParseState,
      closeChar: Char
    ): RawState = RawState(parent.config, parent, closeChar)
  }

  case class InlineMacroState(
    config: Config,
    parent: DoxInlineParseState,
    name: String,
    cs: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case ']' if _is_close(evt) => leave_inline_to(_make_inline_macro)
        case m => copy(cs = cs :+ m)
      }

    private def _is_close(evt: CharEvent): Boolean =
      name match {
        case "pass" => evt.next.forall(_.isWhitespace)
        case _ => true
      }

    private def _make_inline_macro: Inline = {
      val contents = cs.mkString
      DoxInlineParserInlineMacro.create(config, name, contents)
    }
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

    override protected def open_Bracket_State(evt: CharEvent): DoxInlineParseState =
      InlineState(OrgModeLinkUrnState(config, parent), ']', evt.location)

    override protected def close_Bracket_State(c: Char): DoxInlineParseState =
      ???

    override protected def character_State(evt: CharEvent): DoxInlineParseState = {
      val r = InlineState.createCloseFirst(MarkdownLinkUrnState(config, parent), ']', evt.c, evt.location)
      r
    }
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

    override protected def open_Bracket_State(evt: CharEvent): DoxInlineParseState =
      InlineState(OrgModeLinkLabelState(config, parent, urn), ']', ']', evt.location)

    override protected def close_Bracket_State(c: Char): DoxInlineParseState = {
      // // XXX annotation, block, figure
      // val uri = make_text(urn)
      // if (config.isImageFile(uri))
      //   leave_to(ReferenceImg(uri))
      // else
      //   leave_to(Hyperlink(urn, uri))
      leave_to_urn(urn)
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
      leave_to_urn(urn, doxes)
      // val location = None // TODO
      // val uri = make_text(urn)
      // if (config.isImageFile(uri))
      //   leave_to(ReferenceImg(uri))
      // else
      //   leave_to(Hyperlink(doxes, uri, location))
    }
  }

  case class MarkdownLinkUrnState(
    config: Config,
    parent: DoxInlineParseState
  ) extends ChildDoxInlineParseState {
    override protected def use_bracket = true

    override def returnInlineFrom(doxes: Seq[Inline]) = MarkdownLinkUrnContState(config, parent, doxes)
    // override protected def open_Bracket_State(c: Char): DoxInlineParseState =
    //   InlineState(MarkdownLinkLabelState(config, parent, urn), ']', ']')

    // override protected def close_Bracket_State(c: Char): DoxInlineParseState = {
    //   // XXX annotation, block, figure
    //   val uri = make_text(urn)
    //   if (config.isImageFile(uri))
    //     leave_to(ReferenceImg(uri))
    //   else
    //     leave_to(Hyperlink(urn, uri))
    // }
  }
  object MarkdownLinkUrnState {
    def apply(p: DoxInlineParseState): MarkdownLinkUrnState = MarkdownLinkUrnState(
      p.config,
      p
    )
  }

  case class MarkdownLinkUrnContState(
    config: Config,
    parent: DoxInlineParseState,
    urn: Seq[Inline] = Vector.empty
  ) extends ChildDoxInlineParseState {
    override protected def handle_End(): Transition =
      _deprecated_site_link(EndEvent)

    override protected def handle_char_event(evt: CharEvent): Transition =
      evt.c match {
        case '(' => to_transition(InlineState(MarkdownLinkLabelState(config, parent, urn), ')', evt.location))
        case _ =>
          _deprecated_site_link(evt)
      }

    private def _deprecated_site_link(evt: ParseEvent): Transition = {
      val label = make_text(urn)
      val text = s"[$label]"
      if (_is_structural_bracket_text(label)) {
        val next = leave_to(Text(text))
        next.apply(config, evt)
      } else if (!_is_legacy_site_link(label)) {
        val next = leave_to_urn(urn)
        next.apply(config, evt)
      } else {
        val next = leave_to_urn(urn)
        val (msgs, result, state) = next.apply(config, evt)
        val message = s"Deprecated SmartDox site link '$text'. Use 'site:[$label]' instead."
        scala.Console.err.println(s"warning: $message")
        val warn = ParseMessageSequence.warning(message)
        (warn + msgs, result, state)
      }
    }

    private def _is_structural_bracket_text(label: String): Boolean = {
      val s = label.trim
      s.length >= 2 && (
        (s.head == '"' && s.last == '"') ||
        (s.head == '\'' && s.last == '\'') ||
        (s.startsWith("&quot;") && s.endsWith("&quot;")) ||
        (s.startsWith("&#39;") && s.endsWith("&#39;"))
      )
    }

    private def _is_legacy_site_link(label: String): Boolean = {
      val s = label.trim
      s.endsWith(".dox") && !s.exists(_.isWhitespace)
    }
  }

  case class MarkdownLinkLabelState(
    config: Config,
    parent: DoxInlineParseState,
    urn: Seq[Inline]
  ) extends ChildDoxInlineParseState {
    override def returnInlineFrom(doxes: Seq[Inline]) =
      leave_to_urn(doxes, urn)
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
    parent: DoxInlineParseState,
    kind: Option[Code.Kind] = None
  ) extends ChildDoxInlineParseState {
    override def returnInlineFrom(dox: Seq[Inline]): DoxInlineParseState =
      leave_to(Code(dox.toList, kind))
  }
  object CodeState {
    def console(
      config: Config,
      parent: DoxInlineParseState
    ): CodeState = CodeState(config, parent, Some(Code.Kind.Console))
  }

  case class StrikeThroughState(
    config: Config,
    parent: DoxInlineParseState
  ) extends ChildDoxInlineParseState {
    override def returnInlineFrom(dox: Seq[Inline]): DoxInlineParseState =
      leave_to(Del(dox.toList))
  }

  type XmlState = DoxInlineXmlParser.XmlState
  val XmlState = DoxInlineXmlParser.XmlState
  type OpenTagState = DoxInlineXmlParser.OpenTagState
  val OpenTagState = DoxInlineXmlParser.OpenTagState
  type TagAttributeListState = DoxInlineXmlParser.TagAttributeListState
  val TagAttributeListState = DoxInlineXmlParser.TagAttributeListState
  type TagAttributeState = DoxInlineXmlParser.TagAttributeState
  val TagAttributeState = DoxInlineXmlParser.TagAttributeState
  type TagAttributeValueState = DoxInlineXmlParser.TagAttributeValueState
  val TagAttributeValueState = DoxInlineXmlParser.TagAttributeValueState
  type CloseTagState = DoxInlineXmlParser.CloseTagState
  val CloseTagState = DoxInlineXmlParser.CloseTagState
}
