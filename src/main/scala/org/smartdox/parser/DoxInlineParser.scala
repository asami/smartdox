package org.smartdox.parser

import java.net.URI
import org.goldenport.RAISE
import org.goldenport.context.Consequence
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
 *  version Jun. 10, 2025
 *  version Jul. 29, 2025
 *  version Sep.  9, 2025
 *  version Oct. 26, 2025
 *  version Nov.  5, 2025
 *  version Apr. 20, 2026
 *  version Jun. 29, 2026
 * @version Aug. 19, 2026
 * @author  ASAMI, Tomoharu
 */
object DoxInlineParser {
  type Transition = (ParseMessageSequence, ParseResult[Dox], DoxInlineParseState)

  // def parse(p: LogicalParagraph): Dox = parse(p.lines)

  // def parse(p: LogicalLines): Dox = toDox(p.lines.map(parse))

  def parse(p: LogicalLine): Dox = parse(Config.default.withLocation(p.location), p.text)

  def parse(in: String): Dox = parse(Config.default, in)

  def parse(config: Config, in: String): Dox =
    _parse_inline_macro(config, in).getOrElse {
      // println(s"Inline($config): $in")
      val (messages, result, state) = apply(config, in)
      result match {
        case ParseSuccess(dox, _) => Dox.toDox(dox)
        case ParseFailure(_, _) => RAISE.notImplementedYetDefect
        case EmptyParseResult() => RAISE.notImplementedYetDefect
      }
    }

  private val _inline_macro_regex = """^([A-Za-z][A-Za-z0-9_-]*):\[(.*)\]$""".r

  private def _parse_inline_macro(config: Config, in: String): Option[Dox] =
    if (config.asciidoc.isInlineMacro)
      in match {
        case _inline_macro_regex("site", contents) =>
          Some(Dox.attachLocation(Hyperlink(Vector(Text(contents)), contents), config.location))
        case _inline_macro_regex(name, contents) =>
          Some(Dox.attachLocation(InlineMacro(name, contents), config.location))
        case _ => None
      }
    else
      None

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
    orgmode: Config.OrgMode = Config.OrgMode.none,
    asciidoc: Config.Asciidoc = Config.Asciidoc.none,
    location: Option[ParseLocation] = None
  ) extends ParseConfig {
    def withLocation(p: Option[ParseLocation]): Config = copy(location = p)
    def withLocation(p: ParseLocation): Config = copy(location = p.toOption)

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

  trait DoxInlineParseState extends ParseReaderWriterState[Config, Dox] with DoxInlineParseStateSupport {
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
      if (cs.isEmpty)
        ParseSuccess(Dox.toDox(doxes))
      else
        ParseSuccess(Dox.toDox(doxes :+ Text(cs.mkString)))

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
      val (prefix, name) = _split_inline_macro_name(cs)
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
      val (prefix, name) = _split_inline_macro_name(cs.dropRight(1))
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

    private def _split_inline_macro_name(p: Vector[Char]): (Option[String], String) = {
      val s = p.mkString
      val i = s.lastIndexWhere(ch => !Character.isLetterOrDigit(ch) && ch != '_' && ch != '-')
      val name = s.drop(i + 1)
      val prefix = if (i < 0) None else Some(s.take(i + 1))
      if (_is_inline_macro_name(name))
        prefix -> name
      else
        None -> ""
    }

    private def _is_inline_macro_name(p: String): Boolean =
      p.nonEmpty && p.head.isLetter && p.forall(ch => Character.isLetterOrDigit(ch) || ch == '_' || ch == '-')

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
    isInSpace: Boolean = false
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
      RAISE.notImplementedYetDefect(s"end_Result(${getClass.getSimpleName}): $this")

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
      closeChar1: Char,
      closeChar2: Char
    ): InlineState = InlineState(parent.config, parent, closeChar1, Some(closeChar2))

    def createCloseFirst(
      parent: DoxInlineParseState,
      closeChar: Char,
      firstchar: Char
    ): InlineState = InlineState(parent.config, parent, closeChar, cs = Vector(firstchar))

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
      name match {
        case "site" => Dox.attachLocation(Hyperlink(Vector(Text(contents)), contents), config.location).asInstanceOf[Inline]
        case _ => Dox.attachLocation(InlineMacro(name, contents), config.location).asInstanceOf[Inline]
      }
    }
  }

  case class XmlState(
    config: Config,
    parent: DoxInlineParseState,
    tagName: String,
    attrs: Vector[(String, String)],
    cs: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    private val _ctx = Dox2Parser.ParseContext.now() // TODO
    implicit def dtctx = _ctx.dateTimeContext

    override def returnFrom(doxes: Seq[Dox]): DoxInlineParseState = {
      // Integrate parsed child Dox elements into this tag and return to the parent
      val element = _create_dox(doxes)
      leave_to(element)
    }

    override def returnEndResult: ParseResult[Dox] = {
      // Handle unclosed tag by finalizing current content
      val xs = parseInlineContents(cs.mkString)
      val dox = _create_dox(xs)
      ParseSuccess(dox)
    }

    override def returnCharsFrom(p: Seq[Char]): DoxInlineParseState = {
      val s = (cs ++ p).mkString
      val trimmed = s.trim
      if (trimmed.startsWith("</")) {
        val tag = trimmed.drop(2).takeWhile(_ != '>').trim
        if (tag == tagName) {
          // Matched closing tag: finalize current element and return to parent
          val xs = parseInlineContents(cs.mkString)
          val dox = _create_dox(xs)
          leave_to(dox)
        } else {
          // Not this tag: propagate to higher parent
          parent.returnCharsFrom(p)
        }
      } else {
        // Continue accumulating inner content
        copy(cs = cs ++ p)
      }
    }

    private def _return_chars_from_block(p: Seq[Char]): DoxInlineParseState = {
      val s = (cs ++ p).mkString
      val c = Dox2Parser.Config.smartdox.withInlineConfig(config).withoutComplementParagraph()
      val xs = Dox2Parser.parseFragment(c, s)
      val dox = _create_dox(xs.contents)
      leave_to(dox)
    }

    private def _return_chars_from_inline(p: Seq[Char]): DoxInlineParseState = {
      val s = (cs ++ p).mkString
      val dox = _create_dox(parseInlineContents(s))
      leave_to(dox)
    }

    private[parser] def _create_dox(doxes: Seq[Dox]): Dox =
      Dox.attachLocation(Dox.create(tagName, attrs, doxes), config.location)

    def parseInlineContents(s: String): List[Dox] =
      _normalize(DoxInlineParser.parse(config, s))

    private def _normalize(p: Dox): List[Dox] = p match {
      case m: Paragraph => m.contents // TODO
      case m: Fragment => m.contents
      case m: Div => m.contents
      case m => List(m)
    }

    override protected def use_angle_bracket = true

    override protected def end_Result(): ParseResult[Dox] =
      leave_end

    override protected def character_State(c: Char) = copy(cs = cs :+ c)

    override protected def open_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState = {
      if (XmlState._is_in_escape(cs))
        character_State(evt)
      else
        evt.next match {
          case Some('/') =>
            // closing tag of this element
            SkipOneState(XmlState.XmlCloseState(config, this), '/')
          case _ =>
            // open nested tag: create nested XmlState via TagOpenState
            XmlState.TagOpenState(config, this)
        }
    }

    override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
      if (XmlState._is_in_escape(cs))
        character_State(evt)
      else
        super.close_Angle_Bracket_State(evt)
  }
  object XmlState {
    case class XmlCloseState(
      config: Config,
      parent: XmlState,
      cs: Vector[Char] = Vector('<', '/')
    ) extends ChildDoxInlineParseState with RawFeature {
      override protected def use_angle_bracket = true
      implicit val dtctx = parent.dtctx

      override protected def character_State(c: Char) = copy(cs = cs :+ c)

      override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState = {
        val tag = cs.drop(2).mkString.trim.stripSuffix(">")
        if (tag == parent.tagName) {
          // Proper closing tag: finalize content and return with child elements preserved
          val xs = parent.parseInlineContents(parent.cs.mkString)
          val dox = parent._create_dox(xs)
          parent.parent match {
            case gp: XmlState =>
              // Append the rendered markup (with attributes preserved) to the outer XmlState buffer
              val inner = {
                val buf = new StringBuilder
                dox.printDox(buf)
                buf.toString
              }
              gp.copy(cs = gp.cs ++ inner.toVector)
            case _ =>
              parent.parent.returnFrom(Seq(dox))
          }
        } else if (parent.isInstanceOf[XmlState] && parent.asInstanceOf[XmlState].tagName != tag) {
          parent.parent match {
            case gp: XmlState =>
              gp.returnCharsFrom(s"</$tag>".toVector)
            case _ =>
              parent.character_State('<')
          }
        } else {
          parent.character_State('<')
        }
      }
    }

    case class TagOpenState(
      config: Config,
      parent: DoxInlineParseState,
      cs: Vector[Char] = Vector('<')
    ) extends ChildDoxInlineParseState with RawFeature {
      override protected def use_angle_bracket = true

      override def returnCharsFrom(p: Seq[Char]): DoxInlineParseState =
        parent.returnCharsFrom(cs ++ p)

      override protected def character_State(c: Char) = copy(cs = cs :+ c)

      override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState = {
        // When finishing opening tag, construct a new nested XmlState
        val raw = cs.dropWhile(_ == '<').mkString
        val (tag, attrs) = XmlState._parse_tag_definition(raw)
        XmlState(config, parent = parent, tagName = tag, attrs = attrs)
      }
    }

    case class ContentState(
      config: Config,
      parent: DoxInlineParseState,
      cs: Vector[Char] = Vector.empty
    ) extends ChildDoxInlineParseState with RawFeature {
      override def returnCharsFrom(p: Seq[Char]): DoxInlineParseState =
        parent.returnCharsFrom(cs ++ p)

      override protected def use_angle_bracket = true

      override protected def character_State(c: Char) = copy(cs = cs :+ c)

      override protected def open_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
        evt.next match {
          case Some(s) if s == '/' => SkipOneState(TagCloseState(config, this), s)
          case _ => TagOpenState(config, this)
        }

      override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
        if (_is_in_escape(cs))
          character_State(evt)
        else
          super.close_Angle_Bracket_State(evt)
    }

    private def _is_in_escape(cs: Vector[Char]): Boolean = {
      @annotation.tailrec
      def loop(xs: List[Char], tripleCount: Int, singleCount: Int): Boolean = xs match {
        case '`' :: '`' :: '`' :: rest =>
          // Found a triple backtick: toggle code block mode
          loop(rest, tripleCount + 1, singleCount)
        case '`' :: rest if tripleCount % 2 == 0 =>
          // Found a single backtick outside a code block
          loop(rest, tripleCount, singleCount + 1)
        case _ :: rest =>
          // Other characters: continue scanning
          loop(rest, tripleCount, singleCount)
        case Nil =>
          // Escaping if inside a code block or inline code
          (tripleCount % 2 == 1) || (singleCount % 2 == 1)
      }

      loop(cs.toList, 0, 0)
    }

    case class TagCloseState(
      config: Config,
      parent: DoxInlineParseState,
      cs: Vector[Char] = Vector('<', '/')
    ) extends ChildDoxInlineParseState with RawFeature {
      override protected def use_angle_bracket = true

      override protected def character_State(c: Char) = copy(cs = cs :+ c)

      override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
        leave_to_chars(cs :+ '>')
    }

    private def _parse_tag_definition(raw: String): (String, Vector[(String, String)]) = {
      val trimmed = raw.trim.stripSuffix(">")
      val normalized = {
        val r = trimmed.reverse.dropWhile(_.isWhitespace)
        val s = if (r.startsWith("/")) r.drop(1) else r
        s.reverse.trim
      }
      val len = normalized.length
      @annotation.tailrec
      def skipSpace(idx: Int): Int =
        if (idx < len && normalized.charAt(idx).isWhitespace)
          skipSpace(idx + 1)
        else
          idx

      val start = skipSpace(0)
      val nameEnd = {
        @annotation.tailrec
        def loop(i: Int): Int =
          if (i < len && !normalized.charAt(i).isWhitespace) loop(i + 1)
          else i
        loop(start)
      }
      val tagName =
        if (start < nameEnd) normalized.substring(start, nameEnd) else ""

      val attrs = Vector.newBuilder[(String, String)]

      @annotation.tailrec
      def parseAttr(idx: Int): Unit = {
        val i = skipSpace(idx)
        if (i >= len)
          ()
        else {
          val nameStart = i
          @annotation.tailrec
          def readName(j: Int): Int =
            if (j < len) {
              val ch = normalized.charAt(j)
              if (!ch.isWhitespace && ch != '=') readName(j + 1)
              else j
            } else j

          val nameEnd = readName(nameStart)
          val attrName =
            if (nameStart < nameEnd) normalized.substring(nameStart, nameEnd) else ""
          val afterName = skipSpace(nameEnd)
          if (attrName.nonEmpty) {
            if (afterName < len && normalized.charAt(afterName) == '=') {
              val valueStart = skipSpace(afterName + 1)
              if (valueStart < len && (normalized.charAt(valueStart) == '"' || normalized.charAt(valueStart) == '\'')) {
                val quote = normalized.charAt(valueStart)
                val valueBodyStart = valueStart + 1
                @annotation.tailrec
                def readQuoted(j: Int): Int =
                  if (j < len && normalized.charAt(j) != quote) readQuoted(j + 1) else j
                val valueEnd = readQuoted(valueBodyStart)
                val value = normalized.substring(valueBodyStart, valueEnd)
                attrs += attrName -> value
                val next = if (valueEnd < len) valueEnd + 1 else valueEnd
                parseAttr(next)
              } else {
                @annotation.tailrec
                def readUnquoted(j: Int): Int =
                  if (j < len && !normalized.charAt(j).isWhitespace) readUnquoted(j + 1) else j
                val valueEnd = readUnquoted(valueStart)
                val value = normalized.substring(valueStart, valueEnd)
                attrs += attrName -> value
                parseAttr(valueEnd)
              }
            } else {
              // boolean attribute
              attrs += attrName -> attrName
              parseAttr(afterName)
            }
          } else {
            parseAttr(afterName + 1)
          }
        }
      }

      parseAttr(nameEnd)
      (tagName, attrs.result())
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

    override protected def character_State(c: Char): DoxInlineParseState = {
      val r = InlineState.createCloseFirst(MarkdownLinkUrnState(config, parent), ']', c)
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

    override protected def open_Bracket_State(c: Char): DoxInlineParseState =
      InlineState(OrgModeLinkLabelState(config, parent, urn), ']', ']')

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
        case '(' => to_transition(InlineState(MarkdownLinkLabelState(config, parent, urn), ')'))
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

  case class OpenTagState(
    config: Config,
    parent: DoxInlineParseState,
    name: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    def resultOpenClose(ps: Vector[(String, String)]) =
      if (true)
        XmlState(config, parent, name.mkString, ps)
      else
        InlineState(CloseTagState(config, parent, name.mkString, ps), '<', '/')

    def resultOpenEnd(ps: Vector[(String, String)]) = ???

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '>' =>
          if (true)
            XmlState(config, parent, name.mkString, Vector.empty)
          else
            InlineState(CloseTagState(config, parent, name.mkString, Vector.empty), '<', '/')
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

    override def returnFrom(c: Char): DoxInlineParseState = copy(key = key :+ c)

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
          leave_to(Dox.attachLocation(Dox.create(name, attrs, dox), config.location))
        case ' ' => this
        case m => copy(closeName = closeName :+ m)
      }
  }
}
