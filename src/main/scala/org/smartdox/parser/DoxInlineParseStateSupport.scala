package org.smartdox.parser

import org.goldenport.RAISE
import org.goldenport.parser._
import org.smartdox.Dox

/*
 * @since   Aug. 19, 2026
 * @version Aug. 19, 2026
 * @author  ASAMI, Tomoharu
 */

private[parser] trait DoxInlineParseStateSupport { self: DoxInlineParser.DoxInlineParseState =>
  import DoxInlineParser._

  type Transition = DoxInlineParser.Transition

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

protected def start_State(): DoxInlineParser.DoxInlineParseState = this

protected final def handle_end(): Transition =
  handle_End()

protected def handle_End(): Transition =
  (ParseMessageSequence.empty, end_Result(), end_State())

protected def end_Result(): ParseResult[Dox] =
  RAISE.notImplementedYetDefect(s"end_Result(${getClass.getSimpleName})")

protected def end_State(): DoxInlineParser.DoxInlineParseState = EndState(config)

protected final def handle_line_end(evt: LineEndEvent): Transition =
  handle_Line_End(evt)

protected def handle_Line_End(evt: LineEndEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, line_End_State(evt))

protected def line_End_State(evt: LineEndEvent): DoxInlineParser.DoxInlineParseState = RAISE.notImplementedYetDefect

protected final def handle_space(evt: CharEvent): Transition =
  handle_Space(evt)

protected def handle_Space(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, space_State(evt))

protected def space_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  space_State(evt.c)

protected def space_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RAISE.notImplementedYetDefect(s"spaceState(${getClass.getSimpleName}): $c")

protected final def handle_open_parenthesis(evt: CharEvent): Transition =
  handle_Open_Parenthesis(evt)

protected def handle_Open_Parenthesis(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, open_Parenthesis_State(evt))

protected def open_Parenthesis_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  open_Parenthesis_State(evt.c)

protected def open_Parenthesis_State(c: Char): DoxInlineParser.DoxInlineParseState =
  LinkState(config, this)

protected final def handle_close_parenthesis(evt: CharEvent): Transition =
  handle_Close_Parenthesis(evt)

protected def handle_Close_Parenthesis(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, close_Parenthesis_State(evt))

protected def close_Parenthesis_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  close_Parenthesis_State(evt.c)

protected def close_Parenthesis_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RAISE.notImplementedYetDefect

protected final def handle_open_angle_bracket(evt: CharEvent): Transition =
  handle_Open_Angle_Bracket(evt)

protected def handle_Open_Angle_Bracket(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, open_Angle_Bracket_State(evt))

protected def open_Angle_Bracket_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  open_Angle_Bracket_State(evt.c)

protected def open_Angle_Bracket_State(c: Char): DoxInlineParser.DoxInlineParseState =
  OpenTagState(config, this)

protected final def handle_close_angle_bracket(evt: CharEvent): Transition = {
  // println(s"${getClass.getSimpleName}#handle_close_angle_bracket: $evt")
  handle_Close_Angle_Bracket(evt)
}

protected def handle_Close_Angle_Bracket(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, close_Angle_Bracket_State(evt))

protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  close_Angle_Bracket_State(evt.c)

protected def close_Angle_Bracket_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RAISE.notImplementedYetDefect(s"close_Angle_Bracket_State(${getClass.getSimpleName}): $c")

protected final def handle_open_bracket(evt: CharEvent): Transition =
  handle_Open_Bracket(evt)

protected def handle_Open_Bracket(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, open_Bracket_State(evt))

protected def open_Bracket_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  open_Bracket_State(evt.c)

protected def open_Bracket_State(c: Char): DoxInlineParser.DoxInlineParseState =
  LinkState(config, this)

protected final def handle_close_bracket(evt: CharEvent): Transition =
  handle_Close_Bracket(evt)

protected def handle_Close_Bracket(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, close_Bracket_State(evt))

protected def close_Bracket_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  close_Bracket_State(evt.c)

protected def close_Bracket_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RAISE.notImplementedYetDefect

protected final def handle_open_brace(evt: CharEvent): Transition =
  handle_Open_Brace(evt)

protected def handle_Open_Brace(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, open_Brace_State(evt))

protected def open_Brace_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  open_Brace_State(evt.c)

protected def open_Brace_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RAISE.notImplementedYetDefect

protected final def handle_close_brace(evt: CharEvent): Transition =
  handle_Close_Brace(evt)

protected def handle_Close_Brace(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, close_Brace_State(evt))

protected def close_Brace_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  close_Brace_State(evt.c)

protected def close_Brace_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RAISE.notImplementedYetDefect

protected final def handle_asterisc(evt: CharEvent): Transition =
  handle_Asterisc(evt)

protected def handle_Asterisc(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, asterisc_State(evt))

protected def asterisc_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  asterisc_State(evt.c)

protected def asterisc_State(c: Char): DoxInlineParser.DoxInlineParseState =
  InlineState(BoldState(config, this), '*')

protected final def handle_underscore(evt: CharEvent): Transition =
  handle_Underscore(evt)

protected def handle_Underscore(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, underscore_State(evt))

protected def underscore_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  underscore_State(evt.c)

protected def underscore_State(c: Char): DoxInlineParser.DoxInlineParseState =
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

protected def backquote_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  backquote_State(evt.c)

protected def backquote_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RawState(CodeState(config, this), '`')

protected final def handle_tilde(evt: CharEvent): Transition =
  handle_Tilde(evt)

protected def handle_Tilde(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, tilde_State(evt))

protected def tilde_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  tilde_State(evt.c)

protected def tilde_State(c: Char): DoxInlineParser.DoxInlineParseState =
  InlineState(CodeState.console(config, this), '~')

protected final def handle_colon(evt: CharEvent): Transition =
  handle_Colon(evt)

protected def handle_Colon(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, colon_State(evt))

protected def colon_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  colon_State(evt.c)

protected def colon_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RAISE.notImplementedYetDefect

protected final def handle_equal(evt: CharEvent): Transition =
  handle_Equal(evt)

protected def handle_Equal(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, equal_State(evt))

protected def equal_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  equal_State(evt.c)

protected def equal_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RawState(CodeState(config, this), '=')

protected final def handle_plus(evt: CharEvent): Transition =
  handle_Plus(evt)

protected def handle_Plus(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, plus_State(evt))

protected def plus_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  plus_State(evt.c)

protected def plus_State(c: Char): DoxInlineParser.DoxInlineParseState =
  InlineState(StrikeThroughState(config, this), '+')

protected final def handle_slash(evt: CharEvent): Transition =
  handle_Slash(evt)

protected def handle_Slash(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, slash_State(evt))

protected def slash_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  slash_State(evt.c)

protected def slash_State(c: Char): DoxInlineParser.DoxInlineParseState =
  InlineState(ItalicState(config, this), '/')

protected final def handle_dallor(evt: CharEvent): Transition =
  handle_Dallor(evt)

protected def handle_Dallor(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, dallor_State(evt))

protected def dallor_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  dallor_State(evt.c)

protected def dallor_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RAISE.notImplementedYetDefect

protected final def handle_character(evt: CharEvent): Transition =
  handle_Character(evt)

protected def handle_Character(evt: CharEvent): Transition =
  (ParseMessageSequence.empty, ParseResult.empty, character_State(evt))

protected def character_State(evt: CharEvent): DoxInlineParser.DoxInlineParseState =
  character_State(evt.c)

protected def character_State(c: Char): DoxInlineParser.DoxInlineParseState =
  RAISE.notImplementedYetDefect(this, s"character_State: $c")

//
protected final def make_text(ps: Seq[Dox]): String = ps.map(_.toPlainText).mkString

protected final def to_transition(p: DoxInlineParser.DoxInlineParseState) =
  (ParseMessageSequence.empty, ParseResult.empty, p)

}
