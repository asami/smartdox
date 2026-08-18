package org.smartdox.parser

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.collection.NonEmptyVector
import org.goldenport.parser._
import org.smartdox._

/*
 * @since   Aug. 19, 2026
 * @version Aug. 19, 2026
 * @author  ASAMI, Tomoharu
 */

private[parser] trait DoxLinesParseStateSupport { self: DoxLinesParser.DoxLinesParseState =>
  import DoxLinesParser._

  type Transition = DoxLinesParser.Transition

protected val use_empty: Boolean = true

def result = RAISE.noReachDefect(this, "result")
// def result: Dox
def apply(config: Config, evt: ParseEvent): Transition = {
//      println(s"in($this): $evt")
  val r = handle_event(config, evt)
//      println(s"out($this): $r")
  r
}

def returnFrom(doxes: Seq[Dox]): DoxLinesParser.DoxLinesParseState = RAISE.noReachDefect(this, "returnFrom")

def returnFrom(dox: Dox): DoxLinesParser.DoxLinesParseState = returnFrom(Vector(dox))

def returnFrom(as: NonEmptyVector[AnnotationMark]): DoxLinesParser.DoxLinesParseState = RAISE.noReachDefect(this, s"returnFrom: $as")

protected def handle_event(config: Config, evt: ParseEvent): Transition =
  evt match {
    case StartEvent => start_transition(config)
    case EndEvent => end_transition(config)
//        case m: LogicalLineEvent if use_empty && m.line.isEmpty => empty_transition(config, m.line)
    case m: LogicalLineEvent => handle_line(config, m)
    case m => RAISE.noReachDefect(this, "handle_event")
  }

protected def handle_line(config: Config, evt: LogicalLineEvent): Transition = {
  val line = evt.line
  if (use_empty && line.isEmptyLine)
    empty_transition(config, evt)
  else
    get_list_transition(config, evt) orElse
  get_table_transition(config, evt) orElse
  get_image_transition(config, evt) orElse
  get_annotation_transition(config, evt) orElse
  get_block_macro_transition(config, evt) orElse
  get_horizontal_rule_transition(config, evt) orElse
  get_quotation_transition(config, evt) getOrElse
  text_transition(config, evt)
}

protected def start_transition(config: Config): Transition = transit_none

protected def end_transition(config: Config): Transition = {
  end_Transition(config)
}

protected def end_Transition(config: Config): Transition =
  RAISE.noReachDefect(this, "end_Transition")

protected def empty_transition(config: Config, evt: LogicalLineEvent): Transition = {
  empty_Transition(config, evt.line)
}

protected def empty_Transition(config: Config, evt: LogicalLine): Transition =
  RAISE.noReachDefect(this, "empty_Transition")

protected def get_list_transition(config: Config, evt: LogicalLineEvent): Option[Transition] = {
  get_List_Transition(config, evt.line)
}

protected def get_List_Transition(config: Config, evt: LogicalLine): Option[Transition] =
  RAISE.noReachDefect(this, "get_List_Transition")

protected def get_table_transition(config: Config, evt: LogicalLineEvent): Option[Transition] = {
  get_Table_Transition(config, evt.line)
}

protected def get_Table_Transition(config: Config, evt: LogicalLine): Option[Transition] =
  RAISE.noReachDefect(this, "get_Table_Transition")

protected def get_image_transition(config: Config, evt: LogicalLineEvent): Option[Transition] =
  get_Image_Transition(config, evt.line)

protected def get_Image_Transition(config: Config, line: LogicalLine): Option[Transition] =
  RAISE.noReachDefect(this, "get_Image_Transition")

protected def get_annotation_transition(config: Config, evt: LogicalLineEvent): Option[Transition] = {
  get_Annotation_Transition(config, evt.line)
}

protected def get_Annotation_Transition(config: Config, evt: LogicalLine): Option[Transition] =
  RAISE.noReachDefect(this, "get_Annotation_Transition")

protected def get_block_macro_transition(config: Config, evt: LogicalLineEvent): Option[Transition] = {
  get_Block_Macro_Transition(config, evt.line)
}

protected def get_Block_Macro_Transition(config: Config, evt: LogicalLine): Option[Transition] =
  RAISE.noReachDefect(this, "get_Block_Macro_Transition")

protected def get_horizontal_rule_transition(config: Config, evt: LogicalLineEvent): Option[Transition] = {
  get_Horizontal_Rule_Transition(config, evt.line)
}

protected def get_Horizontal_Rule_Transition(config: Config, evt: LogicalLine): Option[Transition] =
  RAISE.noReachDefect(this, "get_Horizontal_Rule_Transition")

protected def get_quotation_transition(config: Config, evt: LogicalLineEvent): Option[Transition] = {
  get_Quotation_Transition(config, evt.line)
}

protected def get_Quotation_Transition(config: Config, evt: LogicalLine): Option[Transition] =
  RAISE.noReachDefect(this, "get_Quotation_Transition")

protected def text_transition(config: Config, evt: LogicalLineEvent): Transition = {
  text_Transition(config, evt.line)
}

protected def text_Transition(config: Config, evt: LogicalLine): Transition =
  RAISE.noReachDefect(this, "text_Transition")

// protected final def parse_inline(p: String): (ParseMessageSequence, Option[Dox]) = {
//   val (msgs, result, _) = DoxInlineParser.apply(p)
//   result match {
//     case EmptyParseResult() => (msgs, None)
//     case ParseSuccess(ast, ws) => (msgs :++ ws, Some(Dox.toDox(ast)))
//     case ParseFailure(es, ws) => (msgs :++ es :++ ws, None)
//   }
// }

}

private[parser] trait ChildDoxLinesParseStateSupport { self: DoxLinesParser.ChildDoxLinesParseState =>
  import DoxLinesParser._


protected def leave_to(p: (ParseMessageSequence, Seq[Tree[Dox]])): DoxLinesParser.Transition = {
  val (msgs, doxtrees) = p
  leave_to(msgs, doxtrees)
}

private def _show(ps: Seq[Tree[Dox]]): String =
  ps.map(_.drawTree).mkString("\n")

private def _show(p: Tree[Dox]): String = p.drawTree

protected def leave_to(dox: Dox): DoxLinesParser.Transition =
  _leave_to(ParseMessageSequence.empty, dox)

protected def leave_to(msgs: ParseMessageSequence, doxtrees: Seq[Tree[Dox]]): DoxLinesParser.Transition = {
  val doxes = doxtrees.flatMap(Dox.untreeO)
  // println(s"${getClass.getSimpleName}#leave_to: ${_show(doxtrees)} => $doxes")
  _leave_to(msgs, doxes)
}

private def _leave_to(msgs: ParseMessageSequence, doxes: Seq[Dox]): DoxLinesParser.Transition = {
  (msgs, ParseResult.empty, parent.returnFrom(doxes))
}

protected def leave_to(msgs: ParseMessageSequence, doxtree: Tree[Dox]): DoxLinesParser.Transition = {
  // println(s"${getClass.getSimpleName}#leave_to: ${_show(doxtree)} => ...")
  val dox = Dox.untreeE(doxtree)
  // println(s"${getClass.getSimpleName}#leave_to: ${_show(doxtree)} => $dox")
  _leave_to(msgs, dox)
}

private def _leave_to(msgs: ParseMessageSequence, dox: Dox): DoxLinesParser.Transition = {
  (msgs, ParseResult.empty, parent.returnFrom(dox))
}

protected def leave_to(annotaions: NonEmptyVector[AnnotationMark]): DoxLinesParser.Transition =
  (ParseMessageSequence.empty, ParseResult.empty, parent.returnFrom(annotaions))

protected def leave_to_in_end(config: Config, p: (ParseMessageSequence, Tree[Dox])): DoxLinesParser.Transition = {
  val (msgs, doxtree) = p
  leave_to_in_end(config, msgs, doxtree)
}

protected def leave_to_in_end(config: Config, msgs: ParseMessageSequence, doxes: Tree[Dox]): DoxLinesParser.Transition = {
  val (ms, r, s) = leave_to(msgs, doxes)
  val (ms2, r2, s2) = s.apply(config, EndEvent)
  val dox = Dox.toDox(r.get.toVector ++ r2.get.toVector)
  (ms + ms2, ParseSuccess(dox), s2)
}

protected def leave_to_in_end(config: Config, annotaions: NonEmptyVector[AnnotationMark]): DoxLinesParser.Transition = {
  val (ms, r, s) = leave_to(annotaions)
  val (ms2, r2, s2) = s.apply(config, EndEvent)
  val dox = Dox.toDox(r.get.toVector ++ r2.get.toVector)
  (ms + ms2, ParseSuccess(dox), s2)
}

protected def leave_none: DoxLinesParser.Transition = transit_next(parent)

protected def leave_to(config: Config, p: ParseEvent): DoxLinesParser.Transition =
  parent.apply(config, p)

protected def leave_to(config: Config, dox: Dox, evt: ParseEvent): DoxLinesParser.Transition =
  parent.returnFrom(dox).apply(config, evt)

protected def leave_to_with_warning(config: Config, p: ParseEvent, warn: String): DoxLinesParser.Transition =
  RAISE.notImplementedYetDefect(this, s"$p: $warn")

protected def leave_end(config: Config): DoxLinesParser.Transition =
  parent.apply(config, EndEvent)

// protected def return_From(doxes: Seq[Dox]): DoxLinesParser.DoxLinesParseState = RAISE.noReachDefect(this, "return_From")

// protected def leave_to(dox: Seq[Tree[Dox]]): DoxLinesParser.Transition = ???

// protected def leave_to(dox: Dox): DoxLinesParser.Transition = return_From(dox)

// protected def return_From(dox: Dox): DoxLinesParser.Transition = RAISE.noReachDefect
}

private[parser] trait DoxLinesNormalStateSupport { self: DoxLinesParser.NormalState =>
  import DoxLinesParser._


override protected def text_transition(config: Config, evt: LogicalLineEvent): DoxLinesParser.Transition =
  _text_transition_inline(config, evt)

private def _text_transition_inline(config: Config, evt: LogicalLineEvent): DoxLinesParser.Transition = {
  val (msgs, result, _) =
    DoxInlineParser.apply(config.inlineConfig.withLocation(evt.line.location), evt.line.text)
  result match {
    case EmptyParseResult() => (msgs, ParseResult.empty, this)
    case ParseSuccess(ast, ws) =>
      val contents = _normalize(ast)
      val p = contents match {
        case Nil => Fragment.empty
        case x :: Nil =>
          if (config.isComplementParagraph && _is_inline(x))
            Paragraph(List(x), evt.line)
          else
            x
        case xs =>
          if (config.isComplementParagraph && _is_inline(xs))
            Paragraph(xs, evt.line)
          else
            Fragment(xs)
      }
      (msgs :++ ws, ParseResult.empty, copy(lines = lines :+ p))
    case ParseFailure(es, ws) => (msgs :++ es :++ ws, ParseResult.empty, this)
  }
}

private def _is_inline(p: Dox): Boolean = p.isInstanceOf[Inline]

private def _is_inline(ps: List[Dox]): Boolean = if (ps.isEmpty) false else ps.forall(_is_inline)

private def _text_transition_lines(config: Config, evt: LogicalLineEvent): DoxLinesParser.Transition = {
  val (msgs, result, _) = DoxLinesParser.apply(config, LogicalLines(evt.line))
  result match {
    case EmptyParseResult() => (msgs, ParseResult.empty, this)
    case ParseSuccess(ast, ws) =>
      val contents = _normalize(List(ast))
      val p = contents match {
        case Nil => Fragment.empty
        case x :: Nil =>
          if (_is_inline(x))
            Paragraph(List(x), evt.line)
          else
            x
        case xs if _is_inline(xs) => Paragraph(xs, evt.line)
        case xs => Paragraph(xs, evt.line)
      }
      (msgs :++ ws, ParseResult.empty, copy(lines = lines :+ p))
    case ParseFailure(es, ws) => (msgs :++ es :++ ws, ParseResult.empty, this)
  }
}

private def _normalize(p: Seq[Dox]): List[Dox] = p.flatMap {
  case m: Fragment => _normalize(m.contents)
  case m => List(m)
}.toList

private def _to_paragraph(ps: Seq[Dox]): Paragraph = Paragraph(ps.toList)

}
