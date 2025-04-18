package org.smartdox.parser

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.parser._
import org.goldenport.parser.LogicalBlock.{VerbatimMarkClass, VerbatimMark}
import org.goldenport.collection.{NonEmptyVector, VectorMap}
import org.goldenport.util.VectorUtils
import org.smartdox._
import org.smartdox.util.DoxUtils

/*
 * @since   Nov. 12, 2018
 *  version Dec. 31, 2018
 *  version Jan. 26, 2019
 *  version Oct.  2, 2019
 *  version Nov. 16, 2019
 *  version Jan. 11, 2021
 *  version Feb.  9, 2021
 *  version Jun.  6, 2024
 *  version Sep.  5, 2024
 *  version Oct. 31, 2024
 *  version Nov. 23, 2024
 *  version Jan.  1, 2025
 *  version Mar.  8, 2025
 * @version Apr.  6, 2025
 * @author  ASAMI, Tomoharu
 */
object DoxLinesParser {
  type Transition = (ParseMessageSequence, ParseResult[Dox], DoxLinesParseState)

  // def parse(p: LogicalParagraph): Dox = parse(p.lines)

  def parse(config: Config, p: LogicalParagraph): Dox = parse(config, p.lines)

  // def parse(p: LogicalLines): Dox = parse(Config.default, p)

  def parse(config: Config, p: LogicalLines): Dox = {
    // println(s"DoxLinesParser#parse(${config}): $p")
    // DoxInlineParser.toDox(p.lines.map(parse))
    val parser = LogicalLineReaderWriterStateClass(config, NormalState.init)
    val (messages, result, state) = parser.apply(p)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() =>
        // println(s"DoxLinesParser#parse[EmptyResult]: $p")
        RAISE.notImplementedYetDefect
    }
  }

  case class Config(
    isDebug: Boolean = false,
    isLocation: Boolean = true,
    inlineConfig: DoxInlineParser.Config = DoxInlineParser.Config.default
  ) extends ParseConfig {
  }
  object Config {
    val default = Config()
    val debug = Config(true)
    val orgmode = default.copy(
      inlineConfig = DoxInlineParser.Config.orgmode
    )
    val markdown = default.copy(
      inlineConfig = DoxInlineParser.Config.markdown
    )
    val literateModel = default.copy(
      inlineConfig = DoxInlineParser.Config.literateModel
    )
  }

  case class ListMark(mark: String, level: Int) {
  }
  object ListMark {
    case class Candidate(
      mark: String,
      listElement: Dox,
      rowOffset: Int,
      text: String,
      lines: LogicalLines = LogicalLines.empty,
      term: Option[String] = None
    ) {
      def toDox: Dox = Li(text) // ListStateOld

      def add(p: LogicalLine): Candidate = copy(text = text + " " + p.text)
    }
    case class Ctx(rowOffset: Int = 0) {
      def space = Ctx(rowOffset + 1)
    }

    def getCandidate(p: String): Option[Candidate] =
      if (p.isEmpty)
        None
      else
        _get(p, Ctx())

    @annotation.tailrec
    private def _get(p: String, ctx: Ctx): Option[Candidate] = {
      val t = p.tail
      p.head match {
        case ' ' =>
          if (t.isEmpty)
            None
          else
            _get(t, ctx.space)
        case '-' =>
          t.headOption.flatMap { x =>
            if (x == ' ') {
              val c = t.tail.dropWhile(_ == ' ')
              _get_definition_list(c, ctx) orElse {
                Some(Candidate("-", Ul, ctx.rowOffset, c))
              }
            } else {
              None
            }
          }
        case c if (c.isDigit) => _get_order_list(p, ctx)
        case _ => None
      }
    }

    private def _get_definition_list(p: String, ctx: Ctx) =
      p.indexOf(" :: ") match {
        case -1 => None
        case i =>
          val t = p.substring(0, i).trim
          val d = p.substring(i + " :: ".length).trim
          Some(Candidate("-", Dl, ctx.rowOffset, d, term = Some(t)))
      }

    private def _get_order_list(p: String, ctx: Ctx) = {
      val regex = """\h*(\d+)[.]\h+(.*)""".r
      regex.findFirstMatchIn(p).
        map { x =>
          val n = x.group(1)
          Candidate(n, Ol, ctx.rowOffset, x.group(2))
        }
    }
  }

  sealed trait TableMark {
    def line: LogicalLine
  }
  object TableMark {
    val topCorner = Vector('┌', '┏', '┐', '┓')
    val bottomCorner = Vector('└', '┗', '┘', '┛')
    val rowDelimiters = Vector('|', '│', '┃')
    val commonSeparatorDelimitters = Vector('-', '─', '━')
    val topSeparatorDelimiters = Vector('┬', '┯', '┰', '┳') ++ commonSeparatorDelimitters ++ topCorner
    val middleSeparatorDelimiters = Vector('├', '┝', '┠', '┣', '┤', '┥', '┨', '┫', '┼', '┿', '╂', '╋') ++ commonSeparatorDelimitters
    val bottomSeparatorDelimiters = Vector('┴', '┷', '┸', '┻') ++ commonSeparatorDelimitters ++ bottomCorner

    case class TableSeparator(line: LogicalLine) extends TableMark {
    }

    case class TableRow(line: LogicalLine) extends TableMark {
      val a = Strings.totokens(line.text, rowDelimiters)
      val fields: List[String] = a.lastOption.map(x =>
        if (x.forall(Character.isWhitespace))
          a.init
        else
          a
      ).getOrElse(a)
      def thRecord(config: Config): TRecord = TR(fields.map(x => TH(_inline_list(config, x))))
      def tdRecord(config: Config): TRecord = TR(fields.map(x => TD(_inline_list(config, x))))

      private def _inline_list(config: Config, p: String): List[Inline] = {
        // println(s"TableRow#_inline_list($p)")
        val (m, od) = parse_inline(config, p)
        // println(s"TableRow#_inline_list($od)")
        od.toList.asInstanceOf[List[Inline]] // TODO
      }
    }

    def get(p: LogicalLine): Option[TableMark] =
      p.text.headOption.flatMap {
        case '|' =>
          val t = p.text.tail
          if (t.contains('-') && _only(t))
            Some(TableSeparator(p))
          else
            Some(TableRow(p))
        case _ => None
      }

    private val _only_symbols = Vector('|', '-', '+', ':', ' ')

    private def _only(ps: String) = ps.forall(_only_symbols.contains)
  }

  sealed trait AnnotationMark {
    def location: Option[ParseLocation]
  }
  case class BrokenAnnotation(
    text: String,
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  object BrokenAnnotation {
    def apply(p: LogicalLine): BrokenAnnotation = BrokenAnnotation(
      p.text,
      p.location
    )
  }
  case class TitleAnnotation(
    title: Inline,
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  case class CaptionAnnotation(
    caption: List[Inline],
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  case class LabelAnnotation(
    label: String,
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  case class AttrHtmlAnnotation(
    text: String, // TODO
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  case class AttrLatexAnnotation(
    text: String, // TODO
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  case class GenericAnnotation(
    key: String,
    value: String,
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  sealed trait VerbatimAnnotationMark extends AnnotationMark with VerbatimMark {
    def name: String
    lazy val tagName: String = "end_" + name

    def isDone(p: String): Boolean = isDone(LogicalLine(p))
    def isDone(p: LogicalLine): Boolean = {
      val r = AnnotationMark.parse(p).map {
        case (key, params, value) => key == tagName
      }.getOrElse(false)
      // println(s"${getClass.getSimpleName}($tagName): ${p.text} => $r")
      r
    }
  }
  object VerbatimAnnotationMark {
    val elements = Vector(
      BeginSrcAnnotationClass,
      BeginExampleAnnotationClass,
      GenericBeginAnnotationClass
    )
  }
  sealed trait VerbatimAnnotationMarkClass extends VerbatimMarkClass {
    def name: String
    lazy val tagName = "begin_" + name

    def isMatch(p: String): Boolean = get(p).isDefined

    def get(p: String): Option[VerbatimMark] = get(LogicalLine(p))

    def get(p: LogicalLine): Option[VerbatimAnnotationMark] =
      if (p.text.startsWith("#+"))
        _get(p)
      else
        None

    protected def _get(p: LogicalLine): Option[VerbatimAnnotationMark] =
      AnnotationMark.parse(p).collect {
        case (key, params, value) if key == tagName => create_Mark(params, value, p.location)
      }

    protected def create_Mark(params: List[String], value: String, location: Option[ParseLocation]): VerbatimAnnotationMark
  }
  case object BeginSrcAnnotationClass extends VerbatimAnnotationMarkClass {
    val name = "src"

    protected def create_Mark(params: List[String], value: String, location: Option[ParseLocation]): VerbatimAnnotationMark =
      BeginSrcAnnotation(params, location)
  }
  case class BeginSrcAnnotation(
    parameters: List[String],
    location: Option[ParseLocation]
  ) extends VerbatimAnnotationMark {
    def name = BeginSrcAnnotationClass.name
  }
  case class EndSrcAnnotation(
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  case object BeginExampleAnnotationClass extends VerbatimAnnotationMarkClass {
    val name = "example"

    protected def create_Mark(params: List[String], value: String, location: Option[ParseLocation]): VerbatimAnnotationMark =
      BeginExampleAnnotation(params, location)
  }
  case class BeginExampleAnnotation(
    parameters: List[String],
    location: Option[ParseLocation]
  ) extends VerbatimAnnotationMark {
    def name = BeginExampleAnnotationClass.name
  }
  case class EndExampleAnnotation(
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  case object GenericBeginAnnotationClass extends VerbatimMarkClass {
    def isMatch(p: String): Boolean = get(p).isDefined
    def get(p: String): Option[VerbatimMark] = get(LogicalLine(p))
    def get(p: LogicalLine): Option[VerbatimAnnotationMark] =
      if (p.text.startsWith("#+"))
        _get(p)
      else
        None

    protected def _get(p: LogicalLine): Option[VerbatimAnnotationMark] =
      AnnotationMark.parse(p).collect {
        case (key, params, value) if key.startsWith("begin_") => GenericBeginAnnotation(key.substring("begin_".length), params, p.location)
      }
  }
  case class GenericBeginAnnotation(
    key: String,
    parameters: List[String],
    location: Option[ParseLocation]
  ) extends VerbatimAnnotationMark {
    def name = key
  }
  case class GenericEndAnnotation(
    key: String,
    location: Option[ParseLocation]
  ) extends AnnotationMark {
  }
  object AnnotationMark {
    private val _regex = "#[+]([^:]+)[:]?[ ]*(.*)".r

    def get(config: Config, p: LogicalLine): Option[AnnotationMark] = {
      if (p.text.startsWith("#+"))
        Some(_take(config, p))
      else
        None
    }

    def parse(p: LogicalLine): Option[(String, List[String], String)] =
      _regex.findFirstMatchIn(p.text).flatMap(x =>
        if (x.groupCount == 2) {
          val a = x.group(1).toLowerCase.trim
          Strings.totokens(a, " ") match {
            case Nil => None
            case key :: params =>
              val value = x.group(2)
              Some((key, params, value))
          }
        } else {
          None
        }
      )

    private def _take(config: Config, p: LogicalLine): AnnotationMark =
      parse(p).flatMap {
        case (key, params, value) => _get(config, key, params, value, p.location)
      }.getOrElse(BrokenAnnotation(p))

    // private def _take(config: Config, p: LogicalLine) =
    //   _regex.findFirstMatchIn(p.text).map(x =>
    //     if (x.groupCount == 2) {
    //       val a = x.group(1).toLowerCase.trim
    //       Strings.totokens(a, " ") match {
    //         case Nil => BrokenAnnotation(p)
    //         case key :: params =>
    //           val value = x.group(2)
    //           _get(config, key, params, value, p.location).getOrElse(BrokenAnnotation(p))
    //       }

    //     } else{
    //       BrokenAnnotation(p)
    //     }
    //   ).getOrElse(BrokenAnnotation(p))

    private def _get(
      config: Config,
      key: String,
      params: List[String],
      value: String,
      location: Option[ParseLocation]
    ): Option[AnnotationMark] = {
      // println(s"key: $key")
      Option(key) collect {
        case "title" =>
          val (m, d) = parse_inline(config, value)
          TitleAnnotation(d.getOrElse(EmptyDox), location)
        case "caption" =>
          val (m, d) = parse_inline(config, value)
          // TODO warn
          CaptionAnnotation(d.toList, location)
        case "label" => LabelAnnotation(value, location)
        case "attr_html" => AttrHtmlAnnotation(value, location)
        case "attr_latex" => AttrLatexAnnotation(value, location)
        case "begin_src" => BeginSrcAnnotation(params, location)
        case "end_src" => EndSrcAnnotation(location)
        case "begin_example" => BeginExampleAnnotation(params, location)
        case "end_example" => EndExampleAnnotation(location)
        case m =>
          if (m.startsWith("begin_"))
            GenericBeginAnnotation(m, params, location)
          else if (m.startsWith("end_"))
            GenericEndAnnotation(m, location)
          else
            GenericAnnotation(key, value, location)
      }
    }
  }

  sealed trait DoxLinesParseState extends LogicalLineReaderWriterState[Config, Dox] {
    protected val use_empty: Boolean = true

    def result = RAISE.noReachDefect(this, "result")
    // def result: Dox
    def apply(config: Config, evt: ParseEvent): Transition = {
//      println(s"in($this): $evt")
      val r = handle_event(config, evt)
//      println(s"out($this): $r")
      r
    }

    def returnFrom(doxes: Seq[Dox]): DoxLinesParseState = RAISE.noReachDefect(this, "returnFrom")

    def returnFrom(dox: Dox): DoxLinesParseState = returnFrom(Vector(dox))

    def returnFrom(as: NonEmptyVector[AnnotationMark]): DoxLinesParseState = RAISE.noReachDefect(this, s"returnFrom: $as")

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
      get_annotation_transition(config, evt) getOrElse
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

    protected def get_annotation_transition(config: Config, evt: LogicalLineEvent): Option[Transition] = {
      get_Annotation_Transition(config, evt.line)
    }

    protected def get_Annotation_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      RAISE.noReachDefect(this, "get_Annotation_Transition")

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

  sealed trait ChildDoxLinesParseState extends DoxLinesParseState {
    def parent: DoxLinesParseState

    protected def leave_to(p: (ParseMessageSequence, Seq[Tree[Dox]])): Transition = {
      val (msgs, doxtrees) = p
      leave_to(msgs, doxtrees)
    }

    private def _show(ps: Seq[Tree[Dox]]): String =
      ps.map(_.drawTree).mkString("\n")

    private def _show(p: Tree[Dox]): String = p.drawTree

    protected def leave_to(dox: Dox): Transition =
      _leave_to(ParseMessageSequence.empty, dox)

    protected def leave_to(msgs: ParseMessageSequence, doxtrees: Seq[Tree[Dox]]): Transition = {
      val doxes = doxtrees.flatMap(Dox.untreeO)
      // println(s"${getClass.getSimpleName}#leave_to: ${_show(doxtrees)} => $doxes")
      _leave_to(msgs, doxes)
    }

    private def _leave_to(msgs: ParseMessageSequence, doxes: Seq[Dox]): Transition = {
      (msgs, ParseResult.empty, parent.returnFrom(doxes))
    }

    protected def leave_to(msgs: ParseMessageSequence, doxtree: Tree[Dox]): Transition = {
      // println(s"${getClass.getSimpleName}#leave_to: ${_show(doxtree)} => ...")
      val dox = Dox.untreeE(doxtree)
      // println(s"${getClass.getSimpleName}#leave_to: ${_show(doxtree)} => $dox")
      _leave_to(msgs, dox)
    }

    private def _leave_to(msgs: ParseMessageSequence, dox: Dox): Transition = {
      (msgs, ParseResult.empty, parent.returnFrom(dox))
    }

    protected def leave_to(annotaions: NonEmptyVector[AnnotationMark]): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, parent.returnFrom(annotaions))

    protected def leave_to_in_end(config: Config, p: (ParseMessageSequence, Tree[Dox])): Transition = {
      val (msgs, doxtree) = p
      leave_to_in_end(config, msgs, doxtree)
    }

    protected def leave_to_in_end(config: Config, msgs: ParseMessageSequence, doxes: Tree[Dox]): Transition = {
      val (ms, r, s) = leave_to(msgs, doxes)
      val (ms2, r2, s2) = s.apply(config, EndEvent)
      val dox = Dox.toDox(r.get.toVector ++ r2.get.toVector)
      (ms + ms2, ParseSuccess(dox), s2)
    }

    protected def leave_to_in_end(config: Config, annotaions: NonEmptyVector[AnnotationMark]): Transition = {
      val (ms, r, s) = leave_to(annotaions)
      val (ms2, r2, s2) = s.apply(config, EndEvent)
      val dox = Dox.toDox(r.get.toVector ++ r2.get.toVector)
      (ms + ms2, ParseSuccess(dox), s2)
    }

    protected def leave_none: Transition = transit_next(parent)

    protected def leave_to(config: Config, p: ParseEvent): Transition =
      parent.apply(config, p)

    protected def leave_to(config: Config, dox: Dox, evt: ParseEvent): Transition =
      parent.returnFrom(dox).apply(config, evt)

    protected def leave_to_with_warning(config: Config, p: ParseEvent, warn: String): Transition = 
      RAISE.notImplementedYetDefect(this, s"$p: $warn")

    protected def leave_end(config: Config): Transition =
      parent.apply(config, EndEvent)

    // protected def return_From(doxes: Seq[Dox]): DoxLinesParseState = RAISE.noReachDefect(this, "return_From")

    // protected def leave_to(dox: Seq[Tree[Dox]]): Transition = ???

    // protected def leave_to(dox: Dox): Transition = return_From(dox)

    // protected def return_From(dox: Dox): Transition = RAISE.noReachDefect
  }

  case class NormalState(
    lines: Vector[Dox],
    title: Option[Inline]
  ) extends DoxLinesParseState {
    // override protected def end_Result(config: Config): ParseResult[Dox] =
    //   ParseSuccess(Paragraph(List(Text(cs.mkString))))

    // override protected def character_State(c: Char): DoxLinesParseState =
    //   copy(cs = cs :+ c)
    // protected def handle_event(config: Config, evt: LogicalLine): Transition =
    //   _get_list(evt) getOrElse _text(evt)

    override def returnFrom(doxes: Seq[Dox]): DoxLinesParseState =
      copy(lines ++ doxes)

    override def returnFrom(dox: Dox): DoxLinesParseState =
      copy(lines :+ dox)

    override def returnFrom(as: NonEmptyVector[AnnotationMark]): DoxLinesParseState =
      as.vector.collect {
        case m: TitleAnnotation => m.title
      }.headOption.
        map(x => copy(title = Some(x))).
        getOrElse(this)

    override protected def end_Transition(config: Config): Transition =
      transit_result_next(Dox.toDox(_get_head.toVector ++: lines), NormalState.init)

    private def _get_head: Option[Head] = {
      Head(
        title.toList
      ).toOption
    }

    override protected def empty_Transition(config: Config, evt: LogicalLine): Transition = transit_none

    override protected def get_List_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      // ListMark.getCandidate(evt.text).map(x => transit_next(ListState(this, NonEmptyVector(x))))
      ListMark.getCandidate(evt.text).map(x => transit_next(ListState(this, x)))

    override protected def get_Table_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      TableMark.get(evt).map(x => transit_next(TableState(this, x)))

    override protected def get_Annotation_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      AnnotationMark.get(config, evt).map {
        case m: BeginSrcAnnotation => transit_next(SourceCodeState(this, m.parameters))
        case m: BeginExampleAnnotation => transit_next(SourceCodeState(this, m.parameters)) // TODO
        case m => transit_next(AnnotationState(this, m))
      }

    override protected def text_transition(config: Config, evt: LogicalLineEvent): Transition = {
      val (msgs, result, _) = DoxInlineParser.apply(config.inlineConfig, evt.line.text)
      result match {
        case EmptyParseResult() => (msgs, ParseResult.empty, this)
        case ParseSuccess(ast, ws) =>
          val contents = _normalize(ast)
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

    private def _is_inline(p: Dox): Boolean = p.isInstanceOf[Inline]

    private def _is_inline(ps: List[Dox]): Boolean = if (ps.isEmpty) false else ps.forall(_is_inline)

    private def _normalize(p: Seq[Dox]): List[Dox] = p.flatMap {
      case m: Fragment => _normalize(m.contents)
      case m => List(m)
    }.toList

    private def _to_paragraph(ps: Seq[Dox]): Paragraph = Paragraph(ps.toList)
  }

  object NormalState {
    val init = NormalState(Vector.empty, None)
  }

  case class ListState(
    parent: DoxLinesParseState,
    slots: NonEmptyVector[ListState.SlotGroup]
  ) extends ChildDoxLinesParseState {
    import ListState._

    override def returnFrom(doxes: Seq[Dox]): DoxLinesParseState = {
      val xs = doxes.collect {
        case x: ListContent => x
      }
      copy(slots = slots.mapLast(_.addChildren(xs)))
    }

    private def _result: Dox = {
      Dox.toDox(slots.map(_.toDox))
      // slots.head.listElement
      // val xs = slots.map(_.doxItem)
      // slots.head.doxContainer(xs.vector)
    }

    override protected def end_Transition(config: Config): Transition =
      parent.returnFrom(_result).apply(config, EndEvent)

    override protected def handle_line(config: Config, evt: LogicalLineEvent): Transition = {
      ListMark.getCandidate(evt.line.text) map { x =>
        if (slots.last.rowOffset == x.rowOffset)
          transit_next(copy(slots = _add_slot(x)))
        else if (slots.last.rowOffset < x.rowOffset)
          transit_next(ListState(this, x))
        else
          parent.returnFrom(_result).apply(config, evt)
      } getOrElse {
        val s = evt.line.text.trim
        transit_next(copy(slots = slots.mapLast(_.addLine(s))))
      }
    }

    private def _add_slot(p: ListMark.Candidate): NonEmptyVector[SlotGroup] = {
      val a = slots.last.append(p)
      slots.initVector ++: a
    }
  }
  object ListState {
    case class SlotGroup(slots: NonEmptyVector[Slot]) {
      def listElement = slots.head.listElement
      def rowOffset = slots.last.rowOffset

      def add(p: Slot) = copy(slots = slots :+ p)

      def addLine(p: String) = copy(slots = slots.mapLast(_.addLine(p)))

      def addChildren(ps: Seq[ListContent]) = copy(slots = slots.mapLast(_.addChildren(ps)))

      def append(p: ListMark.Candidate): NonEmptyVector[SlotGroup] =
        if (listElement == p.listElement)
          NonEmptyVector(copy(slots = slots :+ Slot(p)))
        else
          NonEmptyVector.create(this, SlotGroup(p))

      def toDox: Dox = listElement match {
        case m: Ul => Ul(_build_lis)
        case m: Ol => Ol(_build_lis)
        case m: Dl => Dl(_build_dtdds)
      }

      private def _build_lis: Seq[Li] = slots.map(_.toLi).list

      private def _build_dtdds: List[(Dt, Dd)] = slots.map(_.toDtDd).list
    }
    object SlotGroup {
      def apply(p: ListMark.Candidate): SlotGroup = SlotGroup(NonEmptyVector(Slot(p)))
    }

    case class Slot(
      candidate: ListMark.Candidate,
      lines: Vector[String] = Vector.empty,
      children: Vector[ListContent] = Vector.empty
    ) {
      def listElement = candidate.listElement
      def rowOffset = candidate.rowOffset
      def toLi = {
        val ts = Text(DoxUtils.concatLines(candidate.text, lines))
        Li(ts +: children)
      }
      def toDtDd: (Dt, Dd) = {
        val dt = Dt(candidate.term getOrElse candidate.text)
        val a = Text(DoxUtils.concatLines(candidate.text, lines))
        val dd = Dd(List(a))
        (dt, dd)
      }
      private def doxContainer(ps: Seq[Li]) = candidate.listElement match {
        case m: Ul => Ul(ps)
        case m: Ol => Ol(ps)
      }
      def addLine(p: String) = copy(lines = lines :+ p)
      def addChildren(ps: Seq[ListContent]) = copy(children = children ++ ps)
    }

    def apply(parent: DoxLinesParseState, p: ListMark.Candidate): ListState = ListState(parent, NonEmptyVector(SlotGroup(p)))
  }

  case class ListStateOld(
    parent: DoxLinesParseState,
    listMarkCandidates: NonEmptyVector[ListMark.Candidate]
  ) extends ChildDoxLinesParseState {
    override protected def end_Transition(config: Config): Transition = {
      // println(s"listMarkCandidates: ${listMarkCandidates}")
      val md = close_state(config, listMarkCandidates.head, listMarkCandidates.tailVector)
      leave_to_in_end(config, md)
    }

    protected def close_state(config: Config, p: ListMark.Candidate, ps: Vector[ListMark.Candidate]): (ParseMessageSequence, Tree[Dox]) = {
      case class Z(
        base: ListMark.Candidate,
        followers: Vector[ListMark.Candidate] = Vector.empty,
        trees: Vector[Tree[Dox]] = Vector.empty,
        messages: ParseMessageSequence = ParseMessageSequence.empty
      ) {
        def r: (ParseMessageSequence, Tree[Dox]) = {
          // println(s"Z: ${this}")
          // trees.map(x => println(s"Z tree: ${x.drawTree}"))
          base.term.map(_to_dl).getOrElse(_to_uol)
        }

        private def _to_uol = {
          val uol = base.listElement
          val (bmsgs, node) = parse_inline(config, base.text)
          val ms = messages + bmsgs
          node.map { n =>
            // val licontent = n match {
            //   case m: ListContent => m
            //   case _ => RAISE.noReachDefect(this, "end_Transition")
            // }
            // followers.headOption.map { x =>
            //   val (cmsgs, children) = go(NonEmptyList.nel(x, followers.tail.toList))
            //   val a0: Tree[Dox] = Tree.node(Li.empty, Stream(Tree.leaf(licontent), children))
            //   val a1: Vector[Tree[Dox]] = trees :+ a0
            //   val a: Stream[Tree[Dox]] = a1.toStream
            //   (ms + cmsgs, Tree.node(uol, a))
            // }.getOrElse {
            //   val a0: Tree[Dox] = Tree.node(Li.empty, Stream(Tree.leaf(licontent)))
            //   val a1: Vector[Tree[Dox]] = trees :+ a0
            //   val a: Stream[Tree[Dox]] = a1.toStream
            //   (ms, Tree.node(uol, a))
            // }
            val (cmsgs, x) = _parse_item(n)
            val a = trees ++ x
            (ms + cmsgs, Tree.node(uol, a.toStream))
          }.getOrElse(
            (ms, Tree.node(uol, trees.toStream))
          )
        }

        private def _to_dl(term: String) = {
          val uol = base.listElement
          val (bmsgs, node) = parse_inline(config, base.text)
          val ms = messages + bmsgs
          node.map { n =>
            val (cmsgs, x) = _parse_dtdd(term, n)
            val a = trees ++ x
            // println(s"_to_dl: ${a.map(_.drawTree)}")
            (ms + cmsgs, Tree.node(uol, a.toStream))
          }.getOrElse(
            (ms, Tree.node(uol, trees.toStream))
          )
        }

        def +(rhs: ListMark.Candidate) = {
          // println(s"+: $rhs <= $this")
          val r = if (rhs.rowOffset <= base.rowOffset) {
            val (bmsgs, node) = parse_inline(config, base.text)
            val ms = messages + bmsgs
            node.map { n =>
              val (cmsgs, x) = _parse_item(base.term, n)
              Z(rhs, followers = Vector.empty, trees = trees ++ x, messages = ms + cmsgs)
            }.getOrElse(
              copy(followers = followers :+ rhs, messages = ms)
            )
          } else {
            copy(followers = followers :+ rhs)
          }
          // println(s"+: $rhs => $r")
          // println(s"""+: $rhs => ${r.trees.map(_.drawTree).mkString("\n")}""")
          r
        }

        private def _parse_item(term: Option[String], n: Dox): (ParseMessageSequence, Vector[Tree[Dox]]) =
          term.map(_parse_dtdd(_, n)).getOrElse(_parse_item(n))

        private def _parse_item(n: Dox): (ParseMessageSequence, Vector[Tree[Dox]]) = {
          val licontent = n match {
            case m: Text => _parse_item_text(m)
            case m: ListContent => m
            case _ => RAISE.noReachDefect(this, "_parse_item")
          }
          followers.headOption.map { x =>
            val (cmsgs, children) = close_state(config, x, followers.tail)
            val a: Tree[Dox] = Tree.node(Li.empty, Stream(licontent.tree, children))
            (cmsgs, Vector(a))
          }.getOrElse {
            val a: Tree[Dox] = Tree.node(Li.empty, Stream(licontent.tree))
            (ParseMessageSequence.empty, Vector(a))
          }
        }

        private def _parse_item_text(p: Text): ListContent = ??? // ListContentBuilder(p)

        private def _parse_dtdd(term: String, n: Dox): (ParseMessageSequence, Vector[Tree[Dox]]) = {
          val (tmsgs, t) = parse_inline(config, term)
          t match {
            case Some(s) => 
              val (msgs, r) = _parse_dtdd(s, n)
              (tmsgs + msgs, r)
            case None =>
              (tmsgs, Vector.empty) // TODO DoxError
          }
        }

        private def _parse_dtdd(term: Dox, n: Dox): (ParseMessageSequence, Vector[Tree[Dox]]) = {
          val dt: Tree[Dox] = Tree.node(Dt, Stream(term.tree))
          val licontent = n match {
            case m: ListContent => m
            case _ => RAISE.noReachDefect(this, "_parse_dtdd")
          }
          followers.headOption.map { x =>
            val (cmsgs, children) = close_state(config, x, followers.tail)
            val dd: Tree[Dox] = Tree.node(Dd, Stream(licontent.tree, children))
            (cmsgs, Vector(dt, dd))
          }.getOrElse {
            val dd: Tree[Dox] = Tree.node(Dd, Stream(licontent.tree))
            (ParseMessageSequence.empty, Vector(dt, dd))
          }
        }
      }
      ps./:(Z(p))(_+_).r
    }

    override protected def empty_Transition(config: Config, evt: LogicalLine): Transition = {
      // println(s"listMarkCandidates: ${listMarkCandidates}")
      val (m, d) = close_state(config, listMarkCandidates.head, listMarkCandidates.tailVector)
      leave_to(m, d)
    }

    override protected def get_List_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      ListMark.getCandidate(evt.text).map(x =>
        transit_next(copy(listMarkCandidates = listMarkCandidates :+ x)))

    override protected def get_Table_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      None // XXX

    override protected def get_Annotation_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      None

    override protected def text_transition(config: Config, evt: LogicalLineEvent): Transition = {
      if (_is_append_item(evt))
        _append_item(config, evt)
      else
        _next_text(config, evt)
    }

    private def _is_append_item(evt: LogicalLineEvent) = false // TODO

    private def _append_item(config: Config, evt: LogicalLineEvent) = {
      val a = copy(listMarkCandidates = listMarkCandidates.replace(listMarkCandidates.last, listMarkCandidates.last.add(evt.line)))
      transit_next(a)
    }

    private def _next_text(config: Config, evt: LogicalLineEvent) = {
      val lis = listMarkCandidates.vector.map(_.toDox).asInstanceOf[Seq[Li]]
      val dox: Dox = Ul(lis) // TODO
      parent.returnFrom(dox).apply(config, evt)
    }

//    protected def get_List_Transition(p: LogicalLine): Option[Transition] =
      // ListMark.getCandidate(p.text).map { x =>
      //   val (msgs, result, _) = DoxInlineParser.apply(x.text)
      //   (msgs, ParseResult.empty, ListState(x.toMark, result.ast))
      // }
  }
  object ListStateOld {
  }

  case class TableState(
    parent: DoxLinesParseState,
    blocks: TableState.Blocks,
    separators: TableState.Separators,
    annotation: Vector[AnnotationMark] = Vector.empty,
    inSeparator: Boolean = false
  ) extends ChildDoxLinesParseState {
    override protected def end_Transition(config: Config): Transition = {
      val md = close_state(config)
      leave_to_in_end(config, md)
    }

    protected def close_state(config: Config) = {
      // println(s"close_state: $annotation")
      val caption = annotation.collect {
        case CaptionAnnotation(title, _) => Caption(title)
      }.headOption
      // println("Caption:" + caption)
      val label = annotation.collect {
        case LabelAnnotation(label, _) => label
      }.headOption
      // println("Label:" + label)
      val head = blocks.head(config)
      val body = blocks.body(config)
      val foot = blocks.foot(config)
      val side = None
      val cg = _make_colgroup(separators)
      // println(s"TableState#close_state: $blocks")
      val table: Dox = Table(head, body, foot, side, cg, caption, label)
      (ParseMessageSequence.empty, Tree.leaf(table))
    }

    private def _make_colgroup(p: TableState.Separators): Option[Colgroup] =
      p.separators.headOption.flatMap(_make_colgroup)

    private def _make_colgroup(p: TableMark.TableSeparator): Option[Colgroup] = {
      val normalizedtext = {
        val a = p.line.text.headOption match {
          case None => ""
          case Some(s) =>
            if (TableMark.rowDelimiters.contains(s))
              p.line.text.tail
            else
              p.line.text
        }
        a.lastOption match {
          case None => ""
          case Some(s) => 
            if (TableMark.rowDelimiters.contains(s))
              a.init
            else
              a
        }
      }
      if (normalizedtext.isEmpty) {
        None
      } else {
        val a = Strings.totokens(normalizedtext, TableMark.rowDelimiters)
        val b = _aligns_(a.map(_.trim))
        if (b.forall(_.isEmpty))
          None
        else
          Some(
            Colgroup(b.map {
              case Some(s) => Col(s)
              case None => Col()
            })
          )
      }
    }

    private def _aligns_(p: List[String]): List[Option[Table.Align]] =
      p.map(_get_align)

    private def _get_align(p: String): Option[Table.Align] =
      if (p.length < 2) {
        None
      } else {
        (p.head, p.last) match {
          case (':', ':') => Some(Table.Align.Center)
          case (':', _) => Some(Table.Align.Left)
          case (_, ':') => Some(Table.Align.Right)
          case _ => None
        }
      }

    override protected def empty_Transition(config: Config, evt: LogicalLine): Transition = {
      val (m, d) = close_state(config)
      leave_to(m, Vector(d))
    }

    override protected def get_List_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      None // TODO

    override protected def get_Table_Transition(config: Config, evt: LogicalLine): Option[Transition] = {
      val a = TableMark.get(evt).map {
        case m: TableMark.TableSeparator => copy(separators = separators.add(m), inSeparator = true)
        case m: TableMark.TableRow =>
          if (inSeparator)
            copy(blocks = blocks.addNewBlock(m), inSeparator = false)
          else
            copy(blocks = blocks.add(m))
      }
      a.map(transit_next)
    }

    override protected def text_Transition(config: Config, evt: LogicalLine): Transition = {
      val (m, d) = close_state(config)
      val t = ???
      leave_to(m, Vector(d, t))
    }
  }
  object TableState {
    import TableMark._

    case class Separators(separators: Vector[TableSeparator]) {
      def add(p: TableSeparator) = copy(separators = separators :+ p)
    }
    object Separators {
      val init = Separators(Vector.empty)
    }

    case class Blocks(blocks: NonEmptyVector[Block]) {
      def add(p: TableRow) = Blocks(blocks.mapLast(_ :+ p))
      def addNewBlock(p: TableRow) = Blocks(blocks :+ Block(p))
      def head(config: Config): Option[THead] = blocks.length match {
        case 1 => None
        case _ => Some(THead(blocks(0).thRecords(config).toList))
      }
      def body(config: Config): TBody = blocks.length match {
        case 1 => TBody(blocks(0).tdRecords(config).toList)
        case _ => TBody(blocks(1).tdRecords(config).toList)
      }
      def foot(config: Config): Option[TFoot] = blocks.length match {
        case 1 => None
        case 2 => None
        case 3 => Some(TFoot(blocks(2).tdRecords(config).toList))
      }
    }
    object Blocks {
      val init = Blocks(NonEmptyVector(Block.empty))

      def apply(p: TableRow): Blocks = Blocks(NonEmptyVector(Block(p)))
    }
    case class Block(rows: Vector[TableRow]) {
      def thRecords(config: Config): Vector[TRecord] = rows.map(_.thRecord(config))
      def tdRecords(config: Config): Vector[TRecord] = rows.map(_.tdRecord(config))

      def :+(p: TableRow) = Block(rows :+ p)
    }
    object Block {
      val empty = Block(Vector.empty)

      def apply(p: TableRow, ps: TableRow*): Block = Block(p +: ps.toVector)
    }
//    case class Block(rows: Vector[Row])
//    case class Row(columns: Vector[Dox])

    def apply(parent: DoxLinesParseState, p: TableMark): TableState =
      p match {
        case m: TableSeparator => TableState(parent, Blocks.init, Separators.init)
        case m: TableRow => TableState(parent, Blocks(m), Separators.init)
      }

    def apply(parent: DoxLinesParseState, p: TableMark, as: Vector[AnnotationMark]): TableState =
      p match {
        case m: TableSeparator => TableState(parent, Blocks.init, Separators.init, as)
        case m: TableRow => TableState(parent, Blocks(m), Separators.init, as)
      }
  }

  // case class TableSeparatorState(
  //   parent: DoxLinesParseState
  // ) extends ChildDoxLinesParseState {
  //   override protected def end_transition(config: Config): Transition =
  //     leave_end(config)

  //   override protected def empty_transition(config: Config, evt: LogicalLineEvent): Transition =
  //     leave_to(config, evt)

  //   override protected def get_list_transition(config: Config, evt: LogicalLineEvent): Option[Transition] =
  //     Some(leave_none)

  //   override protected def get_table_transition(config: Config, evt: LogicalLineEvent): Option[Transition] =
  //     TableMark.get(evt.line).map {
  //       case m: TableMark.TableSeparator => this
  //       case m: TableMark.TableRow => leave_to(config, evt)
  //     }.orElse(Some(leave_none))

  //   override protected def text_transition(config: Config, evt: LogicalLineEvent): Transition =
  //     leave_to(config, evt)
  // }

  protected final def parse_inline(c: Config, p: String): (ParseMessageSequence, Option[Inline]) = {
    // println(s"parse_inline: $c, $p")
    val (msgs, result, _) = DoxInlineParser.apply(c.inlineConfig, p)
    result match {
      case EmptyParseResult() => (msgs, None)
      case ParseSuccess(ast, ws) =>
        val d = Dox.toDox(ast)
        val i = d.asInstanceOf[Inline]
        (msgs :++ ws, Some(i))
      case ParseFailure(es, ws) => (msgs :++ es :++ ws, None)
    }
  }

  case class AnnotationState(
    parent: DoxLinesParseState,
    annotations: NonEmptyVector[AnnotationMark]
  ) extends ChildDoxLinesParseState {
    override protected def end_Transition(config: Config): Transition =
      leave_to_in_end(config, annotations)

    override protected def get_list_transition(config: Config, evt: LogicalLineEvent): Option[Transition] =
      for {
        s <- evt.getLogicalLineText
        _ <- ListMark.getCandidate(s)
      } yield {
        leave_to_with_warning(config, evt, "List does not support annotations.")
      }

    override protected def get_Table_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      TableMark.get(evt).map { x =>
        transit_next(TableState(parent, x, annotations.vector))
      }

    override protected def get_Annotation_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      AnnotationMark.get(config, evt).collect {
        case m: BeginSrcAnnotation => transit_next(SourceCodeState(parent, m.parameters, annotations.vector))
        case m => transit_next(AnnotationState(parent, annotations :+ m))
      }

    override protected def text_transition(config: Config, evt: LogicalLineEvent): Transition =
      leave_to_with_warning(config, evt, s"Dangling annotations: $annotations")
  }
  object AnnotationState {
    def apply(
      parent: DoxLinesParseState,
      annotation: AnnotationMark
    ): AnnotationState = AnnotationState(parent, NonEmptyVector(annotation))
  }

  case class SourceCodeState(
    parent: DoxLinesParseState,
    parameters: List[String],
    annotations: Vector[AnnotationMark] = Vector.empty,
    code: LogicalLines = LogicalLines.empty
  ) extends ChildDoxLinesParseState {
    override protected def end_Transition(config: Config): Transition =
      // leave_to_in_end(config, annotations)
      ???

    override protected def handle_line(config: Config, evt: LogicalLineEvent): Transition = {
      // println(s"SourceCodeState#handle_line $evt")
      AnnotationMark.get(config, evt.line).collect {
        case m: EndSrcAnnotation => leave_to(_source_code)
      }.getOrElse(transit_next(copy(code = code :+ evt.line.text)))
    }

    private def _source_code = {
      val cs = code.text
      val attrs = VectorMap.empty[String, String]
      val loc = annotations.toStream.flatMap(_.location).headOption.orElse(code.lines.toStream.flatMap(_.location).headOption)
      Program(cs, attrs, loc)
    }
  }

  // trait ListContentBuilder {
  //   def r: ListContent
  //   def +(rhs: String): ListContentBuilder
  //   def comeback(ps: Vector[Dox]): ListContentBuilder

  //   protected final def get_mark(p: String): Option[(Int, String, String)] = {
  //     ???
  //   }

  //   protected final def make_dox(p: String) = Text(p)
  // }
  // object ListContentBuilder {
  //   case class Plain(xs: Vector[Dox]) extends ListContentBuilder {
  //     def r = xs match {
  //       case Vector() => Text("")
  //       case Vector(x) => x match {
  //         case m: ListContent => m
  //         case _ => Fragment(x)
  //       }
  //       case _ => Fragment(xs.toList)
  //     }

  //     def +(rhs: String) = {
  //       get_mark(rhs) match {
  //         case Some((i, mark, c)) => mark match {
  //           case "-" => Ul(this, i, make_dox(c))
  //           case _ => _content(rhs)
  //         }
  //         case None => _content(rhs)
  //       }
  //     }
  //     def comeback(ps: Vector[Dox]) = copy(xs = xs ++ ps)

  //     private def _content(p: String) = copy(xs :+ Text(p))
  //   }

  //   case class Ul(
  //     parent: ListContentBuilder,
  //     indent: Int,
  //     xs: Vector[Dox] = Vector.empty
  //   ) extends ListContentBuilder {
  //     def r = ???

  //     def +(rhs: String) = {
  //       get_mark(rhs) match {
  //         case Some((i, mark, c)) => mark match {
  //           case "-" => Ul(this, i, make_dox(c))
  //           case _ => _content(rhs)
  //         }
  //         case None => _content(rhs)
  //       }
  //     }

  //     def comeback(ps: Vector[Dox]) = ???

  //     private def _content(p: String) = ???
  //   }
  //   object Ul {
  //     def apply(
  //       parent: ListContentBuilder,
  //       indent: Int,
  //       content: Dox
  //     ): Li = Li(Ul(parent, indent), indent, Vector(content))
  //   }

  //   case class Li(
  //     parent: ListContentBuilder,
  //     indent: Int,
  //     content: Vector[Dox]
  //   ) extends ListContentBuilder {
  //     def r = ???

  //     def +(rhs: String) = {
  //       get_mark(rhs) match {
  //         case Some((i, mark, c)) => mark match {
  //           case "-" =>
  //             if (i == indent)
  //               Li(this, indent, rhs)
  //             else if (indent < i)
  //               ???
  //             else
  //               parent.comeback(content) + rhs
  //           case _ => _content(rhs)
  //         }
  //         case None => _content(rhs)
  //       }
  //     }

  //     def comeback(ps: Vector[Dox]) = ???

  //     private def _content(p: String) = copy(content = content :+ make_dox(p))
  //   }
  //   object Li {
  //     def apply(
  //       parent: ListContentBuilder,
  //       indent: Int,
  //       content: String
  //     ): Li = {
  //       Li(parent, indent, Vector(Text(content)))
  //     }
  //   }

  //   case class Ol(indent: Int, xs: Vector[Dox]) extends ListContentBuilder {
  //     def r = ???

  //     def +(rhs: String) = {
  //       get_mark(rhs) match {
  //         case Some((i, mark, c)) => mark match {
  //           case "-" => ???
  //           case _ => ???
  //         }
  //         case None => ???
  //       }
  //     }

  //     def comeback(ps: Vector[Dox]) = ???
  //   }

  //   def apply(p: Text): ListContent = apply(p.contents)
  //   def apply(p: String): ListContent = apply(Strings.tolines(p))
  //   def apply(ps: Vector[String]): ListContent = {
  //     ps match {
  //       case Vector() => Text("")
  //       case Vector(c) => Text(c)
  //       case c +: xs => xs.foldLeft(_init(c))(_+_).r
  //     }
  //   }

  //   private def _init(p: String): ListContentBuilder = Plain(Vector(Text(p)))
  // }
}
