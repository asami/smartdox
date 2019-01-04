package org.smartdox.parser

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.parser._
import org.goldenport.collection.NonEmptyVector
import org.goldenport.util.VectorUtils
import org.smartdox._

/*
 * @since   Nov. 12, 2018
 * @version Dec. 31, 2018
 * @author  ASAMI, Tomoharu
 */
object DoxLinesParser {
  type Transition = (ParseMessageSequence, ParseResult[Dox], DoxLinesParseState)

  // def parse(p: LogicalParagraph): Dox = parse(p.lines)

  def parse(config: Config, p: LogicalParagraph): Dox = parse(config, p.lines)

  // def parse(p: LogicalLines): Dox = parse(Config.default, p)

  def parse(config: Config, p: LogicalLines): Dox = {
    println(s"DoxLinesParser#parse(${config}): $p")
    // DoxInlineParser.toDox(p.lines.map(parse))
    val parser = LogicalLineReaderWriterStateClass(config, NormalState.init)
    val (messages, result, state) = parser.apply(p)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() =>
        println(s"DoxLinesParser#parse[EmptyResult]: $p")
        RAISE.notImplementedYetDefect
    }
  }

  case class Config(
    isDebug: Boolean = false,
    inlineConfig: DoxInlineParser.Config = DoxInlineParser.Config.default
  ) extends ParseConfig {
  }
  object Config {
    val default = Config()
    val debug = Config(true)
    val orgmodeFull = default.copy(
      inlineConfig = DoxInlineParser.Config.orgmodeFull
    )
  }

  case class ListMark(mark: String, level: Int) {
  }
  object ListMark {
    case class Candidate(
      mark: String,
      listElement: Dox,
      rawOffset: Int,
      text: String,
      lines: LogicalLines = LogicalLines.empty,
      term: Option[String] = None
    ) {
      def add(p: LogicalLine): Candidate = copy(text = text + " " + p.text)
    }
    case class Ctx(rawOffset: Int = 0) {
      def space = Ctx(rawOffset + 1)
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
                Some(Candidate("-", Ul, ctx.rawOffset, c))
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
          Some(Candidate("-", Dl, ctx.rawOffset, d, term = Some(t)))
      }

    private def _get_order_list(p: String, ctx: Ctx) = {
      val regex = """\h*(\d+)[.]\h+(.*)""".r
      regex.findFirstMatchIn(p).
        map { x =>
          val n = x.group(1)
          Candidate(n, Ol, ctx.rawOffset, x.group(2))
        }
    }
  }

  sealed trait TableMark {
    def line: LogicalLine
  }
  object TableMark {
    case class TableSeparator(line: LogicalLine) extends TableMark
    case class TableRow(line: LogicalLine) extends TableMark {
      val a = Strings.totokens(line.text, "|")
      val fields: List[String] = a.lastOption.map(x =>
        if (x.forall(Character.isWhitespace))
          a.init
        else
          a
      ).getOrElse(a)
      def thRecord(config: Config): TRecord = TR(fields.map(x => TH(_inline_list(config, x))))
      def tdRecord(config: Config): TRecord = TR(fields.map(x => TD(_inline_list(config, x))))

      private def _inline_list(config: Config, p: String): List[Inline] = {
        println(s"TableRow#_inline_list($p)")
        val (m, od) = parse_inline(???, p)
        println(s"TableRow#_inline_list($od)")
        od.toList.asInstanceOf[List[Inline]] // TODO
      }
    }

    def get(p: LogicalLine): Option[TableMark] =
      p.text.headOption.flatMap {
        case '|' => p.text.lift(1) map {
          case '-' => TableSeparator(p)
          case _ => TableRow(p)
        }
        case _ => None
      }
  }

  sealed trait AnnotationMark {
  }
  case class BrokenAnnotation(text: String) extends AnnotationMark {}
  case class CaptionAnnotation(
    caption: List[Inline]
  ) extends AnnotationMark {
  }
  case class LabelAnnotation(
    label: String
  ) extends AnnotationMark {
  }
  case class AttrHtmlAnnotation(
    text: String // TODO
  ) extends AnnotationMark {
  }
  case class AttrLatexAnnotation(
    text: String // TODO
  ) extends AnnotationMark {
  }
  case class GenericAnnotation(
    key: String,
    value: String
  ) extends AnnotationMark {
  }
  object AnnotationMark {
    private val _regex = "#[+]([^:]+):(.*)".r

    def get(p: String): Option[AnnotationMark] = {
      if (p.startsWith("#+"))
        Some(_take(p))
      else
        None
    }

    private def _take(p: String) =
      _regex.findFirstMatchIn(p).map(x =>
        if (x.groupCount == 2) {
          val key = x.group(1).toLowerCase
          val value = x.group(2).trim
          _get(key, value).getOrElse(BrokenAnnotation(p))
        } else{
          BrokenAnnotation(p)
        }
      ).getOrElse(BrokenAnnotation(p))

    private def _get(key: String, value: String): Option[AnnotationMark] = {
      println(s"key: $key")
      Option(key) collect {
        case "caption" =>
          val (m, d) = parse_inline(???, value)
          // TODO warn
          CaptionAnnotation(d.toList)
        case "label" => LabelAnnotation(value)
        case "attr_html" => AttrHtmlAnnotation(value)
        case "attr_latex" => AttrLatexAnnotation(value)
        case _ => GenericAnnotation(key, value)
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

    def returnFrom(dox: Dox): DoxLinesParseState = RAISE.noReachDefect(this, "returnFrom")

    protected def handle_event(config: Config, evt: ParseEvent): Transition =
      evt match {
        case StartEvent => start_transition(config)
        case EndEvent => end_transition(config)
//        case m: LogicalLineEvent if use_empty && m.line.isEmpty => empty_transition(config, m.line)
        case m: LogicalLineEvent =>
          val line = m.line
          if (use_empty && line.isEmptyLine)
            empty_transition(config, m)
          else
            get_list_transition(config, m) orElse
              get_table_transition(config, m) orElse
              get_annotation_transition(config, m) getOrElse
              text_transition(config, m)
        case m => RAISE.noReachDefect(this, "handle_event")
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

    protected def leave_to(msgs: ParseMessageSequence, doxtrees: Seq[Tree[Dox]]): Transition = {
      val doxes = doxtrees.flatMap(Dox.untreeO)
      println(s"${getClass.getSimpleName}#leave_to: ${_show(doxtrees)} => $doxes")
      _leave_to(msgs, doxes)
    }

    private def _leave_to(msgs: ParseMessageSequence, doxes: Seq[Dox]): Transition = {
      (msgs, ParseResult.empty, parent.returnFrom(doxes))
    }

    protected def leave_to(msgs: ParseMessageSequence, doxtree: Tree[Dox]): Transition = {
      println(s"${getClass.getSimpleName}#leave_to: ${_show(doxtree)} => ...")
      val dox = Dox.untreeE(doxtree)
      println(s"${getClass.getSimpleName}#leave_to: ${_show(doxtree)} => $dox")
      _leave_to(msgs, dox)
    }

    private def _leave_to(msgs: ParseMessageSequence, dox: Dox): Transition = {
      (msgs, ParseResult.empty, parent.returnFrom(dox))
    }
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

    protected def leave_none: Transition = transit_next(parent)

    protected def leave_to(config: Config, p: ParseEvent): Transition =
      parent.apply(config, p)

    protected def leave_to_with_warning(config: Config, p: ParseEvent, warn: String): Transition = 
      ???

    protected def leave_end(config: Config): Transition =
      parent.apply(config, EndEvent)

    // protected def return_From(doxes: Seq[Dox]): DoxLinesParseState = RAISE.noReachDefect(this, "return_From")

    // protected def leave_to(dox: Seq[Tree[Dox]]): Transition = ???

    // protected def leave_to(dox: Dox): Transition = return_From(dox)

    // protected def return_From(dox: Dox): Transition = RAISE.noReachDefect
  }


  case class NormalState(
    lines: Vector[Dox]
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

    override protected def end_Transition(config: Config): Transition =
      transit_result_next(Dox.toDox(lines), NormalState.init)

    override protected def empty_Transition(config: Config, evt: LogicalLine): Transition = ???

    override protected def get_List_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      ListMark.getCandidate(evt.text).map(x => transit_next(ListState(this, NonEmptyVector(x))))

    override protected def get_Table_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      TableMark.get(evt).map(x => transit_next(TableState(this, x)))

    override protected def get_Annotation_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      AnnotationMark.get(evt.text).map(x => transit_next(AnnotationState(this, x)))

    override protected def text_Transition(config: Config, evt: LogicalLine): Transition = {
      val (msgs, result, _) = DoxInlineParser.apply(config.inlineConfig, evt.text)
      result match {
        case EmptyParseResult() => (msgs, ParseResult.empty, this)
        case ParseSuccess(ast, ws) =>
          val p = Paragraph(ast.toList)
          (msgs :++ ws, ParseResult.empty, copy(lines = lines :+ p))
        case ParseFailure(es, ws) => (msgs :++ es :++ ws, ParseResult.empty, this)
      }
    }

    private def _to_paragraph(ps: Seq[Dox]): Paragraph = Paragraph(ps.toList)
  }

  object NormalState {
    val init = NormalState(Vector.empty)
  }

  case class ListState(
    parent: DoxLinesParseState,
    listMarkCandidates: NonEmptyVector[ListMark.Candidate]
  ) extends ChildDoxLinesParseState {
    override protected def end_Transition(config: Config): Transition = {
      println(s"listMarkCandidates: ${listMarkCandidates}")
      val md = close_state(config, listMarkCandidates.head, listMarkCandidates.tail)
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
          println(s"Z: ${this}")
          trees.map(x => println(s"Z tree: ${x.drawTree}"))
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
          println(s"+: $rhs <= $this")
          val r = if (rhs.rawOffset <= base.rawOffset) {
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
          println(s"+: $rhs => $r")
          println(s"""+: $rhs => ${r.trees.map(_.drawTree).mkString("\n")}""")
          r
        }

        private def _parse_item(term: Option[String], n: Dox): (ParseMessageSequence, Vector[Tree[Dox]]) =
          term.map(_parse_dtdd(_, n)).getOrElse(_parse_item(n))

        private def _parse_item(n: Dox): (ParseMessageSequence, Vector[Tree[Dox]]) = {
          val licontent = n match {
            case m: ListContent => m
            case _ => RAISE.noReachDefect(this, "_parse_item")
          }
          followers.headOption.map { x =>
            val (cmsgs, children) = close_state(config, x, followers.tail)
            val a: Tree[Dox] = Tree.node(Li.empty, Stream(Tree.leaf(licontent), children))
            (cmsgs, Vector(a))
          }.getOrElse {
            val a: Tree[Dox] = Tree.node(Li.empty, Stream(Tree.leaf(licontent)))
            (ParseMessageSequence.empty, Vector(a))
          }
        }

        private def _parse_dtdd(term: String, n: Dox): (ParseMessageSequence, Vector[Tree[Dox]]) = {
          val dt: Tree[Dox] = Tree.leaf(Dt(term))
          val licontent = n match {
            case m: ListContent => m
            case _ => RAISE.noReachDefect(this, "_parse_dtdd")
          }
          followers.headOption.map { x =>
            val (cmsgs, children) = close_state(config, x, followers.tail)
            val dd: Tree[Dox] = Tree.node(Dd, Stream(Tree.leaf(licontent), children))
            (cmsgs, Vector(dt, dd))
          }.getOrElse {
            val dd: Tree[Dox] = Tree.node(Dd, Stream(Tree.leaf(licontent)))
            (ParseMessageSequence.empty, Vector(dt, dd))
          }
        }
      }
      ps./:(Z(p))(_+_).r
    }

    override protected def empty_Transition(config: Config, evt: LogicalLine): Transition = {
      println(s"listMarkCandidates: ${listMarkCandidates}")
      val (m, d) = close_state(config, listMarkCandidates.head, listMarkCandidates.tail)
      leave_to(m, d)
    }

    override protected def get_List_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      ListMark.getCandidate(evt.text).map(x =>
        transit_next(copy(listMarkCandidates = listMarkCandidates :+ x)))

    override protected def get_Table_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      None // XXX

    override protected def get_Annotation_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      None

    override protected def text_Transition(config: Config, evt: LogicalLine): Transition = {
      val a = copy(listMarkCandidates = listMarkCandidates.replace(listMarkCandidates.last, listMarkCandidates.last.add(evt)))
      transit_next(a)
    }

//    protected def get_List_Transition(p: LogicalLine): Option[Transition] =
      // ListMark.getCandidate(p.text).map { x =>
      //   val (msgs, result, _) = DoxInlineParser.apply(x.text)
      //   (msgs, ParseResult.empty, ListState(x.toMark, result.ast))
      // }
  }
  object ListState {
  }

  case class TableState(
    parent: DoxLinesParseState,
    blocks: TableState.Blocks,
    annotation: Vector[AnnotationMark] = Vector.empty,
    inSeparator: Boolean = false
  ) extends ChildDoxLinesParseState {
    override protected def end_Transition(config: Config): Transition = {
      val md = close_state(config)
      leave_to_in_end(config, md)
    }

    protected def close_state(config: Config) = {
      println(s"close_state: $annotation")
      val caption = annotation.collect {
        case CaptionAnnotation(title) => Caption(title)
      }.headOption
      println("Caption:" + caption)
      val label = annotation.collect {
        case LabelAnnotation(label) => label
      }.headOption
      println("Label:" + label)
      val head = blocks.head(config)
      val body = blocks.body(config)
      val foot = blocks.foot(config)
      println(s"TableState#close_state: $blocks")
      val table: Dox = Table(head, body, foot, caption, label)
      (ParseMessageSequence.empty, Tree.leaf(table))
    }

    override protected def empty_Transition(config: Config, evt: LogicalLine): Transition = {
      val (m, d) = close_state(config)
      leave_to(m, Vector(d))
    }

    override protected def get_List_Transition(config: Config, evt: LogicalLine): Option[Transition] =
      None // TODO

    override protected def get_Table_Transition(config: Config, evt: LogicalLine): Option[Transition] = {
      val a = TableMark.get(evt).map {
        case m: TableMark.TableSeparator => copy(inSeparator = true)
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

    case class Blocks(blocks: NonEmptyVector[Block]) {
      def add(p: TableSeparator) = Blocks(blocks :+ Block.empty)
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
        case m: TableSeparator => TableState(parent, Blocks.init)
        case m: TableRow => TableState(parent, Blocks(m))
      }

    def apply(parent: DoxLinesParseState, p: TableMark, as: Vector[AnnotationMark]): TableState =
      p match {
        case m: TableSeparator => TableState(parent, Blocks.init, as)
        case m: TableRow => TableState(parent, Blocks(m), as)
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
    println(s"parse_inline: $c, $p")
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
      AnnotationMark.get(evt.text).map(x => transit_next(AnnotationState(parent, annotations :+ x)))

    override protected def text_transition(config: Config, evt: LogicalLineEvent): Transition =
      leave_to_with_warning(config, evt, "Dangling annotations")
  }

  object AnnotationState {
    def apply(
      parent: DoxLinesParseState,
      annotation: AnnotationMark
    ): AnnotationState = AnnotationState(parent, NonEmptyVector(annotation))
  }
}
