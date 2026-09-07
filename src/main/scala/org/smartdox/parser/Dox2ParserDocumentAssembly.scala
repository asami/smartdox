package org.smartdox.parser

import scalaz._, Scalaz._, Validation._
import com.typesafe.config.{Config => Hocon}
import org.goldenport.collection.VectorMap
import org.goldenport.context.DateTimeContext
import org.goldenport.i18n.I18NElement
import org.goldenport.parser._
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.metadata.DocumentPropertiesParser
import org.smartdox.metadata.Explanation
import Dox._
import Dox2Parser._

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
private[parser] final class Dox2ParserDocumentAssembly(context: ParseContext) {
  private implicit val _date_time_context: DateTimeContext = context.dateTimeContext

  def apply(blocks: LogicalBlocks): ParseResult[Document] = {
    val xs = _blocks(context, blocks)
    // println(s"Dox2Parser#apply ${blocks} => $xs")
    val a = _create(xs)
    if (context.isResolve)
      a.map(_resolve)
    else
      a
  }

  private def _create(ps: Vector[Dox]) = {
    case class Z(
      head: Head = Head(),
      elements: Vector[Dox] = Vector.empty
    ) {
      def r = {
        val desc = _distill_brief(elements)
        val (h, xs) = desc match {
          case Some(s) =>
            val h = head.withSummaryIfRequired(s)
            (h, elements.toList)
          case None =>
            val lead = head.metadata.getEffectiveLead
            val xs = lead.toList.flatMap(_.makeParagraphs) ++ elements
            (head, xs)
        }
        ParseSuccess(Document(h, Body(xs)))
      }

      def +(rhs: Dox) = rhs match {
        case m: Head => copy(head = head.merge(m))
        case m: Section if _is_head(m) =>
          val meta = _parse_head(m)
          copy(head = head.merge(meta))
        case m => copy(elements = elements :+ m)
      }
    }
    ps.foldLeft(Z())(_+_).r
  }

  private def _is_head(p: Section) = p.titleName.trim == "HEAD"

  private def _distill_brief(ps: Vector[Dox]): Option[List[Inline]] = {
    val a = ps.toStream.collect {
      case m: Paragraph => m
    }.headOption
    a match {
      case Some(s) => Some(List(I18NFragment.create(Dox.toInlineContents(s))))
      case None => None
    }
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
    context.config.style match {
      case Config.DoxStyle.SmartDox => _section_smartdox(ctx, p)
      case Config.DoxStyle.Markdown => if (_is_head(p)) _head(p) else Vector(_section(ctx, p))
      case Config.DoxStyle.OrgMode => Vector(_section(ctx, p))
    }

  private def _section_smartdox(
    ctx: ParseContext,
    p: LogicalSection
  ): Vector[Dox] =
    if (_is_head(p))
      _head(p)
    else p.mark match {
      case Some("=") =>
        val (blocks, meta0) = _distill_logical_meta(p.blocks)
        val xs = _blocks(ctx, blocks)
        val meta = meta0.withTitle(List(_to_dox(p.title)))
        val head = Head(metadata = meta)
        head +: xs
      case _ => Vector(_section(ctx.levelUp, p))
    }

  private def _is_head(p: LogicalSection) = p.keyForModel == "head"

  private def _head(p: LogicalSection): Vector[Dox] = {
    val parsed = _parse_head(p)
    val head = Head(metadata = parsed.metadata)
    parsed.errorMessage.fold(Vector[Dox](head))(x =>
      Vector[Dox](head, DiagnosticBlock.error("SmartDox HEAD metadata parse error", x, "HEAD source", parsed.source))
    )
  }

  private case class HeadParseResult(
    metadata: DocumentMetaData,
    errorMessage: Option[String] = None,
    source: String = ""
  )

  // private def _section_head(
  //   p: LogicalSection,
  //   xs: Vector[Dox]
  // ): (Vector[Dox], DocumentMetaData) = {
  //   val title = List(_to_dox(p.title))
  //   val (xs1, props) = _distill_props(xs)
  //   val meta0 = props.fold(DocumentMetaData.empty)(DocumentMetaData.create(_))
  //   val meta = meta0.withTitle(title)
  //   (xs1, meta)
  // }

  private def _section_head(
    p: LogicalSection,
    xs: Vector[Dox]
  ): (Vector[Dox], DocumentMetaData) = {
    val title = List(_to_dox(p.title))
    val (xs1, meta0) = _distill_meta(xs)
    val meta = meta0.withTitle(title)
    (xs1, meta)
  }

  private def _parse_head(p: Section): DocumentMetaData = {
    val (_, meta) = _distill_meta(p.contents.toVector)
    val a = for {
      ex <- Explanation.parse(p)
      updatehistory <- DocumentMetaData.UpdateHistory.parse(p)
      relations <- DocumentMetaData.Relations.parse(p)
    } yield DocumentMetaData.create(ex, updatehistory, relations)
    meta + a.take
  }

  private def _parse_head(p: LogicalSection): HeadParseResult = {
    val propertiestext = _logical_section_properties_text(p)
    val (propertiesmeta, errormessage) = DocumentPropertiesParser.parse(propertiestext).fold(
      c => {
        val message = s"SmartDox HEAD metadata parse error: ${c.message}"
        scala.Console.err.println(message)
        (DocumentMetaData.empty, Some(message))
      },
      hocon => (DocumentMetaData.create(hocon), None)
    )
    val section = _head_explanation_section(p)
    val a = for {
      ex <- Explanation.parse(section)
      updatehistory <- DocumentMetaData.UpdateHistory.parse(section)
      relations <- DocumentMetaData.Relations.parse(section)
    } yield DocumentMetaData.create(ex, updatehistory, relations)
    HeadParseResult(propertiesmeta + a.take, errormessage, propertiestext)
  }

  private def _logical_section_properties_text(p: LogicalSection): String =
    p.blocks.blocks.headOption.collect {
      case m: LogicalParagraph => _logical_paragraph_properties_text(m)
    }.getOrElse("")

  private def _head_explanation_section(p: LogicalSection): Section = {
    val blocks = p.blocks.blocks match {
      case Vector(_: LogicalParagraph, xs @ _*) => LogicalBlocks(xs.toVector)
      case _ => p.blocks
    }
    Section(List(_to_dox(p.title)), _blocks(context.levelUp, blocks).toList, context.level)
  }

  private def _distill_logical_meta(p: LogicalBlocks): (LogicalBlocks, DocumentMetaData) =
    p.blocks match {
      case Vector(x: LogicalParagraph, xs @ _*) =>
        val text = _logical_paragraph_properties_text(x)
        if (DocumentPropertiesParser.isPropertiesText(text)) {
          val meta = DocumentPropertiesParser.parse(text).
            map(DocumentMetaData.create(_)).
            getOrElse(DocumentMetaData.empty)
          (LogicalBlocks(xs.toVector), meta)
        } else {
          (p, DocumentMetaData.empty)
        }
      case _ => (p, DocumentMetaData.empty)
    }

  private def _logical_paragraph_properties_text(p: LogicalParagraph): String =
    p.lines.lines.flatMap(_.physicalLines).mkString("\n")

  private def _distill_meta(ps: Vector[Dox]): (Vector[Dox], DocumentMetaData) = {
    val (xs, props) = _distill_props(ps)
    val meta = props.fold(DocumentMetaData.empty)(DocumentMetaData.create(_))
    (xs, meta)
   }

  private def _distill_props(ps: Vector[Dox]): (Vector[Dox], Option[Hocon]) =
    ps match {
      case Vector() => (ps, None)
      case Vector(x, xs @ _*) =>
        _parse_properties(x) match {
          case Right(r) => r match {
            case Some(s) => (xs.toVector, Some(s))
            case None => (ps, None)
          }
          case Left(l) =>
            val r = Paragraph(List(Text(l))) +: xs
            (r.toVector, None)
        }
    }

  private def _parse_properties(p: Dox): Either[String, Option[Hocon]] =
    _get_text_data_in_simple_paragraph(p) match {
      case Some(s) =>
        DocumentPropertiesParser.parse(s).
          map(Some.apply).
          toEitherString
      case None => Right(None)
    }

  private def _get_text_data_in_simple_paragraph(p: Dox): Option[String] = p match {
    case m: Paragraph =>
      val data = m.toData
      if (_is_property_data(data))
        Some(data)
      else
        None
    case _ => None
  }

  private def _is_property_data(p: String): Boolean = {
    val lines = _property_candidate_lines(p)
    lines.nonEmpty && lines.forall(_is_property_line)
  }

  private def _property_candidate_lines(p: String): Vector[String] =
    p.linesIterator.map(_.trim).filterNot(x => x.isEmpty || x.startsWith("#")).toVector

  private def _is_property_line(p: String): Boolean = {
    val i = p.indexOf('=')
    i > 0 && {
      val key = p.substring(0, i).trim
      key.nonEmpty && key.forall(c => c.isLetterOrDigit || c == '_' || c == '-' || c == '.')
    }
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

  private def _to_dox(p: I18NElement) = toInline(context.config, p) // Text(p.toI18NString.en) // TODO

  private def _to_list(p: Dox): List[Dox] = p match {
    case m: Div => _normalize(m.contents)
    case m: Span => _normalize(m.contents)
    case m => List(m)
  }

  private def _normalize(ps: List[Dox]): List[Dox] = ps.flatMap(_to_list)

  private def _paragraph(ctx: ParseContext, p: LogicalParagraph): Dox = {
    DoxLinesParser.parse(ctx.config.linesConfig._with_source_identity(ctx.config.file), p)
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
    val kind = p.getKind
    val cs = p.lines.text
    val attrs: Map[String, String] = VectorMap.create("kind" -> kind)
    Program.create(cs, attrs, p.location)
  }

  private def _resolve(p: Document): Document = {
    val ttc = context.treeTransformerContext
    val drc = DoxResolver.Context(context)
    val resolver = new Resolver(ttc, drc)
    Dox.transformDocument(p, resolver)
  }
}
