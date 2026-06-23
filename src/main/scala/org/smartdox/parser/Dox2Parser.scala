package org.smartdox.parser

import scalaz._, Scalaz._, Validation._, Tree._
import java.net.URI
import java.util.Locale
import java.text.SimpleDateFormat
import scala.collection.JavaConverters._
import com.typesafe.config.{Config => Hocon, ConfigFactory}
import org.goldenport.RAISE
import org.goldenport.context._
import org.goldenport.parser._
import org.goldenport.collection.VectorMap
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NElement
import org.goldenport.i18n.LocaleUtils
import org.goldenport.io.InputSource
import org.goldenport.io.FileTextResolver
import org.goldenport.tree._
import org.goldenport.util.VectorUtils
import org.goldenport.util.StringUtils
import org.goldenport.util.ExceptionUtils
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.metadata.DocumentPropertiesParser
import org.smartdox.metadata.Explanation
import org.smartdox.transformer._
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
 *  version Oct. 24, 2024
 *  version Nov. 23, 2024
 *  version Jan.  1, 2025
 *  version Feb.  7, 2025
 *  version Mar.  2, 2025
 *  version Apr.  6, 2025
 *  version May. 24, 2025
 *  version Jun. 24, 2025
 *  version Jul. 29, 2025
 *  version Aug. 18, 2025
 *  version Sep. 15, 2025
 *  version Oct. 26, 2025
 *  version Nov. 17, 2025
 *  version Dec. 10, 2025
 *  version Jun.  8, 2026
 * @version Jun. 23, 2026
 * @author  ASAMI, Tomoharu
 */
class Dox2Parser(context: Dox2Parser.ParseContext) {
  import Dox2Parser._

  def config = context.config
  implicit def dateTimeContext = context.dateTimeContext

  def apply(s: String): ParseResult[Document] = {
    val blocks = LogicalBlocks.parse(config.blocksConfig, s)
    apply(blocks)
  }

  def apply(in: LogicalBlock): ParseResult[Document] = {
    val blocks = LogicalBlocks(in)
    apply(blocks)
  }

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
    config.style match {
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

  private def _to_dox(p: I18NElement) = toInline(config, p) // Text(p.toI18NString.en) // TODO

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

object Dox2Parser {
  type Transition = (ParseMessageSequence, ParseResult[Dox], DoxSectionParseState)

  case class Config(
    isDebug: Boolean,
    isLocation: Boolean,
    blocksConfig: LogicalBlocks.Config,
    linesConfig: DoxLinesParser.Config,
    file: Option[URI],
    style: Config.DoxStyle
  ) extends ParseConfig {
    def isAutoI18n: Boolean = true // AutoI18nTransformer
    def autoI18nDelimiter = "｜"
    def autoI18nLanguages = List(LocaleUtils.en, LocaleUtils.ja)
    def isResolve: Boolean = true

    lazy val fileTextResolverContext: FileTextResolver.Context = {
      val c = FileTextResolver.Context.default
      file.fold(c)(c.withBaseFile)
    }

    def withPathname(pathname: String) = {
      val pn = StringUtils.toRelative(pathname)
      copy(file = Some(new URI(pn)))
    }

    def withInlineConfig(p: DoxInlineParser.Config) =
      copy(linesConfig = linesConfig.withInlineConfig(p))

    def withoutComplementParagraph() = copy(linesConfig = linesConfig.withoutComplementParagraph())

    def withDoxStyle(p: Config.DoxStyle): Config =
      p match {
        case Config.DoxStyle.SmartDox => copy(
          linesConfig = linesConfig.withInlineConfig(DoxInlineParser.Config.smartdox),
          style = Config.DoxStyle.SmartDox
        )
        case Config.DoxStyle.Markdown => copy(
          linesConfig = linesConfig.withInlineConfig(DoxInlineParser.Config.markdown),
          style = Config.DoxStyle.Markdown
        )
        case Config.DoxStyle.OrgMode => copy(
          linesConfig = linesConfig.withInlineConfig(DoxInlineParser.Config.orgmode),
          style = Config.DoxStyle.OrgMode
        )
      }

    def withFilename(filename: String): Config =
      Config.doxStyleForFilename(filename).
        fold(this)(withDoxStyle).
        withPathname(filename)
  }
  object Config {
    import DoxLinesParser.{Config => _, _}

    sealed trait DoxStyle {
    }
    object DoxStyle {
      case object SmartDox extends DoxStyle
      case object Markdown extends DoxStyle
      case object OrgMode extends DoxStyle

      val default = SmartDox
    }

    val verbatims = Vector(
      BeginSrcAnnotationClass,
      BeginExampleAnnotationClass,
      GenericBeginAnnotationClass
    )
    val linesConfig = LogicalLines.Config.easyHtml.copy(useBackQuote = true)
    val blocksConfig = LogicalBlocks.Config.easyHtml.
      addVerbatims(verbatims).
      withLinesConfig(linesConfig)
    val default = Config(
      false,
      true,
      Config.blocksConfig,
      DoxLinesParser.Config.default,
      None,
      DoxStyle.SmartDox
    )
    val debug = default.copy(true)
    val smartdox = default.copy(
      linesConfig = DoxLinesParser.Config.smartdox,
      style = DoxStyle.SmartDox
    )
    val orgmodeInline = default.copy(
      linesConfig = DoxLinesParser.Config.orgmode,
      style = DoxStyle.OrgMode
    )
    val orgmode = orgmodeInline
    val markdown = default.copy(
      linesConfig = DoxLinesParser.Config.markdown,
      style = DoxStyle.Markdown
    )
    val literateModel = default.copy(
      blocksConfig = LogicalBlocks.Config.literateModel,
      linesConfig = DoxLinesParser.Config.literateModel
    )

    def doxStyleForFilename(filename: String): Option[DoxStyle] =
      StringUtils.getSuffix(filename).flatMap {
        case "dox" => Some(DoxStyle.SmartDox)
        case "org" => Some(DoxStyle.OrgMode)
        case "md" => Some(DoxStyle.Markdown)
        case "markdown" => Some(DoxStyle.Markdown)
        case _ => None
      }
  }

  case class ParseContext(
    config: Config,
    dateTimeContext: DateTimeContext,
    level: Int = 0,
    treeTransformerContext: TreeTransformer.Context[Dox] = TreeTransformer.Context.default,
    fileTextResolverContextOption: Option[FileTextResolver.Context] = None
  ) {
    lazy val fileTextResolverContext = fileTextResolverContextOption getOrElse config.fileTextResolverContext
    def fileResolverContext = fileTextResolverContext.fileResolverContext

    def levelUp = copy(level = level + 1)

    def isAutoI18n: Boolean = config.isAutoI18n // AutoI18nTransformer
    def isResolve: Boolean = config.isResolve

    def withParameters(params: FileTextResolver.Parameters) =
      copy(fileTextResolverContextOption = Some(fileTextResolverContext.withParameters(params)))
  }
  object ParseContext {
    def now(): ParseContext = now(Config.default)

    def now(c: Config): ParseContext = ParseContext(
      c,
      DateTimeContext.now()
    )
  }

  def create(c: Dox2Parser.Config): Dox2Parser =
    new Dox2Parser(ParseContext.now(c))

  def parse(in: String): Dox = parse(Config.default, in)

  def parseWithFilename(filename: String, in: String): Dox =
    parseWithFilename(Config.default, filename, in)

  def parseWithFilename(config: Config, filename: String, in: String): Dox =
    parse(config.withFilename(filename), in)

  def parse(config: Config, in: String): Dox = {
    val (body, frontmatter) = _take_markdown_front_matter(config, in)
    val ctx = ParseContext.now(config)
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(body)
    result match {
      case ParseSuccess(dox, _) => frontmatter.fold(dox)(_merge_metadata(dox, _))
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  private def _take_markdown_front_matter(config: Config, in: String): (String, Option[DocumentMetaData]) =
    if (config.style != Config.DoxStyle.Markdown)
      (in, None)
    else {
      val lines = Option(in).getOrElse("").linesIterator.toVector
      if (lines.headOption.exists(_.trim == "---")) {
        lines.zipWithIndex.drop(1).find(_._1.trim == "---") match {
          case Some((_, end)) =>
            val yaml = lines.slice(1, end).mkString("\n")
            val body = lines.drop(end + 1).mkString("\n")
            implicit val dtctx: DateTimeContext = DateTimeContext.now()
            val metadata = _parse_markdown_front_matter(yaml)
            (body, metadata)
          case None => (in, None)
        }
      } else {
        (in, None)
      }
    }


  private def _parse_markdown_front_matter(yaml: String)(implicit ctx: DateTimeContext): Option[DocumentMetaData] =
    Option(new org.yaml.snakeyaml.Yaml().load[Any](Option(yaml).getOrElse(""))).collect {
      case m: java.util.Map[_, _] =>
        val normalized = new java.util.LinkedHashMap[String, AnyRef]()
        m.asScala.foreach { case (key, value) =>
          normalized.put(key.toString, _normalize_yaml_value(value))
        }
        DocumentMetaData.create(ConfigFactory.parseMap(normalized))
    }

  private def _normalize_yaml_value(value: Any): AnyRef =
    value match {
      case null => ""
      case m: java.util.Map[_, _] =>
        val normalized = new java.util.LinkedHashMap[String, AnyRef]()
        m.asScala.foreach { case (key, value) =>
          normalized.put(key.toString, _normalize_yaml_value(value))
        }
        normalized
      case xs: java.util.List[_] =>
        xs.asScala.map(x => _normalize_yaml_value(x)).asJava
      case d: java.util.Date =>
        new SimpleDateFormat("yyyy-MM-dd").format(d)
      case v: java.lang.Boolean => v
      case v: java.lang.Number => v
      case v: String => v
      case v => v.toString
    }

  private def _merge_metadata(dox: Document, metadata: DocumentMetaData): Document =
    dox match {
      case Document(head, body, foot, attributes, location) =>
        Document(head.merge(metadata), body, foot, attributes, location)
    }

  def parse(config: Config, in: LogicalBlock): Dox = {
    val ctx = ParseContext.now(config)
    parse(ctx, in)
  }

  def parse(ctx: ParseContext, in: LogicalBlock): Dox = {
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseOne(in: String): Dox = parseOne(Config.default, in)

  def parseOne(config: Config, in: String): Dox = {
    val ctx = ParseContext.now(config)
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox.body.elements match {
        case Nil => EmptyDox
        case x :: Nil => x
        case xs => Fragment(xs) // SyntaxErrorFault("${xs.mkstring}").RAISE
      }
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseFragment(config: Config, in: String): Fragment = {
    val ctx = ParseContext.now(config)
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => Fragment(dox.body.elements)
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseI18NFragmentOptionC(in: String): Consequence[Option[I18NFragment]] =
    Consequence {
      if (in.isEmpty)
        None
      else
        Some(parseI18NFragment(in))
    }

  def parseI18NFragmentOption(in: String): Option[I18NFragment] =
    if (in.isEmpty)
      None
    else
      Some(parseI18NFragment(in))

  def parseI18NFragmentC(in: String): Consequence[I18NFragment] = Consequence {
    parseI18NFragment(in)
  }

  def parseI18NFragment(in: String): I18NFragment = {
    val config = Config.default
    parseI18NFragment(config, in)
  }

  def parseI18NFragment(config: Config, in: String): I18NFragment = {
    if (config.isAutoI18n && in.contains(config.autoI18nDelimiter)) {
      val (en, ja) = _make_en_ja(config, in)
      I18NFragment.enja(en, ja)
    } else {
      I18NFragment.create(parseFragment(config, in))
    }
  }

  def parse(config: Config, in: LogicalBlocks): Dox =
    parse(ParseContext.now(config), in)

  def parse(ctx: ParseContext, in: LogicalBlocks): Dox = {
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(in)
    result match {
      case ParseSuccess(dox, _) => dox
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseSection(config: Config, in: LogicalSection): Section =
    parseSection(ParseContext.now(config), in)

  def parseSection(ctx: ParseContext, in: LogicalSection): Section = {
    val parser = new Dox2Parser(ctx)
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
      val title = List(toInline(config, p.title))
      val section = Section(title, dox, level)
      (ParseMessageSequence.empty, ParseSuccess(Dox.empty), copy(body = body :+ section))
    }

//    private def _to_dox(p: I18NElement) = Text(p.toI18NString.en) // TODO

    private def _to_list(config: Config, p: Dox): List[Dox] = p match {
      case m: Div => _normalize(config, m.contents)
      case m: Span => _normalize(config, m.contents)
      case m: Text => toInlines(config, m)
      case m => List(m)
    }

    private def _normalize(config: Config, ps: List[Dox]): List[Dox] = ps.flatMap(_to_list(config, _))

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

  class Resolver(
    val treeTransformerContext: TreeTransformer.Context[Dox],
    val resolverContext: DoxResolver.Context
  ) extends DoxHomoTreeTransformer {
    private val _resolver = new DoxResolver(resolverContext)

    override protected def make_Node(
      node: TreeNode[Dox],
      content: Dox
    ): TreeTransformer.Directive[Dox] = content match {
      case m: Include =>
        val a = _resolver.resolve(m.directive).foldConclusion(Error(_))
        directive_node(a)
      case m => directive_default
    }
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

  def toInlines(config: Config, p: I18NElement): List[Inline] =
    toInline(config, p) match {
      case m: Fragment => m.toInlines
      case m: I18NFragment => m.makeInlines
      case m => List(m)
    }

  def toInline(config: Config, p: I18NElement): Inline = {
    val s = p.toI18NString
    val a = if (config.isAutoI18n) {
      if (s.c.contains(config.autoI18nDelimiter)) {
        val (en, ja) = _make_en_ja(config, s.c)
        s.localeMap + (LocaleUtils.en -> en, LocaleUtils.ja -> ja)
      } else {
        s.localeMap
      }
    } else {
      s.localeMap
    }
    _to_inline(a)
  }

  private def _make_en_ja(config: Config, p: String) = {
    val a = p.split(config.autoI18nDelimiter).toList
    a match {
      case Nil => (p, p)
      case x :: Nil => (x, x)
      case x :: y :: _ => (x, y)
    }
  }

  private def _to_inline(p: Map[Locale, String]): Inline = {
    val minimumscope = p.keySet.forall {
      case LocaleUtils.C => true
      case LocaleUtils.en => true
      case LocaleUtils.ja => true
      case _ => false
    }
    if (minimumscope) {
      (p.get(LocaleUtils.en), p.get(LocaleUtils.ja)) match {
        case (Some(en), Some(ja)) =>
          if (en == ja) {
            Text(en)
          } else {
            // I18NFragment.enja(List(Text(en)), List(Text(ja)))
            I18NFragment.enja(en, ja)
          }
        case (Some(en), None) => Text(en)
        case (None, Some(ja)) => Text(ja)
        case (None, None) => EmptyDox
      }
    } else {
      I18NFragment.createString(p)
    }
  }

  def toInlines(config: Config, s: Text): List[Inline] = {
    val a = _get_inlines(config, s.contents)
    a getOrElse List(s)
  }

  def toInlines(config: Config, s: String): List[Inline] = {
    val a = _get_inlines(config, s)
    a getOrElse List(Text(s))
  }

  private def _get_inlines(config: Config, s: String): Option[List[Inline]] =
    if (config.isAutoI18n) {
      if (s.contains(config.autoI18nDelimiter)) {
        val (en, ja) = _make_en_ja(config, s)
        Some(List(Span.createEn(en), Span.createJa(ja)))
      } else {
        None
      }
    } else {
      None
    }

  def errorDocument(title: String, e: Throwable): Document = {
    val s = s"""Error: ${title}
=====

status=error


```
Exception :: ${ExceptionUtils.showName(e)}
Message :: ${ExceptionUtils.showMessage(e)}
```

```
${StringUtils.makeStack(e)}
```

"""
    Dox.toDocument(parse(s))
  }
}
