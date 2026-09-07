package org.smartdox.parser

import scalaz._, Scalaz._, Validation._, Tree._
import java.net.URI
import java.util.Locale
import com.typesafe.config.{Config => Hocon}
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
 *  version Jun. 23, 2026
 *  version Aug. 30, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
class Dox2Parser(context: Dox2Parser.ParseContext) {
  import Dox2Parser._

  private val _assembly = new Dox2ParserDocumentAssembly(context)

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
    _assembly(blocks)
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

    def withResourceRoot(p: java.nio.file.Path): Config =
      copy(linesConfig = linesConfig.withInlineConfig(linesConfig.inlineConfig.withResourceRoot(p)))

    private[smartdox] def _with_virtual_resource_parent(parent: String): Config =
      copy(linesConfig = linesConfig.withInlineConfig(
        linesConfig.inlineConfig._with_virtual_resource_parent(parent)
      ))

    def withoutComplementParagraph() = copy(linesConfig = linesConfig.withoutComplementParagraph())

    def withDoxStyle(p: Config.DoxStyle): Config =
      p match {
        case Config.DoxStyle.SmartDox => copy(
          linesConfig = linesConfig.withInlineConfig(_inline_config(DoxInlineParser.Config.smartdox)),
          style = Config.DoxStyle.SmartDox
        )
        case Config.DoxStyle.Markdown => copy(
          linesConfig = linesConfig.withInlineConfig(_inline_config(DoxInlineParser.Config.markdown)),
          style = Config.DoxStyle.Markdown
        )
        case Config.DoxStyle.OrgMode => copy(
          linesConfig = linesConfig.withInlineConfig(_inline_config(DoxInlineParser.Config.orgmode)),
          style = Config.DoxStyle.OrgMode
        )
      }

    def withFilename(filename: String): Config =
      Config.doxStyleForFilename(filename).
        fold(this)(withDoxStyle).
        withPathname(filename)

    private def _inline_config(config: DoxInlineParser.Config): DoxInlineParser.Config =
      config._with_resource_origin(linesConfig.inlineConfig._resource_origin_context)
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
    val linesConfig = LogicalLines.Config.easyHtml.copy(
      useDoubleQuote = true,
      useBackQuote = true
    )
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
    val (body, frontmatter) = Dox2ParserFrontMatter.split(config, in)
    val ctx = ParseContext.now(config)
    val parser = new Dox2Parser(ctx)
    val result = parser.apply(body)
    result match {
      case ParseSuccess(dox, _) => frontmatter.fold(dox)(Dox2ParserFrontMatter.merge(dox, _))
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
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
      val dox = DoxLinesParser.parse(config.linesConfig._with_source_identity(config.file), p)
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
