package org.smartdox.service.operations

import java.io.File
import java.nio.file.Path
import org.goldenport.context.Consequence
import org.goldenport.cli._
import org.goldenport.bag.ChunkBag
import org.smartdox.Dox
import org.smartdox.generator.{Context => GeneratorContext}
import org.smartdox.generators.AntoraGenerator
import org.smartdox.doxsite.DoxSite
import org.smartdox.converters.Dox2AsciidocConverter
import org.smartdox.converters.Dox2LatexConverter
import org.smartdox.diagnostics.{StructuredRenderingDiagnostic, StructuredRenderingDiagnosticException}
import org.smartdox.transformers.Dox2HtmlTransformer

/*
 * @since   Apr.  9, 2026
 *  version Jun.  3, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
case object PdfOperationClass extends OperationClassWithOperation {
  val request = PdfCommand.specification
  val response = PdfResult.specification
  val specification = spec.Operation("pdf", request, response)

  def apply(env: Environment, req: Request): Response = {
    val cmd = PdfCommand.create(req)
    val selector = _locale_selector_or_throw(req)
    val r = _execute(env, cmd, selector)
    FileResponse(r.artifact, r.target.toURI)
  }

  // Single-document PDF generation. Local dependencies are preferred; Docker
  // is used only when requested or when auto mode cannot find a local tool.
  def execute(env: Environment, cmd: PdfCommand): PdfResult =
    _execute(env, cmd, None)

  private def _execute(
    env: Environment,
    cmd: PdfCommand,
    selector: Option[String],
    diagramrenderer: Option[Dox2LatexConverter.DiagramRenderer] = None
  ): PdfResult = {
    val target = cmd.output.getOrElse(PdfRendererInvocation.defaultOutputFile(cmd.in))
    try {
      val artifact = cmd.renderer match {
        case PdfRenderer.ChromeHeadless => _execute_chrome(env, cmd, selector)
        case PdfRenderer.Asciidoc => _execute_asciidoc(env, cmd, selector)
        case PdfRenderer.Latex => _execute_latex(env, cmd, selector, diagramrenderer)
      }
      PdfResult(artifact, target)
    } catch {
      case failure: PdfRendererExecution.PdfTypesettingProcessFailure =>
        throw new StructuredRenderingDiagnosticException(
          PdfRendererExecution.typesettingFailureDiagnostic(cmd, failure)
        )
    }
  }

  private def _execute_chrome(
    env: Environment,
    cmd: PdfCommand,
    selector: Option[String]
  ): ChunkBag = {
    val ctx = GeneratorContext.create(env)
    val input = _parse_pdf_input(cmd.in)
    val dox = _resolve_site_links(
      ctx,
      cmd,
      _select_locale_or_throw(input.dox, selector, cmd.in.getPath),
      selector
    ).take
    val html = _html(ctx, dox)
    val workspace = _prepare_renderer_workspace(
      PdfRenderer.ChromeHeadless,
      PdfRendererInvocation.htmlFilename(cmd.in),
      html,
      dox,
      input.resourceroot
    )
    try {
      val out = PdfRendererInvocation.outputBag(cmd.in)
      PdfRendererInvocation.printToPdf(cmd, workspace.input, out.toFile.toPath)
      out
    } finally {
      workspace.dispose()
    }
  }

  private def _execute_asciidoc(
    env: Environment,
    cmd: PdfCommand,
    selector: Option[String]
  ): ChunkBag = {
    val ctx = GeneratorContext.create(env)
    val config = DoxSite.Config.create(cmd)
    val input = _parse_pdf_input(cmd.in)
    val dox = _resolve_site_links(
      ctx,
      cmd,
      _select_locale_or_throw(input.dox, selector, cmd.in.getPath),
      selector
    ).take
    val adoc = _asciidoc(ctx, config, dox)
    val workspace = _prepare_renderer_workspace(
      PdfRenderer.Asciidoc,
      PdfRendererInvocation.asciidocFilename(cmd.in),
      adoc,
      dox,
      input.resourceroot
    )
    try {
      val out = PdfRendererInvocation.outputBag(cmd.in)
      PdfRendererInvocation.runAsciidoctorPdf(cmd, workspace.input, out.toFile.toPath)
      out
    } finally {
      workspace.dispose()
    }
  }

  private def _execute_latex(
    env: Environment,
    cmd: PdfCommand,
    selector: Option[String]
  ): ChunkBag =
    _execute_latex(env, cmd, selector, None)

  private[operations] def _execute_latex_with_diagram_renderer(
    env: Environment,
    cmd: PdfCommand,
    diagramrenderer: Dox2LatexConverter.DiagramRenderer
  ): PdfResult =
    _execute(env, cmd, None, Some(diagramrenderer))

  private def _execute_latex(
    env: Environment,
    cmd: PdfCommand,
    selector: Option[String],
    diagramrenderer: Option[Dox2LatexConverter.DiagramRenderer]
  ): ChunkBag = {
    val ctx = GeneratorContext.create(env)
    val input = _parse_pdf_input(cmd.in)
    val dox = _resolve_site_links(
      ctx,
      cmd,
      _select_locale_or_throw(input.dox, selector, cmd.in.getPath),
      selector
    ).take
    val workdir = PdfOperationInputWorkspace.createRendererDirectory(
      PdfRendererInvocation.latexFilename(cmd.in)
    )
    try {
      val latex = PdfRendererExecution.latex(
        ctx,
        cmd,
        dox,
        workdir.toFile,
        input.resourceroot,
        diagramrenderer
      )
      val texbag = PdfOperationInputWorkspace.writeRendererInput(
        workdir,
        PdfRendererInvocation.latexFilename(cmd.in),
        latex
      )
      val out = PdfRendererInvocation.outputBag(cmd.in)
      try {
        PdfRendererInvocation.runLatexPdf(cmd, texbag.toFile.toPath, out.toFile.toPath)
      } finally {
        texbag.dispose()
      }
      out
    } finally {
      PdfOperationInputWorkspace.dispose(workdir)
    }
  }

  private[operations] case class ParsedPdfInput(dox: Dox, resourceroot: File)

  private[operations] case class RendererWorkspace(directory: Path, input: Path) {
    def dispose(): Unit = PdfOperationInputWorkspace.dispose(directory)
  }

  private[operations] def _parse_pdf_input(in: File): ParsedPdfInput =
    PdfOperationInputWorkspace.parse(in)

  private[operations] def _prepare_renderer_workspace(
    renderer: PdfRenderer,
    filename: String,
    content: String,
    dox: Dox,
    resourceroot: File
  ): RendererWorkspace =
    PdfOperationInputWorkspace.prepare(renderer, filename, content, dox, resourceroot)

  private def _parse_selected(in: File, selector: Option[String]): Dox =
    _select_locale(_parse_pdf_input(in).dox, selector).take

  private[operations] def _select_locale(dox: Dox, selector: Option[String]): Consequence[Dox] =
    PdfOperationSiteProjection.selectLocale(dox, selector)

  private[operations] def _locale_selector(req: Request): Consequence[Option[String]] =
    PdfOperationSiteProjection.localeSelector(req)

  private def _locale_selector_or_throw(req: Request): Option[String] =
    PdfOperationSiteProjection.localeSelectorOrThrow(req)

  private def _select_locale_or_throw(
    dox: Dox,
    selector: Option[String],
    sourceidentity: String
  ): Dox =
    PdfOperationSiteProjection.selectLocaleOrThrow(dox, selector, sourceidentity)

  private[operations] def _resolve_site_links(
    context: GeneratorContext,
    cmd: PdfCommand,
    dox: Dox,
    selector: Option[String]
  ): Consequence[Dox] =
    PdfOperationSiteProjection.resolveSiteLinks(context, cmd, dox, selector)

  private def _html(ctx: GeneratorContext, dox: Dox): String = {
    val rule = Dox2HtmlTransformer.Rule.default
    Dox2HtmlTransformer(ctx, rule).transform(dox).take
  }

  private def _asciidoc(ctx: GeneratorContext, config: DoxSite.Config, dox: Dox): String = {
    val actx = AntoraGenerator.Context(ctx, config)
    val cctx = Dox2AsciidocConverter.Context(actx, isDiagramGeneration = true)
    new Dox2AsciidocConverter(cctx).convert(dox).take
  }

  case class PdfCommand(
    siteParameters: SiteParameters,
    siteRoot: Option[File],
    siteConfig: Option[File],
    output: Option[File],
    chrome: Option[File],
    asciidoctorPdf: Option[File],
    latexmk: Option[File],
    latexEngine: Dox2LatexConverter.Engine,
    latexFormat: Dox2LatexConverter.Format,
    latexDate: Option[String],
    latexAffiliation: Option[String],
    latexAuthor: Option[String],
    dependencyMode: PdfDependencyMode,
    docker: Option[File],
    dockerImage: Option[String],
    renderer: PdfRenderer,
    typesettingTimeoutMillis: Long = PdfCommand.defaultTypesettingTimeoutMillis
  ) extends Command with SiteParameters.Holder {
  }

  object PdfCommand {
    val defaultDockerImage = "simplemodeling/smartdox-pdf:latest"
    val defaultTypesettingTimeoutMillis = 300000L

    object params {
      val output = spec.Parameter.propertyFileOption("output")
      val siteRoot = spec.Parameter.propertyFileOption("site-root")
      val siteConfig = spec.Parameter.propertyFileOption("site-config")
      val chrome = spec.Parameter.propertyFileOption("chrome")
      val asciidoctorPdf = spec.Parameter.propertyFileOption("asciidoctor-pdf")
      val latexmk = spec.Parameter.propertyFileOption("latexmk")
      val latexEngine = spec.Parameter.property("latex-engine")
      val latexFormat = spec.Parameter.property("latex-format")
      val latexDate = spec.Parameter.property("latex-date")
      val latexAffiliation = spec.Parameter.property("latex-affiliation")
      val latexAuthor = spec.Parameter.property("latex-author")
      val dependencyMode = spec.Parameter.property("dependency-mode")
      val docker = spec.Parameter.propertyFileOption("docker")
      val dockerImage = spec.Parameter.property("docker-image")
      val renderer = spec.Parameter.property("renderer")
      val typesettingTimeoutMillis = spec.Parameter.property("typesetting-timeout-ms")
      val locale = spec.Parameter(
        "locale",
        spec.Parameter.PropertyKind,
        spec.XString,
        spec.Multiplicity.ZeroOne
      )
    }

    def create(req: Request): PdfCommand =
      cCreate(req).take

    def cCreate(req: Request): Consequence[PdfCommand] =
      for {
        sp <- SiteParameters.createC(req)
        siteroot <- req.cFileOption(params.siteRoot)
        siteconfig <- req.cFileOption(params.siteConfig)
        output <- req.cFileOption(params.output)
        chrome <- req.cFileOption(params.chrome)
        asciidoctorpdf <- req.cFileOption(params.asciidoctorPdf)
      } yield {
        PdfCommand(
          sp,
          _file_option(req, "site-root", siteroot),
          _file_option(req, "site-config", siteconfig),
          _file_option(req, "output", output),
          _file_option(req, "chrome", chrome),
          _file_option(req, "asciidoctor-pdf", asciidoctorpdf),
          _file_option(req, "latexmk", None),
          Dox2LatexConverter.Engine.create(_string_option(req, "latex-engine")),
          Dox2LatexConverter.Format.create(_string_option(req, "latex-format")),
          _string_option(req, "latex-date"),
          _string_option(req, "latex-affiliation"),
          _string_option(req, "latex-author"),
          PdfDependencyMode.create(_string_option(req, "dependency-mode")),
          _file_option(req, "docker", None),
          _string_option(req, "docker-image"),
          PdfRenderer.create(_string_option(req, "renderer")),
          _typesetting_timeout_millis(req)
        )
      }

    def specification: spec.Request = spec.Request(
      params.siteRoot,
      params.siteConfig,
      params.output,
      params.chrome,
      params.asciidoctorPdf,
      params.latexmk,
      params.latexEngine,
      params.latexFormat,
      params.latexDate,
      params.latexAffiliation,
      params.latexAuthor,
      params.dependencyMode,
      params.docker,
      params.dockerImage,
      params.renderer,
      params.typesettingTimeoutMillis,
      params.locale,
      SiteParameters.params.in,
      SiteParameters.params.publication,
      SiteParameters.params.strategy,
      SiteParameters.params.outputScopePolicy,
      SiteParameters.params.target
    )

    private def _file_option(req: Request, name: String, parsed: Option[File]): Option[File] =
      parsed.orElse(_string_option(req, name).map(new File(_)))

    private def _string_option(req: Request, name: String): Option[String] =
      req.getPropertyString(name)

    private def _typesetting_timeout_millis(req: Request): Long =
      _string_option(req, "typesetting-timeout-ms") match {
        case None => defaultTypesettingTimeoutMillis
        case Some(timeout) if timeout.matches("[0-9]+") =>
          try {
            val millis = timeout.toLong
            if (millis > 0L) millis else _invalid_typesetting_timeout(timeout)
          } catch {
            case _: NumberFormatException => _invalid_typesetting_timeout(timeout)
          }
        case Some(timeout) => _invalid_typesetting_timeout(timeout)
      }

    private def _invalid_typesetting_timeout(timeout: String): Nothing =
      throw new StructuredRenderingDiagnosticException(
        StructuredRenderingDiagnostic.pdfTypesettingTimeoutInvalid(timeout)
      )

  }

  case class PdfResult(
    artifact: ChunkBag,
    target: File
  ) extends Result {
  }
  object PdfResult {
    def specification: spec.Response = spec.Response(spec.XFile)
  }

  sealed trait PdfRenderer
  object PdfRenderer {
    case object ChromeHeadless extends PdfRenderer
    case object Asciidoc extends PdfRenderer
    case object Latex extends PdfRenderer

    def create(p: Option[String]): PdfRenderer =
      p.map(value => value -> value.trim.toLowerCase).filter(_._2.nonEmpty) match {
        case None => Latex
        case Some((_, "chrome")) | Some((_, "chrome-headless")) | Some((_, "headless-chrome")) => ChromeHeadless
        case Some((_, "asciidoc")) | Some((_, "asciidoctor")) | Some((_, "asciidoctor-pdf")) => Asciidoc
        case Some((_, "latex")) | Some((_, "tex")) | Some((_, "uplatex")) | Some((_, "lualatex")) | Some((_, "latexmk")) => Latex
        case Some((tokencontext, _)) =>
          throw new StructuredRenderingDiagnosticException(
            StructuredRenderingDiagnostic.pdfRendererUnsupported(tokencontext)
          )
      }
  }

  sealed trait PdfDependencyMode
  object PdfDependencyMode {
    case object Auto extends PdfDependencyMode
    case object Local extends PdfDependencyMode
    case object Docker extends PdfDependencyMode

    def create(p: Option[String]): PdfDependencyMode =
      p.map(_.trim.toLowerCase).filter(_.nonEmpty) match {
        case None | Some("auto") => Auto
        case Some("local") => Local
        case Some("docker") | Some("container") => Docker
        case Some(s) => sys.error(s"Unsupported PDF dependency mode: $s")
      }
  }
}
