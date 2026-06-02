package org.smartdox.service.operations

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import org.goldenport.context.Consequence
import org.goldenport.cli._
import org.goldenport.bag.{ChunkBag, FileBag}
import org.goldenport.io.IoUtils
import org.smartdox.Dox
import org.smartdox.parser.Dox2Parser
import org.smartdox.generator.{Context => GeneratorContext}
import org.smartdox.generators.AntoraGenerator
import org.smartdox.doxsite.DoxSite
import org.smartdox.converters.Dox2AsciidocConverter
import org.smartdox.converters.Dox2LatexConverter
import org.smartdox.transformers.Dox2HtmlTransformer

/*
 * @since   Apr.  9, 2026
 *  version Apr.  9, 2026
 * @version Jun.  3, 2026
 * @author  ASAMI, Tomoharu
 */
case object PdfOperationClass extends OperationClassWithOperation {
  val request = PdfCommand.specification
  val response = PdfResult.specification
  val specification = spec.Operation("pdf", request, response)

  def apply(env: Environment, req: Request): Response = {
    val cmd = PdfCommand.create(req)
    val r = execute(env, cmd)
    FileResponse(r.artifact, r.target.toURI)
  }

  // Single-document PDF generation. Local dependencies are preferred; Docker
  // is used only when requested or when auto mode cannot find a local tool.
  def execute(env: Environment, cmd: PdfCommand): PdfResult = {
    val target = cmd.output.getOrElse(_default_output_file(cmd.in))
    val artifact = cmd.renderer match {
      case PdfRenderer.ChromeHeadless => _execute_chrome(env, cmd)
      case PdfRenderer.Asciidoc => _execute_asciidoc(env, cmd)
      case PdfRenderer.Latex => _execute_latex(env, cmd)
    }
    PdfResult(artifact, target)
  }

  private def _execute_chrome(env: Environment, cmd: PdfCommand): ChunkBag = {
    val ctx = GeneratorContext.create(env)
    val dox = _parse(cmd.in)
    val html = _html(ctx, dox)
    _write_chrome_pdf(cmd, html)
  }

  private def _execute_asciidoc(env: Environment, cmd: PdfCommand): ChunkBag = {
    val ctx = GeneratorContext.create(env)
    val config = DoxSite.Config.create(cmd)
    val dox = _parse(cmd.in)
    val adoc = _asciidoc(ctx, config, dox)
    val adocbag = _text_bag(_asciidoc_filename(cmd.in), adoc)
    val out = _output_bag(cmd.in)
    try {
      _run_asciidoctor_pdf(cmd, adocbag.toFile.toPath, out.toFile.toPath)
    } finally {
      adocbag.dispose()
    }
    out
  }

  private def _execute_latex(env: Environment, cmd: PdfCommand): ChunkBag = {
    val ctx = GeneratorContext.create(env)
    val dox = _parse(cmd.in)
    val workdir = Files.createTempDirectory(_temp_prefix(_latex_filename(cmd.in)))
    val latex = _latex(ctx, cmd, dox, workdir.toFile)
    val texbag = _text_bag_in(workdir, _latex_filename(cmd.in), latex)
    val out = _output_bag(cmd.in)
    try {
      _run_latex_pdf(cmd, texbag.toFile.toPath, out.toFile.toPath)
    } finally {
      texbag.dispose()
      IoUtils.removeDirectory(workdir.toFile)
    }
    out
  }

  private def _parse(in: File): Dox = {
    val text = scala.io.Source.fromFile(in, "UTF-8").mkString
    Dox2Parser.parseWithFilename(Dox2Parser.Config.default, in.getPath, text)
  }

  private def _html(ctx: GeneratorContext, dox: Dox): String = {
    val rule = Dox2HtmlTransformer.Rule.default
    Dox2HtmlTransformer(ctx, rule).transform(dox).take
  }

  private def _asciidoc(ctx: GeneratorContext, config: DoxSite.Config, dox: Dox): String = {
    val actx = AntoraGenerator.Context(ctx, config)
    val cctx = Dox2AsciidocConverter.Context(actx, isDiagramGeneration = true)
    new Dox2AsciidocConverter(cctx).convert(dox).take
  }

  private def _latex(ctx: GeneratorContext, cmd: PdfCommand, dox: Dox, diagramDir: File): String =
    new Dox2LatexConverter(
      cmd.latexEngine,
      cmd.latexFormat,
      cmd.latexDate,
      cmd.latexAffiliation,
      cmd.latexAuthor,
      Some(ctx),
      Some(diagramDir),
      isDiagramGeneration = true
    ).convert(dox).take

  private def _write_chrome_pdf(cmd: PdfCommand, html: String): ChunkBag = {
    val htmlbag = _text_bag(_html_filename(cmd.in), html)
    val out = _output_bag(cmd.in)
    try {
      _print_to_pdf(cmd, htmlbag.toFile.toPath, out.toFile.toPath)
    } finally {
      htmlbag.dispose()
    }
    out
  }

  private def _print_to_pdf(cmd: PdfCommand, html: Path, out: Path): Unit = {
    _local_chrome(cmd) match {
      case Some(chrome) =>
        _run_process(
          Vector(
            chrome.getPath,
            "--headless",
            "--disable-gpu",
            "--no-first-run",
            "--no-default-browser-check",
            "--no-pdf-header-footer",
            "--print-to-pdf-no-header",
            s"--print-to-pdf=${out.toString}",
            html.toUri.toString
          ),
          "PDF generation failed"
        )
      case None =>
        _run_chrome_pdf_in_docker(cmd, html, out)
    }
    if (!Files.exists(out))
      sys.error(s"PDF generation failed: output not found: $out")
  }

  private def _detect_chrome(): File = {
    _detect_chrome_option().getOrElse {
      sys.error("PDF generation requires Chrome/Chromium. Set --chrome <path>, CHROME, or use --dependency-mode docker.")
    }
  }

  private def _detect_chrome_option(): Option[File] = {
    val candidates = Vector(
      sys.env.get("CHROME").map(new File(_)),
      Some(new File("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")),
      Some(new File("/Applications/Chromium.app/Contents/MacOS/Chromium")),
      Some(new File("/usr/bin/google-chrome")),
      Some(new File("/usr/bin/chromium")),
      Some(new File("/usr/bin/chromium-browser")),
      Some(new File("/opt/homebrew/bin/chromium")),
      Some(new File("/usr/local/bin/chromium"))
    ).flatten
    candidates.find(f => f.exists && f.canExecute)
  }

  private def _default_output_file(in: File): File = {
    val body = _filename_body(in.getName)
    new File(s"$body.pdf")
  }

  private def _html_filename(in: File): String =
    s"${_filename_body(in.getName)}.html"

  private def _asciidoc_filename(in: File): String =
    s"${_filename_body(in.getName)}.adoc"

  private def _latex_filename(in: File): String =
    s"${_filename_body(in.getName)}.tex"

  private def _pdf_filename(in: File): String =
    s"${_filename_body(in.getName)}.pdf"

  private def _output_bag(in: File): FileBag = {
    val filename = _pdf_filename(in)
    val path = Files.createTempFile(_temp_prefix(filename), _temp_suffix(filename))
    FileBag.create(path.toFile)
  }

  private def _text_bag(filename: String, content: String): FileBag = {
    val path = Files.createTempFile(_temp_prefix(filename), _temp_suffix(filename))
    val bag = FileBag.create(path.toFile)
    bag.write(content, StandardCharsets.UTF_8)
    bag
  }

  private def _text_bag_in(dir: Path, filename: String, content: String): FileBag = {
    val path = dir.resolve(filename)
    val bag = FileBag.create(path.toFile)
    bag.write(content, StandardCharsets.UTF_8)
    bag
  }

  private def _temp_prefix(filename: String): String = {
    val body = _filename_body(filename).replaceAll("[^A-Za-z0-9._-]", "-")
    val short = if (body.length > 32) body.substring(0, 32) else body
    if (short.length >= 3) short else short.padTo(3, 'x')
  }

  private def _temp_suffix(filename: String): String = {
    val i = filename.lastIndexOf('.')
    if (i >= 0 && i < filename.length - 1) filename.substring(i) else ".tmp"
  }

  private def _filename_body(filename: String): String = {
    val i = filename.lastIndexOf('.')
    if (i <= 0) filename else filename.substring(0, i)
  }

  private def _run_asciidoctor_pdf(cmd: PdfCommand, adoc: Path, out: Path): Unit = {
    _local_asciidoctor_pdf(cmd) match {
      case Some(asciidoctorpdf) =>
        _run_process(
          Vector(
            asciidoctorpdf.getPath,
            "-o",
            out.toString,
            adoc.toString
          ),
          "Asciidoctor PDF generation failed"
        )
      case None =>
        _run_asciidoctor_pdf_in_docker(cmd, adoc, out)
    }
    if (!Files.exists(out))
      sys.error(s"Asciidoctor PDF generation failed: output not found: $out")
  }

  private def _run_latex_pdf(cmd: PdfCommand, tex: Path, out: Path): Unit = {
    _local_latexmk(cmd) match {
      case Some(latexmk) =>
        val engineargs = cmd.latexEngine match {
          case Dox2LatexConverter.Engine.LuaLatex => Vector("-lualatex")
          case Dox2LatexConverter.Engine.UpLatex => Vector("-pdfdvi")
        }
        _run_process(
          Vector(latexmk.getPath) ++
          engineargs ++
          _latexmk_engine_options(cmd.latexEngine) ++
          Vector(
            "-interaction=nonstopmode",
            "-halt-on-error",
            s"-outdir=${out.getParent.toString}",
            tex.toString
          ).filter(_.nonEmpty),
          "LaTeX PDF generation failed"
        )
      case None =>
        _run_latex_pdf_in_docker(cmd, tex, out)
    }
    val generated = _latex_generated_pdf(tex, out)
    if (Files.exists(out) && Files.size(out) > 0) {
      // Docker direct mode writes to the requested FileBag path.
    } else if (Files.exists(generated)) {
      Files.copy(generated, out, StandardCopyOption.REPLACE_EXISTING)
    } else {
      sys.error(s"LaTeX PDF generation failed: output not found: $out")
    }
  }

  private def _latex_generated_pdf(tex: Path, out: Path): Path =
    out.getParent.resolve(s"${_filename_body(tex.getFileName.toString)}.pdf")

  private def _latexmk_engine_options(engine: Dox2LatexConverter.Engine): Vector[String] =
    engine match {
      case Dox2LatexConverter.Engine.LuaLatex => Vector.empty
      case Dox2LatexConverter.Engine.UpLatex => Vector(
        "-latex=uplatex",
        "-e",
        "$dvipdf='dvipdfmx -p a4 %O -o %D %S';"
      )
    }

  private def _detect_asciidoctor_pdf(): File = {
    _detect_asciidoctor_pdf_option().getOrElse {
      sys.error("PDF generation with --renderer asciidoc requires asciidoctor-pdf. Set --asciidoctor-pdf <path>, ASCIIDOCTOR_PDF, or use --dependency-mode docker.")
    }
  }

  private def _detect_asciidoctor_pdf_option(): Option[File] = {
    val candidates = Vector(
      sys.env.get("ASCIIDOCTOR_PDF").map(new File(_)),
      Some(new File("/opt/homebrew/bin/asciidoctor-pdf")),
      Some(new File("/usr/local/bin/asciidoctor-pdf")),
      Some(new File("/usr/bin/asciidoctor-pdf"))
    ).flatten
    candidates.find(f => f.exists && f.canExecute)
  }

  private def _detect_latexmk(): File = {
    _detect_latexmk_option().getOrElse {
      sys.error("PDF generation with --renderer latex requires latexmk with LuaLaTeX or UpLaTeX/dvipdfmx. Set --latexmk <path>, LATEXMK, or use --dependency-mode docker.")
    }
  }

  private def _detect_latexmk_option(): Option[File] = {
    val candidates = Vector(
      sys.env.get("LATEXMK").map(new File(_)),
      Some(new File("/opt/homebrew/bin/latexmk")),
      Some(new File("/usr/local/bin/latexmk")),
      Some(new File("/usr/bin/latexmk"))
    ).flatten
    candidates.find(f => f.exists && f.canExecute)
  }

  private def _local_chrome(cmd: PdfCommand): Option[File] =
    cmd.dependencyMode match {
      case PdfDependencyMode.Docker => None
      case PdfDependencyMode.Local => Some(cmd.chrome.getOrElse(_detect_chrome()))
      case PdfDependencyMode.Auto => cmd.chrome.orElse(_detect_chrome_option())
    }

  private def _local_asciidoctor_pdf(cmd: PdfCommand): Option[File] =
    cmd.dependencyMode match {
      case PdfDependencyMode.Docker => None
      case PdfDependencyMode.Local => Some(cmd.asciidoctorPdf.getOrElse(_detect_asciidoctor_pdf()))
      case PdfDependencyMode.Auto => cmd.asciidoctorPdf.orElse(_detect_asciidoctor_pdf_option())
    }

  private def _local_latexmk(cmd: PdfCommand): Option[File] =
    cmd.dependencyMode match {
      case PdfDependencyMode.Docker => None
      case PdfDependencyMode.Local => Some(cmd.latexmk.getOrElse(_detect_latexmk()))
      case PdfDependencyMode.Auto => cmd.latexmk.orElse(_detect_latexmk_option())
    }

  private def _run_chrome_pdf_in_docker(cmd: PdfCommand, html: Path, out: Path): Unit = {
    val input = _docker_file(html, "input")
    val output = _docker_file(out, "output")
    _run_process(
      _docker_prefix(cmd, input.parent, output.parent) ++ Vector(
        "chromium",
        "--headless",
        "--disable-gpu",
        "--no-sandbox",
        "--no-first-run",
        "--no-default-browser-check",
        "--no-pdf-header-footer",
        "--print-to-pdf-no-header",
        s"--print-to-pdf=${output.containerPath}",
        input.containerUri
      ),
      "Docker Chrome PDF generation failed"
    )
  }

  private def _run_asciidoctor_pdf_in_docker(cmd: PdfCommand, adoc: Path, out: Path): Unit = {
    val input = _docker_file(adoc, "input")
    val output = _docker_file(out, "output")
    _run_process(
      _docker_prefix(cmd, input.parent, output.parent) ++ Vector(
        "asciidoctor-pdf",
        "-a",
        "scripts=cjk",
        "-a",
        "pdf-theme=/opt/smartdox/themes/asciidoctor-pdf-ja.yml",
        "-o",
        output.containerPath,
        input.containerPath
      ),
      "Docker Asciidoctor PDF generation failed"
    )
  }

  private def _run_latex_pdf_in_docker(cmd: PdfCommand, tex: Path, out: Path): Unit = {
    val input = _docker_file(tex, "input")
    val outputdir = Files.createTempDirectory(_temp_prefix(s"${_filename_body(input.name)}-latex-output"))
    val output = DockerFile(outputdir, out.getFileName.toString, "output")
    val dvi = s"${output.parentContainerPath}/${_filename_body(input.name)}.dvi"
    val command = cmd.latexEngine match {
      case Dox2LatexConverter.Engine.LuaLatex =>
        s"cd ${input.parentContainerPath} && lualatex -interaction=nonstopmode -halt-on-error -output-directory=${output.parentContainerPath} ${input.name}"
      case Dox2LatexConverter.Engine.UpLatex =>
        Vector(
          s"cd ${input.parentContainerPath} && uplatex -interaction=nonstopmode -halt-on-error -output-directory=${output.parentContainerPath} ${input.name}",
          s"dvipdfmx -p a4 -o ${output.containerPath} $dvi"
        ).mkString(" && ")
    }
    try {
      _run_process(
        _docker_prefix(cmd, input.parent, output.parent) ++ Vector(
          "sh",
          "-c",
          command
        ),
        "Docker LaTeX PDF generation failed"
      )
      val generated = outputdir.resolve(s"${_filename_body(input.name)}.pdf")
      if (Files.exists(generated))
        Files.copy(generated, out, StandardCopyOption.REPLACE_EXISTING)
    } finally {
      IoUtils.removeDirectory(outputdir.toFile)
    }
  }

  private def _docker_prefix(cmd: PdfCommand, inputdir: Path, outputdir: Path): Vector[String] =
    Vector(
      cmd.docker.map(_.getPath).getOrElse("docker"),
      "run",
      "--rm",
      "-v",
      s"${inputdir.toString}:/work/input:ro",
      "-v",
      s"${outputdir.toString}:/work/output",
      "-w",
      "/work",
      cmd.dockerImage.getOrElse(PdfCommand.defaultDockerImage)
    )

  private def _docker_file(path: Path, containerRoot: String): DockerFile = {
    val absolute = path.toAbsolutePath.normalize
    DockerFile(
      absolute.getParent,
      absolute.getFileName.toString,
      containerRoot
    )
  }

  private def _run_process(args: Vector[String], error: String): Unit = {
    val process = new ProcessBuilder(args: _*).redirectErrorStream(true).start()
    val message = scala.io.Source.fromInputStream(process.getInputStream, "UTF-8").mkString
    val code = process.waitFor()
    if (code != 0)
      sys.error(s"$error: ${message.trim}")
  }

  private case class DockerFile(parent: Path, name: String, containerRoot: String) {
    def containerPath: String =
      s"/work/$containerRoot/$name"

    def parentContainerPath: String =
      s"/work/$containerRoot"

    def containerUri: String =
      s"file://$containerPath"
  }

  case class PdfCommand(
    siteParameters: SiteParameters,
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
    renderer: PdfRenderer
  ) extends Command with SiteParameters.Holder {
  }

  object PdfCommand {
    val defaultDockerImage = "simplemodeling/smartdox-pdf:latest"

    object params {
      val output = spec.Parameter.propertyFileOption("output")
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
    }

    def create(req: Request): PdfCommand =
      cCreate(req).take

    def cCreate(req: Request): Consequence[PdfCommand] =
      for {
        sp <- SiteParameters.createC(req)
        output <- req.cFileOption(params.output)
        chrome <- req.cFileOption(params.chrome)
        asciidoctorpdf <- req.cFileOption(params.asciidoctorPdf)
      } yield {
        PdfCommand(
          sp,
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
          PdfRenderer.create(_string_option(req, "renderer"))
        )
      }

    def specification: spec.Request = spec.Request(
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
      p.map(_.trim.toLowerCase).filter(_.nonEmpty) match {
        case None => Latex
        case Some("chrome") | Some("chrome-headless") | Some("headless-chrome") => ChromeHeadless
        case Some("asciidoc") | Some("asciidoctor") | Some("asciidoctor-pdf") => Asciidoc
        case Some("latex") | Some("tex") | Some("uplatex") | Some("lualatex") | Some("latexmk") => Latex
        case Some(s) => sys.error(s"Unsupported PDF renderer: $s")
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
