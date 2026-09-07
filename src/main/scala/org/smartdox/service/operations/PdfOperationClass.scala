package org.smartdox.service.operations

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.Locale
import org.goldenport.context.Consequence
import org.goldenport.cli._
import org.goldenport.bag.{ChunkBag, FileBag}
import org.goldenport.i18n.I18NContext
import org.goldenport.tree.TreeTransformer
import org.goldenport.io.IoUtils
import org.smartdox.{Dox, Hyperlink, ReferenceImg}
import org.smartdox.parser.Dox2Parser
import org.smartdox.generator.{Context => GeneratorContext}
import org.smartdox.generators.AntoraGenerator
import org.smartdox.doxsite.{DoxSite, SitePublicationContext}
import org.smartdox.converters.Dox2AsciidocConverter
import org.smartdox.converters.Dox2LatexConverter
import org.smartdox.transformers.Dox2HtmlTransformer
import org.smartdox.transformers.LanguageFilterTransformer

/*
 * @since   Apr.  9, 2026
 *  version Jun.  3, 2026
 * @version Sep.  7, 2026
 * @author  ASAMI, Tomoharu
 */
case object PdfOperationClass extends OperationClassWithOperation {
  val request = PdfCommand.specification
  val response = PdfResult.specification
  val specification = spec.Operation("pdf", request, response)

  def apply(env: Environment, req: Request): Response = {
    val cmd = PdfCommand.create(req)
    val selector = _locale_selector(req).take
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
    selector: Option[String]
  ): PdfResult = {
    val target = cmd.output.getOrElse(_default_output_file(cmd.in))
    val artifact = cmd.renderer match {
      case PdfRenderer.ChromeHeadless => _execute_chrome(env, cmd, selector)
      case PdfRenderer.Asciidoc => _execute_asciidoc(env, cmd, selector)
      case PdfRenderer.Latex => _execute_latex(env, cmd, selector)
    }
    PdfResult(artifact, target)
  }

  private def _execute_chrome(
    env: Environment,
    cmd: PdfCommand,
    selector: Option[String]
  ): ChunkBag = {
    val ctx = GeneratorContext.create(env)
    val input = _parse_pdf_input(cmd.in)
    val dox = _resolve_site_links(ctx, cmd, _select_locale(input.dox, selector).take, selector).take
    val html = _html(ctx, dox)
    val workspace = _prepare_renderer_workspace(
      PdfRenderer.ChromeHeadless,
      _html_filename(cmd.in),
      html,
      dox,
      input.resourceroot
    )
    try {
      val out = _output_bag(cmd.in)
      _print_to_pdf(cmd, workspace.input, out.toFile.toPath)
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
    val dox = _resolve_site_links(ctx, cmd, _select_locale(input.dox, selector).take, selector).take
    val adoc = _asciidoc(ctx, config, dox)
    val workspace = _prepare_renderer_workspace(
      PdfRenderer.Asciidoc,
      _asciidoc_filename(cmd.in),
      adoc,
      dox,
      input.resourceroot
    )
    try {
      val out = _output_bag(cmd.in)
      _run_asciidoctor_pdf(cmd, workspace.input, out.toFile.toPath)
      out
    } finally {
      workspace.dispose()
    }
  }

  private def _execute_latex(
    env: Environment,
    cmd: PdfCommand,
    selector: Option[String]
  ): ChunkBag = {
    val ctx = GeneratorContext.create(env)
    val input = _parse_pdf_input(cmd.in)
    val dox = _resolve_site_links(ctx, cmd, _select_locale(input.dox, selector).take, selector).take
    val workdir = Files.createTempDirectory(_temp_prefix(_latex_filename(cmd.in)))
    try {
      val latex = _latex(ctx, cmd, dox, workdir.toFile, input.resourceroot)
      val texbag = _text_bag_in(workdir, _latex_filename(cmd.in), latex)
      val out = _output_bag(cmd.in)
      try {
        _run_latex_pdf(cmd, texbag.toFile.toPath, out.toFile.toPath)
      } finally {
        texbag.dispose()
      }
      out
    } finally {
      IoUtils.removeDirectory(workdir.toFile)
    }
  }

  private[operations] case class ParsedPdfInput(dox: Dox, resourceroot: File)

  private[operations] case class RendererWorkspace(directory: Path, input: Path) {
    def dispose(): Unit = IoUtils.removeDirectory(directory.toFile)
  }

  private[operations] def _parse_pdf_input(in: File): ParsedPdfInput = {
    val file = in.getCanonicalFile
    val root = Option(file.getParentFile).getOrElse(
      throw new IllegalArgumentException(s"PDF input has no parent directory: ${file.getPath}")
    )
    val text = scala.io.Source.fromFile(file, "UTF-8").mkString
    val config = Dox2Parser.Config.default.withResourceRoot(root.toPath)
    ParsedPdfInput(Dox2Parser.parseWithFilename(config, file.getPath, text), root)
  }

  private[operations] def _prepare_renderer_workspace(
    renderer: PdfRenderer,
    filename: String,
    content: String,
    dox: Dox,
    resourceroot: File
  ): RendererWorkspace = {
    val workspace = Files.createTempDirectory(_temp_prefix(filename))
    try {
      val input = workspace.resolve(filename)
      Files.write(input, content.getBytes(StandardCharsets.UTF_8))
      _stage_root_relative_images(renderer, workspace, dox, resourceroot)
      RendererWorkspace(workspace, input)
    } catch {
      case e: Throwable =>
        IoUtils.removeDirectory(workspace.toFile)
        throw e
    }
  }

  private def _stage_root_relative_images(
    renderer: PdfRenderer,
    workspace: Path,
    dox: Dox,
    resourceroot: File
  ): Unit = {
    val rootpath = resourceroot.getCanonicalFile.toPath
    _root_relative_reference_images(dox).foreach { image =>
      val sourcepath = _root_relative_image_source(image, rootpath)
      val targetpath = workspace.resolve(_renderer_image_target(renderer, image)).normalize
      if (!targetpath.startsWith(workspace))
        throw new IllegalArgumentException(
          s"image.local.outside-resource-root: ${_image_context(image, targetpath)} root=$workspace"
        )
      Option(targetpath.getParent).foreach(parent => Files.createDirectories(parent))
      Files.copy(sourcepath, targetpath, StandardCopyOption.REPLACE_EXISTING)
    }
  }

  private def _root_relative_reference_images(dox: Dox): Vector[ReferenceImg] = {
    def _collect_(node: Dox): Vector[ReferenceImg] = {
      val here = node match {
        case image: ReferenceImg if _is_root_relative_reference_image(image) => Vector(image)
        case _ => Vector.empty
      }
      here ++ node.elements.toVector.flatMap(_collect_)
    }
    _collect_(dox)
  }

  private def _is_root_relative_reference_image(image: ReferenceImg): Boolean = {
    val uri = image.src
    !uri.isAbsolute &&
    uri.getRawAuthority == null &&
    uri.getRawQuery == null &&
    uri.getRawFragment == null &&
    Option(uri.getPath).exists(_.nonEmpty)
  }

  private def _root_relative_image_source(image: ReferenceImg, rootpath: Path): Path = {
    val sourcepath = try {
      Paths.get(Option(image.src.getPath).getOrElse(""))
    } catch {
      case _: java.nio.file.InvalidPathException =>
        throw new IllegalArgumentException(s"image.local.invalid-resource: ${_image_context(image, rootpath)}")
    }
    if (sourcepath.isAbsolute)
      throw new IllegalArgumentException(
        s"image.local.outside-resource-root: ${_image_context(image, sourcepath)} root=$rootpath"
      )
    val candidate = rootpath.resolve(sourcepath).normalize
    if (!candidate.startsWith(rootpath))
      throw new IllegalArgumentException(
        s"image.local.outside-resource-root: ${_image_context(image, candidate)} root=$rootpath"
      )
    if (!Files.isRegularFile(candidate))
      _missing_image_resource(image, candidate)
    val canonical = candidate.toRealPath()
    if (!canonical.startsWith(rootpath))
      throw new IllegalArgumentException(
        s"image.local.outside-resource-root: ${_image_context(image, canonical)} root=$rootpath"
      )
    canonical
  }

  private def _renderer_image_target(renderer: PdfRenderer, image: ReferenceImg): Path =
    renderer match {
      case PdfRenderer.ChromeHeadless => Paths.get(image.src.getPath)
      case PdfRenderer.Asciidoc => _asciidoc_image_target(image)
      case PdfRenderer.Latex =>
        throw new IllegalArgumentException(s"PDF renderer workspace is unsupported for: $renderer")
    }

  private def _asciidoc_image_target(image: ReferenceImg): Path = {
    val source = image.src.toString
    val target =
      if (source.startsWith("images/")) {
        val rest = source.substring("images/".length)
        val i = rest.indexOf('/')
        if (i > 0)
          s"${rest.substring(0, i)}:${rest.substring(i + 1)}"
        else
          rest
      } else {
        source
      }
    Paths.get(target)
  }

  private def _missing_image_resource(image: ReferenceImg, path: Path): Nothing =
    throw new IllegalArgumentException(s"image.local.missing-resource: ${_image_context(image, path)}")

  private def _image_context(image: ReferenceImg, path: Path): String = {
    val location = image.location.map(_.toString).getOrElse("<absent>")
    s"location=$location source=${image.src} path=$path"
  }

  private def _parse_selected(in: File, selector: Option[String]): Dox =
    _select_locale(_parse_pdf_input(in).dox, selector).take

  private[operations] def _select_locale(dox: Dox, selector: Option[String]): Consequence[Dox] =
    selector match {
      case None => Consequence.success(dox)
      case Some(value) =>
        _locale(value).flatMap { locale =>
          if (LanguageFilterTransformer._has_exact_locale(dox, locale)) {
            val context = TreeTransformer.Context.default[Dox].
              withI18NContext(I18NContext.default.withLocale(locale))
            Consequence.execute {
              Dox.transform(dox, LanguageFilterTransformer._strict(context))
            }
          } else {
            Consequence.resourceNotFound[Dox](s"pdf.locale.unavailable: $value")
          }
        }
    }

  private def _locale(value: String): Consequence[Locale] = {
    val locale = Locale.forLanguageTag(value)
    if (value.isEmpty || value.trim != value || locale.toLanguageTag != value)
      Consequence.invalidArgumentFault(s"pdf.locale.invalid: $value")
    else if (value == "ja" || value == "en")
      Consequence.success(locale)
    else
      Consequence.unsupportedOperation(s"pdf.locale.unsupported: $value")
  }

  private[operations] def _locale_selector(req: Request): Consequence[Option[String]] =
    req.getPropertyString("locale") match {
      case None => Consequence.success(None)
      case Some(value) => _locale(value).map(_ => Some(value))
    }

  private[operations] def _resolve_site_links(
    context: GeneratorContext,
    cmd: PdfCommand,
    dox: Dox,
    selector: Option[String]
  ): Consequence[Dox] =
    if (_site_links(dox).isEmpty)
      Consequence.success(dox)
    else {
      val publication = (cmd.siteRoot, cmd.siteConfig) match {
        case (Some(root), Some(config)) =>
          SitePublicationContext.create(root, config, cmd.in, context)
        case _ =>
          Consequence.invalidArgumentFault[SitePublicationContext](
            "pdf.site-context.missing: --site-root and --site-config are required for site:[...] links"
          )
      }
      publication.flatMap { sitecontext =>
        val locale = selector match {
          case Some(value) => SitePublicationContext.locale(value)
          case None => sitecontext.defaultLocale
        }
        locale.flatMap { selectedlocale =>
          sitecontext.resolveDox(dox, selectedlocale)
        }
      }
    }

  private def _site_links(dox: Dox): Vector[Hyperlink] = {
    val here = dox match {
      case link: Hyperlink if link.isSite => Vector(link)
      case _ => Vector.empty
    }
    here ++ dox.elements.toVector.flatMap(_site_links)
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

  private def _latex(
    ctx: GeneratorContext,
    cmd: PdfCommand,
    dox: Dox,
    diagramdir: File,
    resourceroot: File
  ): String =
    new Dox2LatexConverter(
      cmd.latexEngine,
      cmd.latexFormat,
      cmd.latexDate,
      cmd.latexAffiliation,
      cmd.latexAuthor,
      Some(ctx),
      Some(diagramdir),
      isDiagramGeneration = true,
      resourceBaseDir = Some(resourceroot)
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
          Vector(_local_latexmk_command(latexmk)) ++
          engineargs ++
          _latexmk_engine_options(cmd.latexEngine) ++
          Vector(
            "-interaction=nonstopmode",
            "-halt-on-error",
            s"-outdir=${out.getParent.toString}",
            tex.toString
          ).filter(_.nonEmpty),
          "LaTeX PDF generation failed",
          Some(tex.getParent)
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

  private def _local_latexmk_command(latexmk: File): String = {
    val command = latexmk.getPath
    if (latexmk.isAbsolute || !_is_path_bearing_command(command))
      command
    else
      latexmk.toPath.toAbsolutePath.normalize.toString
  }

  private def _is_path_bearing_command(command: String): Boolean =
    command.indexOf('/') >= 0 || command.indexOf('\\') >= 0

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

  private def _run_process(
    args: Vector[String],
    error: String,
    workingdirectory: Option[Path] = None
  ): Unit = {
    val builder = new ProcessBuilder(args: _*).redirectErrorStream(true)
    workingdirectory.foreach(path => builder.directory(path.toFile))
    val process = builder.start()
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
    renderer: PdfRenderer
  ) extends Command with SiteParameters.Holder {
  }

  object PdfCommand {
    val defaultDockerImage = "simplemodeling/smartdox-pdf:latest"

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
          PdfRenderer.create(_string_option(req, "renderer"))
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
