package org.smartdox.service.operations

import java.io.File
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}
import org.goldenport.bag.{ChunkBag, FileBag}
import org.goldenport.io.IoUtils
import org.smartdox.converters.Dox2LatexConverter

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
private[operations] object PdfRendererInvocation {
  def defaultOutputFile(in: File): File = {
    val body = _filename_body(in.getName)
    new File(s"$body.pdf")
  }

  def htmlFilename(in: File): String =
    s"${_filename_body(in.getName)}.html"

  def asciidocFilename(in: File): String =
    s"${_filename_body(in.getName)}.adoc"

  def latexFilename(in: File): String =
    s"${_filename_body(in.getName)}.tex"

  def outputBag(in: File): FileBag = {
    val filename = _pdf_filename(in)
    val path = Files.createTempFile(tempPrefix(filename), _temp_suffix(filename))
    FileBag.create(path.toFile)
  }

  private def _text_bag(filename: String, content: String): FileBag = {
    val path = Files.createTempFile(tempPrefix(filename), _temp_suffix(filename))
    val bag = FileBag.create(path.toFile)
    bag.write(content, java.nio.charset.StandardCharsets.UTF_8)
    bag
  }

  private def _has_pdf_output(out: Path): Boolean =
    Files.isRegularFile(out, LinkOption.NOFOLLOW_LINKS) && Files.size(out) > 0

  private def _write_chrome_pdf(cmd: PdfOperationClass.PdfCommand, html: String): ChunkBag = {
    val htmlbag = _text_bag(htmlFilename(cmd.in), html)
    val out = outputBag(cmd.in)
    try {
      printToPdf(cmd, htmlbag.toFile.toPath, out.toFile.toPath)
    } finally {
      htmlbag.dispose()
    }
    out
  }

  def printToPdf(cmd: PdfOperationClass.PdfCommand, html: Path, out: Path): Unit = {
    _local_chrome(cmd) match {
      case Some(chrome) =>
        PdfRendererExecution.runProcess(
          cmd,
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
          )
        )
      case None =>
        _run_chrome_pdf_in_docker(cmd, html, out)
    }
    if (!_has_pdf_output(out))
      throw PdfRendererExecution.PdfTypesettingOutputMissing()
  }

  def runAsciidoctorPdf(cmd: PdfOperationClass.PdfCommand, adoc: Path, out: Path): Unit = {
    _local_asciidoctor_pdf(cmd) match {
      case Some(asciidoctorpdf) =>
        PdfRendererExecution.runProcess(
          cmd,
          Vector(
            asciidoctorpdf.getPath,
            "-o",
            out.toString,
            adoc.toString
          )
        )
      case None =>
        _run_asciidoctor_pdf_in_docker(cmd, adoc, out)
    }
    if (!_has_pdf_output(out))
      throw PdfRendererExecution.PdfTypesettingOutputMissing()
  }

  def runLatexPdf(cmd: PdfOperationClass.PdfCommand, tex: Path, out: Path): Unit = {
    _local_latexmk(cmd) match {
      case Some(latexmk) =>
        val engineargs = cmd.latexEngine match {
          case Dox2LatexConverter.Engine.LuaLatex => Vector("-lualatex")
          case Dox2LatexConverter.Engine.UpLatex => Vector("-pdfdvi")
        }
        PdfRendererExecution.runProcess(
          cmd,
          Vector(_local_latexmk_command(latexmk)) ++
          engineargs ++
          _latexmk_engine_options(cmd.latexEngine) ++
          Vector(
            "-interaction=nonstopmode",
            "-halt-on-error",
            s"-outdir=${out.getParent.toString}",
            tex.toString
          ).filter(_.nonEmpty),
          Some(tex.getParent)
        )
      case None =>
        _run_latex_pdf_in_docker(cmd, tex, out)
    }
    val generated = _latex_generated_pdf(tex, out)
    if (_has_pdf_output(out)) {
      // Docker direct mode writes to the requested FileBag path.
    } else if (_has_pdf_output(generated)) {
      Files.copy(generated, out, StandardCopyOption.REPLACE_EXISTING)
    } else {
      throw PdfRendererExecution.PdfTypesettingOutputMissing()
    }
  }

  private def _local_chrome(cmd: PdfOperationClass.PdfCommand): Option[File] =
    cmd.dependencyMode match {
      case PdfOperationClass.PdfDependencyMode.Docker => None
      case PdfOperationClass.PdfDependencyMode.Local => Some(cmd.chrome.getOrElse(_detect_chrome()))
      case PdfOperationClass.PdfDependencyMode.Auto => cmd.chrome.orElse(_detect_chrome_option())
    }

  private def _local_asciidoctor_pdf(cmd: PdfOperationClass.PdfCommand): Option[File] =
    cmd.dependencyMode match {
      case PdfOperationClass.PdfDependencyMode.Docker => None
      case PdfOperationClass.PdfDependencyMode.Local => Some(cmd.asciidoctorPdf.getOrElse(_detect_asciidoctor_pdf()))
      case PdfOperationClass.PdfDependencyMode.Auto => cmd.asciidoctorPdf.orElse(_detect_asciidoctor_pdf_option())
    }

  private def _local_latexmk(cmd: PdfOperationClass.PdfCommand): Option[File] =
    cmd.dependencyMode match {
      case PdfOperationClass.PdfDependencyMode.Docker => None
      case PdfOperationClass.PdfDependencyMode.Local => Some(cmd.latexmk.getOrElse(_detect_latexmk()))
      case PdfOperationClass.PdfDependencyMode.Auto => cmd.latexmk.orElse(_detect_latexmk_option())
    }

  def tempPrefix(filename: String): String = {
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

  private def _pdf_filename(in: File): String =
    s"${_filename_body(in.getName)}.pdf"

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

  private def _run_chrome_pdf_in_docker(cmd: PdfOperationClass.PdfCommand, html: Path, out: Path): Unit = {
    val input = PdfRendererExecution.dockerFile(html, "input")
    val output = PdfRendererExecution.dockerFile(out, "output")
    PdfRendererExecution.runProcess(
      cmd,
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
      )
    )
  }

  private def _run_asciidoctor_pdf_in_docker(cmd: PdfOperationClass.PdfCommand, adoc: Path, out: Path): Unit = {
    val input = PdfRendererExecution.dockerFile(adoc, "input")
    val output = PdfRendererExecution.dockerFile(out, "output")
    PdfRendererExecution.runProcess(
      cmd,
      _docker_prefix(cmd, input.parent, output.parent) ++ Vector(
        "asciidoctor-pdf",
        "-a",
        "scripts=cjk",
        "-a",
        "pdf-theme=/opt/smartdox/themes/asciidoctor-pdf-ja.yml",
        "-o",
        output.containerPath,
        input.containerPath
      )
    )
  }

  private def _run_latex_pdf_in_docker(cmd: PdfOperationClass.PdfCommand, tex: Path, out: Path): Unit = {
    val input = PdfRendererExecution.dockerFile(tex, "input")
    val outputdir = Files.createTempDirectory(tempPrefix(s"${_filename_body(input.name)}-latex-output"))
    val output = PdfRendererExecution.DockerFile(outputdir, out.getFileName.toString, "output")
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
      PdfRendererExecution.runProcess(
        cmd,
        _docker_prefix(cmd, input.parent, output.parent) ++ Vector(
          "sh",
          "-c",
          command
        )
      )
      val generated = outputdir.resolve(s"${_filename_body(input.name)}.pdf")
      if (_has_pdf_output(generated))
        Files.copy(generated, out, StandardCopyOption.REPLACE_EXISTING)
    } finally {
      IoUtils.removeDirectory(outputdir.toFile)
    }
  }

  private def _docker_prefix(
    cmd: PdfOperationClass.PdfCommand,
    inputdir: Path,
    outputdir: Path
  ): Vector[String] =
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
      cmd.dockerImage.getOrElse(PdfOperationClass.PdfCommand.defaultDockerImage)
    )
}
