package org.smartdox.service.operations

import java.io.File
import java.nio.file.Path
import java.util.concurrent.TimeUnit
import org.smartdox.Dox
import org.smartdox.converters.Dox2LatexConverter
import org.smartdox.diagnostics.{StructuredRenderingDiagnostic, StructuredRenderingDiagnosticException}
import org.smartdox.generator.{Context => GeneratorContext}

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
private[operations] object PdfRendererExecution {
  def latex(
    context: GeneratorContext,
    command: PdfOperationClass.PdfCommand,
    dox: Dox,
    diagramDir: File,
    resourceRoot: File,
    diagramRenderer: Option[Dox2LatexConverter.DiagramRenderer]
  ): String = {
    val converter = new PdfDox2LatexConverter(
      command,
      context,
      diagramDir,
      resourceRoot,
      diagramRenderer
    )
    val latex = converter.convert(dox).take
    converter.diagramGenerationFailure.foreach { failure =>
      throw new StructuredRenderingDiagnosticException(
        StructuredRenderingDiagnostic.pdfDiagramGenerationFailed(
          sourceIdentity = command.in.getPath,
          tokenContext = failure.kind
        )
      )
    }
    latex
  }

  def runProcess(
    command: PdfOperationClass.PdfCommand,
    arguments: Vector[String],
    workingDirectory: Option[Path] = None
  ): Unit = {
    val builder = new ProcessBuilder(arguments: _*)
      .redirectErrorStream(true)
      .redirectOutput(ProcessBuilder.Redirect.DISCARD)
    workingDirectory.foreach(path => builder.directory(path.toFile))
    val process =
      try builder.start()
      catch {
        case _: java.io.IOException => throw PdfTypesettingProcessStartFailed()
      }
    val completed =
      try process.waitFor(command.typesettingTimeoutMillis, TimeUnit.MILLISECONDS)
      catch {
        case e: InterruptedException =>
          Thread.currentThread.interrupt()
          throw e
      }
    if (!completed) {
      _terminate_process_tree(process)
      throw PdfTypesettingTimeout()
    }
    if (process.exitValue() != 0)
      throw PdfTypesettingNonzeroExit()
  }

  def dockerFile(path: Path, containerRoot: String): DockerFile = {
    val absolute = path.toAbsolutePath.normalize
    DockerFile(absolute.getParent, absolute.getFileName.toString, containerRoot)
  }

  def typesettingFailureDiagnostic(
    command: PdfOperationClass.PdfCommand,
    failure: PdfTypesettingProcessFailure
  ): StructuredRenderingDiagnostic =
    failure match {
      case _: PdfTypesettingProcessStartFailed =>
        StructuredRenderingDiagnostic.pdfTypesettingProcessStartFailed(
          sourceIdentity = command.in.getPath,
          tokenContext = _canonical_renderer_token(command.renderer)
        )
      case _: PdfTypesettingNonzeroExit =>
        StructuredRenderingDiagnostic.pdfTypesettingNonzeroExit(
          sourceIdentity = command.in.getPath,
          tokenContext = _canonical_renderer_token(command.renderer)
        )
      case _: PdfTypesettingOutputMissing =>
        StructuredRenderingDiagnostic.pdfTypesettingOutputMissing(
          sourceIdentity = command.in.getPath,
          tokenContext = _canonical_renderer_token(command.renderer)
        )
      case _: PdfTypesettingTimeout =>
        StructuredRenderingDiagnostic.pdfTypesettingTimeout(
          sourceIdentity = command.in.getPath,
          tokenContext = _canonical_renderer_token(command.renderer)
        )
    }

  private def _terminate_process_tree(process: Process): Unit = {
    val handles = _process_tree_handles(process)
    handles.foreach(_destroy_process)
    val interrupted = _await_process_tree_termination(handles, _process_termination_grace_millis)
    handles.foreach(_force_destroy_process)
    val forceinterrupted = _await_process_tree_termination(handles)
    if (interrupted || forceinterrupted) {
      Thread.currentThread.interrupt()
      throw new InterruptedException()
    }
  }

  private def _process_tree_handles(process: Process): Vector[ProcessHandle] = {
    val stream = process.toHandle.descendants()
    try {
      val iterator = stream.iterator()
      val builder = Vector.newBuilder[ProcessHandle]
      while (iterator.hasNext)
        builder += iterator.next()
      builder.result().reverse :+ process.toHandle
    } finally {
      stream.close()
    }
  }

  private def _destroy_process(handle: ProcessHandle): Unit =
    if (handle.isAlive)
      handle.destroy()

  private def _force_destroy_process(handle: ProcessHandle): Unit =
    if (handle.isAlive)
      handle.destroyForcibly()

  private def _await_process_tree_termination(
    handles: Vector[ProcessHandle],
    timeoutmillis: Long
  ): Boolean = {
    val deadline = System.nanoTime + TimeUnit.MILLISECONDS.toNanos(timeoutmillis)
    var interrupted = false
    while (handles.exists(_.isAlive) && System.nanoTime < deadline) {
      val remainingnanos = deadline - System.nanoTime
      if (remainingnanos > 0L) {
        val waitnanos = math.min(
          remainingnanos,
          TimeUnit.MILLISECONDS.toNanos(_process_termination_poll_millis)
        )
        try TimeUnit.NANOSECONDS.sleep(waitnanos)
        catch {
          case _: InterruptedException => interrupted = true
        }
      }
    }
    interrupted
  }

  private def _await_process_tree_termination(handles: Vector[ProcessHandle]): Boolean = {
    var interrupted = false
    while (handles.exists(_.isAlive)) {
      try TimeUnit.MILLISECONDS.sleep(_process_termination_poll_millis)
      catch {
        case _: InterruptedException => interrupted = true
      }
    }
    interrupted
  }

  private final case class PdfDiagramGenerationFailure(
    kind: String,
    source: String,
    error: Throwable
  )

  private final class PdfDox2LatexConverter(
    command: PdfOperationClass.PdfCommand,
    context: GeneratorContext,
    diagramDir: File,
    resourceRoot: File,
    diagramRenderer: Option[Dox2LatexConverter.DiagramRenderer]
  ) extends Dox2LatexConverter(
    command.latexEngine,
    command.latexFormat,
    command.latexDate,
    command.latexAffiliation,
    command.latexAuthor,
    Some(context),
    Some(diagramDir),
    isDiagramGeneration = true,
    diagramRenderer = diagramRenderer,
    resourceBaseDir = Some(resourceRoot)
  ) {
    private var _diagram_generation_failure: Option[PdfDiagramGenerationFailure] = None

    def diagramGenerationFailure: Option[PdfDiagramGenerationFailure] =
      _diagram_generation_failure

    override protected def on_DiagramGenerationFailure(
      kind: String,
      source: String,
      error: Throwable
    ): Unit =
      if (_diagram_generation_failure.isEmpty)
        _diagram_generation_failure = Some(PdfDiagramGenerationFailure(kind, source, error))
  }

  private def _canonical_renderer_token(renderer: PdfOperationClass.PdfRenderer): String =
    renderer match {
      case PdfOperationClass.PdfRenderer.ChromeHeadless => "chrome-headless"
      case PdfOperationClass.PdfRenderer.Asciidoc => "asciidoc"
      case PdfOperationClass.PdfRenderer.Latex => "latex"
    }

  private val _process_termination_grace_millis = 1000L
  private val _process_termination_poll_millis = 10L

  sealed abstract class PdfTypesettingProcessFailure extends RuntimeException

  final case class PdfTypesettingProcessStartFailed()
    extends PdfTypesettingProcessFailure

  final case class PdfTypesettingNonzeroExit()
    extends PdfTypesettingProcessFailure

  final case class PdfTypesettingOutputMissing()
    extends PdfTypesettingProcessFailure

  final case class PdfTypesettingTimeout()
    extends PdfTypesettingProcessFailure

  case class DockerFile(parent: Path, name: String, containerRoot: String) {
    def containerPath: String =
      s"/work/$containerRoot/$name"

    def parentContainerPath: String =
      s"/work/$containerRoot"

    def containerUri: String =
      s"file://$containerPath"
  }
}
