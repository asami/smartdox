package org.smartdox.diagnostics

import io.circe.Json
import org.goldenport.extension.IRecord

/*
 * @since   Sep.  7, 2026
 * @version Sep.  7, 2026
 */
sealed trait RenderingDiagnosticStage {
  def externalValue: String
}

object RenderingDiagnosticStage {
  case object Parse extends RenderingDiagnosticStage {
    val externalValue = "parse"
  }
  case object LocaleSelection extends RenderingDiagnosticStage {
    val externalValue = "locale-selection"
  }
  case object DiagramGeneration extends RenderingDiagnosticStage {
    val externalValue = "diagram-generation"
  }
  case object Typesetting extends RenderingDiagnosticStage {
    val externalValue = "typesetting"
  }
}

case class StructuredRenderingDiagnostic(
  code: String,
  stage: RenderingDiagnosticStage,
  sourceIdentity: Option[String],
  line: Option[Int],
  column: Option[Int],
  tokenContext: Option[String],
  cause: String,
  terminal: Boolean,
  retryable: Boolean
) {
  def toCliText: String = {
    val sourcefacets = Vector(
      sourceIdentity.map(x => s"source=$x"),
      line.map(x => s"line=$x"),
      column.map(x => s"column=$x"),
      tokenContext.map(x => s"token=$x")
    ).flatten
    (Vector(code, s"stage=${stage.externalValue}") ++ sourcefacets).mkString(" ")
  }

  def toIRecord: IRecord = IRecord.data(
    "code" -> code,
    "stage" -> stage.externalValue,
    "sourceIdentity" -> sourceIdentity,
    "line" -> line,
    "column" -> column,
    "tokenContext" -> tokenContext,
    "cause" -> cause,
    "terminal" -> terminal,
    "retryable" -> retryable
  )

  def toJson: Json = Json.obj(
    "code" -> Json.fromString(code),
    "stage" -> Json.fromString(stage.externalValue),
    "sourceIdentity" -> sourceIdentity.map(Json.fromString).getOrElse(Json.Null),
    "line" -> line.map(Json.fromInt).getOrElse(Json.Null),
    "column" -> column.map(Json.fromInt).getOrElse(Json.Null),
    "tokenContext" -> tokenContext.map(Json.fromString).getOrElse(Json.Null),
    "cause" -> Json.fromString(cause),
    "terminal" -> Json.fromBoolean(terminal),
    "retryable" -> Json.fromBoolean(retryable)
  )
}

object StructuredRenderingDiagnostic {
  private val _token_context_limit = 160

  def pdfLocaleInvalid(
    tokenContext: String,
    sourceIdentity: Option[String] = None,
    cause: String = "invalid-locale-selector"
  ): StructuredRenderingDiagnostic =
    _pdf_locale_diagnostic(
      code = "pdf.locale.invalid",
      tokencontext = tokenContext,
      sourceidentity = sourceIdentity,
      cause = cause
    )

  def pdfLocaleUnsupported(
    tokenContext: String,
    sourceIdentity: Option[String] = None,
    cause: String = "unsupported-locale-selector"
  ): StructuredRenderingDiagnostic =
    _pdf_locale_diagnostic(
      code = "pdf.locale.unsupported",
      tokencontext = tokenContext,
      sourceidentity = sourceIdentity,
      cause = cause
    )

  def pdfLocaleUnavailable(
    tokenContext: String,
    sourceIdentity: Option[String] = None,
    cause: String = "selected-locale-unavailable"
  ): StructuredRenderingDiagnostic =
    _pdf_locale_diagnostic(
      code = "pdf.locale.unavailable",
      tokencontext = tokenContext,
      sourceidentity = sourceIdentity,
      cause = cause
    )

  def pdfRendererUnsupported(
    tokenContext: String,
    cause: String = "unsupported-pdf-renderer"
  ): StructuredRenderingDiagnostic =
    StructuredRenderingDiagnostic(
      code = "pdf.renderer.unsupported",
      stage = RenderingDiagnosticStage.Typesetting,
      sourceIdentity = None,
      line = None,
      column = None,
      tokenContext = Some(_bounded_token_context(tokenContext)),
      cause = cause,
      terminal = true,
      retryable = false
    )

  def pdfDiagramGenerationFailed(
    sourceIdentity: String,
    tokenContext: String,
    cause: String = "external-diagram-generation-failed"
  ): StructuredRenderingDiagnostic =
    StructuredRenderingDiagnostic(
      code = "pdf.diagram-generation.failed",
      stage = RenderingDiagnosticStage.DiagramGeneration,
      sourceIdentity = Some(sourceIdentity),
      line = None,
      column = None,
      tokenContext = Some(_bounded_token_context(tokenContext)),
      cause = cause,
      terminal = true,
      retryable = true
    )

  def pdfTypesettingProcessStartFailed(
    sourceIdentity: String,
    tokenContext: String
  ): StructuredRenderingDiagnostic =
    _pdf_typesetting_diagnostic(
      code = "pdf.typesetting.process-start-failed",
      sourceidentity = sourceIdentity,
      tokencontext = tokenContext,
      cause = "external-typesetting-process-start-failed"
    )

  def pdfTypesettingNonzeroExit(
    sourceIdentity: String,
    tokenContext: String
  ): StructuredRenderingDiagnostic =
    _pdf_typesetting_diagnostic(
      code = "pdf.typesetting.nonzero-exit",
      sourceidentity = sourceIdentity,
      tokencontext = tokenContext,
      cause = "external-typesetting-nonzero-exit"
    )

  def pdfTypesettingOutputMissing(
    sourceIdentity: String,
    tokenContext: String
  ): StructuredRenderingDiagnostic =
    _pdf_typesetting_diagnostic(
      code = "pdf.typesetting.output-missing",
      sourceidentity = sourceIdentity,
      tokencontext = tokenContext,
      cause = "external-typesetting-output-missing"
    )

  def pdfTypesettingTimeoutInvalid(
    tokenContext: String
  ): StructuredRenderingDiagnostic =
    StructuredRenderingDiagnostic(
      code = "pdf.typesetting.timeout.invalid",
      stage = RenderingDiagnosticStage.Typesetting,
      sourceIdentity = None,
      line = None,
      column = None,
      tokenContext = Some(_bounded_token_context(tokenContext)),
      cause = "invalid-typesetting-timeout",
      terminal = true,
      retryable = false
    )

  def pdfTypesettingTimeout(
    sourceIdentity: String,
    tokenContext: String
  ): StructuredRenderingDiagnostic =
    _pdf_typesetting_diagnostic(
      code = "pdf.typesetting.timeout",
      sourceidentity = sourceIdentity,
      tokencontext = tokenContext,
      cause = "external-typesetting-timeout"
    )

  def documentSyntaxInvalid(
    sourceIdentity: String,
    line: Int,
    column: Int,
    tokenContext: String,
    cause: String = "unclosed-inline-delimiter"
  ): StructuredRenderingDiagnostic =
    StructuredRenderingDiagnostic(
      code = "document.syntax.invalid",
      stage = RenderingDiagnosticStage.Parse,
      sourceIdentity = Some(sourceIdentity),
      line = Some(line),
      column = Some(column),
      tokenContext = Some(_bounded_token_context(tokenContext)),
      cause = cause,
      terminal = true,
      retryable = false
    )

  private def _pdf_locale_diagnostic(
    code: String,
    tokencontext: String,
    sourceidentity: Option[String],
    cause: String
  ): StructuredRenderingDiagnostic =
    StructuredRenderingDiagnostic(
      code = code,
      stage = RenderingDiagnosticStage.LocaleSelection,
      sourceIdentity = sourceidentity,
      line = None,
      column = None,
      tokenContext = Some(_bounded_token_context(tokencontext)),
      cause = cause,
      terminal = true,
      retryable = false
    )

  private def _pdf_typesetting_diagnostic(
    code: String,
    sourceidentity: String,
    tokencontext: String,
    cause: String
  ): StructuredRenderingDiagnostic =
    StructuredRenderingDiagnostic(
      code = code,
      stage = RenderingDiagnosticStage.Typesetting,
      sourceIdentity = Some(sourceidentity),
      line = None,
      column = None,
      tokenContext = Some(_bounded_token_context(tokencontext)),
      cause = cause,
      terminal = true,
      retryable = true
    )

  private def _bounded_token_context(p: String): String =
    p.take(_token_context_limit)
}

final class StructuredRenderingDiagnosticException(
  val diagnostic: StructuredRenderingDiagnostic
) extends RuntimeException(diagnostic.toCliText)
