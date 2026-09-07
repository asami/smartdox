# Structured Rendering Diagnostics Design

Status: proposed Phase 12 design
Date: 2026-09-07

The normative contract is
[`docs/spec/structured-rendering-diagnostics.md`](../spec/structured-rendering-diagnostics.md).
This design records the stable responsibility boundaries for structured
diagnostics in the SmartDox document-to-PDF path.

## Responsibility split

| Boundary | Responsibility |
| --- | --- |
| Parser | Assign generic syntax failures to `document.syntax.invalid` before a generic `Throwable` or display boundary can discard authored source facets; delegated specialized syntax contracts retain their stable identities. |
| PDF pipeline | Preserve exactly one primary stage for each failure: `parse`, `locale-selection`, `diagram-generation`, or `typesetting`. |
| Diagnostic projection | Derive human CLI output and machine Record/JSON output from the same typed diagnostic. |
| Caller orchestration | May observe retryability, but owns any retry policy independently of the diagnostic contract. |

## Stable diagnostic boundaries

The parser owns conversion of a deterministic syntax failure into the typed
diagnostic. Generic parser syntax is `document.syntax.invalid`; delegated
specialized syntax contracts retain their stable identities, including
`image.markdown.malformed` and `image.markdown.unsupported-resource` under
[`docs/spec/markdown-image-admission.md`](../spec/markdown-image-admission.md).
It retains source identity or path, line, column, and concise authored-token
context. A generic syntax facet identifies the active offending delimiter: its
position composes the parser event position with any configured authored-line
origin, and its bounded context begins at that delimiter rather than at parser
input start. Parser-internal terminal states and generic exception displays are
not a public diagnostic identity.

The PDF pipeline assigns exactly one primary stage. Its vocabulary is closed:
`parse`, `locale-selection`, `diagram-generation`, and `typesetting`. A
failure does not acquire a later stage merely because that stage is nearby in
the pipeline. Later stages do not start after an earlier-stage failure.

Deterministic syntax and locale errors are terminal and non-retryable.
Deterministic PDF renderer selection or descriptor errors, including an
unsupported `PdfRenderer` selection, are `typesetting`, terminal and
non-retryable, and occur before external renderer-process startup. Causes that
originate in an external process or service retain their originating
`diagram-generation` or `typesetting` stage; they are not reclassified as
parse or locale failures. `retryable` describes a diagnostic condition only.
It never starts, requests, or implies a caller retry.

The typed PDF constructors own the stable terminal/non-retryable identities
`pdf.locale.invalid`, `pdf.locale.unsupported`, and `pdf.locale.unavailable`
at `locale-selection`, plus `pdf.renderer.unsupported` at `typesetting`.
Locale diagnostic token context preserves the authored selector; selected
document locale failures also bind the `PdfCommand` input path when available.

The typed PDF constructors also own the terminal/retryable typesetting
identities `pdf.typesetting.process-start-failed`,
`pdf.typesetting.nonzero-exit`, and `pdf.typesetting.output-missing`. Each
binds the authored `PdfCommand` input path, no line or column, and one bounded
canonical renderer token: `chrome-headless`, `asciidoc`, or `latex`. Their
fixed semantic causes describe only the process-start, nonzero-exit, or
missing-output category. They never contain command arguments, configured
executable paths, process output, or untrusted renderer detail.

`PdfCommand` owns the public `--typesetting-timeout-ms` input and its
`300000`-millisecond default. A supplied value must be a positive decimal
millisecond count. A missing supplied value, non-decimal input, nonpositive
value, or overflow is rejected before renderer-process startup as the safe,
terminal, non-retryable `pdf.typesetting.timeout.invalid` diagnostic. It has
no source identity and retains only bounded supplied-value context with cause
`invalid-typesetting-timeout`; its public field shape is otherwise the stable
structured-diagnostic shape.

The shared renderer-process seam owns the deadline for every local and Docker
Chrome, Asciidoctor, and LaTeX execution path. It configures merged standard
output for discard at `ProcessBuilder` creation, so output is neither retained
nor projected. When a started renderer exceeds its deadline, the seam owns
private tree cleanup: it visits the observed descendants in reverse order,
requests graceful descendant-then-parent termination, waits one fixed private
1000-millisecond grace period, and forcibly terminates still-live descendants
and parent. It then waits again for verified termination of every observed
descendant and parent before its timeout outcome escapes. Its public outcome is only the terminal, retryable
`pdf.typesetting.timeout` diagnostic at `typesetting`, bound to the
`PdfCommand` input and canonical renderer token with cause
`external-typesetting-timeout`. Child PIDs, commands, executable paths, and
process output never cross that private boundary. Interrupted waits preserve
the interruption and do not acquire the timeout identity.

An external diagram-generation failure on the LaTeX PDF route is the stable
`pdf.diagram-generation.failed` identity at `diagram-generation`. It binds the
`PdfCommand` input path as source identity, a bounded diagram-kind token
context, no line or column, the semantic cause
`external-diagram-generation-failed`, `terminal=true`, and `retryable=true`.
The PDF-private converter adapter captures the diagram kind, source, and error
without emitting the converter's in-document diagnostic block, then raises this
typed diagnostic after conversion and before TeX-file creation or typesetter
startup. Direct `Dox2LatexConverter` use retains its existing in-document
diagnostic-block fallback. The CLI view remains the safe typed projection and
does not expose the raw external error message.

## PDF ordering boundary

`PdfCommand` creates the renderer selection before PDF execution, so an
unsupported nonblank renderer becomes `pdf.renderer.unsupported` before an
external renderer process can start. `PdfOperationClass` then converts failed
locale-selector results before it parses the input. After parsing, it converts
the selected-document locale result before site resolution, diagram generation,
workspace preparation, or renderer-process startup. The parser's existing
`StructuredRenderingDiagnosticException` is deliberately not caught, so its
document-syntax identity and `parse` stage pass through unchanged.

This boundary maps the external LaTeX diagram-generation failure and all common
renderer-process outcomes. `PdfRendererExecution` owns the package-private
process seam, its PDF-private diagram adapter, failure algebra, canonical
renderer-token projection, and Docker execution-path value. The shared process
seam raises one of the PDF-private outcome types for start failure, nonzero
exit, or deadline expiry, and the Chrome, Asciidoctor, and LaTeX output checks
raise the missing-output outcome. `_execute` invokes that seam's stable typed
diagnostic mapping.
Parser and locale `StructuredRenderingDiagnosticException` values, and the
accepted diagram-generation translation, do not enter that translation path
and retain their earlier-stage identities. Merged process output is discarded
before process startup and is never retained in a public diagnostic.

## Deliberate exclusions

- No admission of `~~~` as SmartDox grammar.
- No renderer, Cozy retry-loop, or launcher change.
- No publication or deployment behavior.
