# Structured Rendering Diagnostics

Status: proposed Phase 12 specification
Date: 2026-09-07

## Purpose

This specification defines the observable diagnostic contract for failures in
the SmartDox document-to-PDF path. It does not prescribe Scala implementation
class names or alter runtime implementation by itself.

## Diagnostic identity and fields

Generic malformed or unsupported SmartDox syntax for which no established
delegated specialized diagnostic contract applies MUST use the stable code
`document.syntax.invalid`. Markdown image admission retains the stable
`image.markdown.malformed` and `image.markdown.unsupported-resource`
diagnostics under
[`docs/spec/markdown-image-admission.md`](markdown-image-admission.md); this
generic document syntax code neither renames nor supersedes those identities.

Every structured rendering diagnostic MUST provide these fields:

| Field | Required behavior |
| --- | --- |
| `code` | Stable diagnostic identity. |
| `stage` | Exactly one of `parse`, `locale-selection`, `diagram-generation`, or `typesetting`. |
| source path/identity | Identifies the authored document or source when known. |
| `line` | Authored source line when the failure is source-located. |
| `column` | Authored source column when the failure is source-located. |
| concise token context | The relevant authored token or fragment without parser-internal state dumps. |
| `cause` | The semantic or originating underlying cause. |
| `terminal` | Whether this failure ends the current rendering attempt. |
| `retryable` | Descriptive retryability information only. |

For `document.syntax.invalid`, source path/identity, line, column, and concise
authored-token context are required syntax facets. Generic syntax facets MUST
identify the active offending delimiter; their bounded context MUST begin at
that delimiter rather than at parser input start. The stage is `parse`.

## Stage and failure semantics

The stage vocabulary is closed to `parse`, `locale-selection`,
`diagram-generation`, and `typesetting`. A diagnostic MUST preserve exactly
one primary stage. Later stages MUST NOT start after an earlier-stage failure.

Deterministic syntax and locale-selection errors MUST be terminal and
non-retryable. Deterministic PDF renderer selection or descriptor errors,
including an unsupported `PdfRenderer` selection, have stage `typesetting`,
are terminal and non-retryable, and occur before external renderer-process
startup. External process or service causes retain the stage at which they
originate: `diagram-generation` or `typesetting`.
`retryable` MUST NOT itself initiate, request, or imply a retry loop.

The PDF-specific stable codes are:

| Code | Stage | Required terminal/retryable values |
| --- | --- | --- |
| `pdf.locale.invalid` | `locale-selection` | `terminal=true`, `retryable=false` |
| `pdf.locale.unsupported` | `locale-selection` | `terminal=true`, `retryable=false` |
| `pdf.locale.unavailable` | `locale-selection` | `terminal=true`, `retryable=false` |
| `pdf.renderer.unsupported` | `typesetting` | `terminal=true`, `retryable=false` |
| `pdf.diagram-generation.failed` | `diagram-generation` | `terminal=true`, `retryable=true` |
| `pdf.typesetting.process-start-failed` | `typesetting` | `terminal=true`, `retryable=true` |
| `pdf.typesetting.nonzero-exit` | `typesetting` | `terminal=true`, `retryable=true` |
| `pdf.typesetting.output-missing` | `typesetting` | `terminal=true`, `retryable=true` |
| `pdf.typesetting.timeout.invalid` | `typesetting` | `terminal=true`, `retryable=false` |
| `pdf.typesetting.timeout` | `typesetting` | `terminal=true`, `retryable=true` |

For PDF execution, command construction rejects an unsupported nonblank
renderer token before an external renderer process can start. The operation
then validates an authored locale selector before parsing, and validates an
available selected document locale after parsing but before diagram generation,
workspace preparation, or renderer-process startup. Parser-origin
`StructuredRenderingDiagnosticException` values remain unchanged through this
pipeline; in particular, a parse diagnostic is not relabeled as a typesetting
diagnostic.

`--typesetting-timeout-ms` configures the finite deadline for every started
local or Docker Chrome, Asciidoctor, and LaTeX renderer process. It accepts
only a positive decimal millisecond value and defaults to `300000` when the
option is omitted. A supplied missing, non-decimal, nonpositive, or overflowed
value MUST fail during `PdfCommand` creation, before any renderer process
starts, with `pdf.typesetting.timeout.invalid`. That diagnostic has stage
`typesetting`, no source identity, no line or column, bounded context containing
the supplied value, cause `invalid-typesetting-timeout`, `terminal=true`, and
`retryable=false`.

On the LaTeX PDF route, an external diagram-generation failure MUST produce
`pdf.diagram-generation.failed` after conversion and before TeX-file creation
or external typesetter startup. It MUST bind the PDF input path as source
identity, a bounded diagram-kind token context, no line or column, and cause
`external-diagram-generation-failed`; it is terminal and retryable. Its CLI
projection MUST remain a safe typed projection and MUST NOT expose the raw
external error message. Direct `Dox2LatexConverter` behavior remains its
existing in-document diagnostic-block fallback.

At the common external renderer-process boundary, a `ProcessBuilder` start
failure MUST produce `pdf.typesetting.process-start-failed`; an exited renderer
with a nonzero status MUST produce `pdf.typesetting.nonzero-exit`; and the
existing Chrome, Asciidoctor, and LaTeX required-output-missing paths MUST
produce `pdf.typesetting.output-missing`. These diagnostics bind the authored
`PdfCommand` input path, have no line or column, and use the bounded canonical
renderer token (`chrome-headless`, `asciidoc`, or `latex`). Their semantic
causes are fixed typed values, and their CLI, Record, and JSON projections MUST
NOT expose raw process output, command arguments, configured executable paths,
or untrusted renderer error detail. Process output may be consumed solely to
preserve the external-process lifecycle boundary; it is not diagnostic data.

The common process seam discards merged process output at the `ProcessBuilder`
boundary before waiting. When the configured deadline expires, it terminates
the entire observed process tree in reverse descendant order: it asks each
descendant and then the parent to terminate, allows one fixed private
1000-millisecond graceful period, and forcibly terminates any remaining
descendants and parent. It MUST wait for verified termination of every observed
descendant and parent before it produces `pdf.typesetting.timeout`, with stage
`typesetting`, the `PdfCommand` input path as source identity, no line or
column, the bounded canonical renderer token, cause
`external-typesetting-timeout`, `terminal=true`, and `retryable=true`. The
safe CLI, Record, and JSON projections never expose a command, executable path,
process output, or child PID. An interrupted wait preserves its interrupt
status and is not reclassified as a timeout.

## Projections

Human CLI output and machine Record/JSON output MUST derive from the same
typed diagnostic. CLI output MUST present a concise diagnostic with its stable
code, stage, and available source facets. Record/JSON output MUST retain the
same code, stage, source path/identity, line, column, concise token context,
cause, terminal, and retryable fields; it MUST NOT replace them with a generic
Throwable display string.

## Required observable evidence

Executable specifications for implementation of this contract MUST show that:

- an unsupported or malformed authored syntax form yields
  `document.syntax.invalid` with active-delimiter source facets and `parse` stage;
- invalid, unsupported, and unavailable PDF locale selection yields its
  corresponding `pdf.locale.*` code before a renderer process starts, and an
  unsupported nonblank PDF renderer yields `pdf.renderer.unsupported` during
  command construction;
- deterministic syntax and locale-selection failures are terminal and
  non-retryable, while deterministic PDF renderer selection or descriptor
  failures, including unsupported `PdfRenderer` selection, are `typesetting`,
  terminal, non-retryable, and occur before external renderer-process startup;
- an earlier-stage failure starts neither diagram generation nor typesetting;
- external process/service failures preserve their originating
  `diagram-generation` or `typesetting` stage; and
- the three `pdf.typesetting.*` process identities bind the PDF input and a
  canonical renderer token while their safe typed projections omit raw renderer
  details; and
- malformed, nonpositive, and overflowed supplied typesetting deadlines fail
  with `pdf.typesetting.timeout.invalid` before renderer startup, while an
  expired deadline produces `pdf.typesetting.timeout` only after a recorded
  renderer descendant has terminated; and
- CLI and Record/JSON views agree because they project one typed diagnostic.

This specification does not admit `~~~` grammar, define a retry loop, change a
renderer or Cozy launcher, or authorize publication or deployment.
