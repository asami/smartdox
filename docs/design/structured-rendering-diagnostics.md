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
context. Parser-internal terminal states and generic exception displays are
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

## Deliberate exclusions

- No admission of `~~~` as SmartDox grammar.
- No renderer, Cozy retry-loop, or launcher change.
- No publication or deployment behavior.
