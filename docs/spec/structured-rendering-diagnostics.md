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
authored-token context are required syntax facets. The stage is `parse`.

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
  `document.syntax.invalid` with its source facets and `parse` stage;
- deterministic syntax and locale-selection failures are terminal and
  non-retryable, while deterministic PDF renderer selection or descriptor
  failures, including unsupported `PdfRenderer` selection, are `typesetting`,
  terminal, non-retryable, and occur before external renderer-process startup;
- an earlier-stage failure starts neither diagram generation nor typesetting;
- external process/service failures preserve their originating
  `diagram-generation` or `typesetting` stage; and
- CLI and Record/JSON views agree because they project one typed diagnostic.

This specification does not admit `~~~` grammar, define a retry loop, change a
renderer or Cozy launcher, or authorize publication or deployment.
