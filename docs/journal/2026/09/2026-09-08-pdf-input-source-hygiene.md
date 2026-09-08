# Phase 13 Hygiene Ledger

Status: OPEN
Date: 2026-09-08

This non-normative ledger preserves the nonblocking maintenance findings from
the focused and mandatory Phase 13 SmartDox reviews. It does not alter the
accepted parser, PDF operation, or Document Project public-URL behavior.

## HYG-PDF-INPUT-SOURCE-CLOSE-001

Status: OPEN

Repository/path: `smartdox`,
`src/main/scala/org/smartdox/service/operations/PdfOperationInputWorkspace.scala:21`.

Evidence: `PdfOperationInputWorkspace.parse` reads PDF input through
`scala.io.Source.fromFile(file, "UTF-8").mkString` without closing the returned
`Source`. This behavior predates the current responsibility extraction and was
preserved by it.

Category/risk: Hygiene / resource lifecycle. Repeated PDF operations can retain
file descriptors longer than necessary.

Separation: replace the reader lifetime with a deterministic close while
preserving UTF-8 decoding, parser input bytes, diagnostics, and exception
behavior. Keep this separate from the current Document Project URL and PDF
operation compatibility repair boundary.

Proposed grouping: SmartDox Phase 13 PDF input/workspace maintenance, or a
separately scoped hygiene-only repair after the current decomposition acceptance.

Task/commit: not admitted to the current repair batch.

## HYG-P13-DECOMP13-03-001

Status: OPEN

Repository/path: `smartdox`,
`src/main/scala/org/smartdox/doxsite/LinkEnabler.scala:592`, `:651`, and
`:1257`.

Evidence: the mandatory Phase 13 review confirmed trailing whitespace at these
three lines is byte-identical to the Phase base and outside the DECOMP13-03
changed hunk.

Category/risk: Hygiene / source formatting. It does not alter DoxSite link
collection, effective-content identity, locale projection, or Antora output.

Separation: remove the whitespace only in a dedicated mechanical hygiene
change; do not combine it with Document Project URL compatibility behavior.

Task/commit: not admitted to the Phase 13 repair boundary.

## HYG-P13-REVIEW-001

Status: OPEN

Repository/path: `smartdox`,
`src/main/scala/org/smartdox/parser/DoxInlineParser.scala:847`.

Evidence: the mandatory Phase 13 review confirmed private member
`openinglocation` predates the Phase base and does not meet the repository
private-member `_snake_case` naming rule.

Category/risk: Hygiene / naming conformance. The member was not introduced or
semantically changed by the parser-facade extraction.

Separation: rename only through a dedicated mechanical hygiene change with
whole-file call-site verification; do not combine it with parser grammar,
state, or diagnostic semantics.

Task/commit: not admitted to the Phase 13 repair boundary.

## HYG-P13-REVIEW-002

Status: OPEN

Repository/path: `smartdox`,
`src/main/scala/org/smartdox/parser/DoxInlineParser.scala:687`, `:801`, `:832`,
and `:859`.

Evidence: the mandatory Phase 13 review confirmed trailing whitespace at these
four lines is byte-identical to the Phase base.

Category/risk: Hygiene / source formatting. It does not affect parser grammar,
facade state, metadata, AST location, or diagnostics.

Separation: remove the whitespace only in a dedicated mechanical hygiene
change; do not combine it with parser behavior changes.

Task/commit: not admitted to the Phase 13 repair boundary.
