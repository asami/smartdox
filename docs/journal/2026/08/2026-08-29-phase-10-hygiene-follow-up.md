# Phase 10 Hygiene Follow-up

Status: OPEN
Date: 2026-08-29

This non-normative ledger preserves nonblocking maintenance findings from the
Phase 10 full review. Neither item changes the accepted Markdown-image/PDF
behavior and neither was admitted to the Phase 10 repair boundary.

## HYG-P10-01

Status: OPEN

Repository/path: `smartdox`,
`src/main/scala/org/smartdox/converters/Dox2LatexConverter.scala` lines 59-60.

Evidence: pre-existing private members `_listStack` and `_krokiGenerator` use
camel-case rather than the repository's private `_snake_case` convention.

Category/risk: Hygiene / naming consistency.

Separation: renaming needs an independently scoped, behavior-preserving
maintenance change with call-site and regression validation.

Proposed grouping: SmartDox Scala naming hygiene batch.

Task/commit: not admitted to Phase 10.

Hygiene Triage: HANDED_OFF
Hygiene ID: HYG-P10-01
Handoff Journal: smartdox:docs/journal/2026/09/2026-09-02-hygiene-resolution-batch-handoff.md
Handed Off On: 2026-09-02

## HYG-P10-01 Resolution

Hygiene Status: RESOLVED
Resolution Batch: smartdox:docs/journal/2026/09/2026-09-02-hygiene-resolution-batch-handoff.md
Final Focused Review: CLEAN; `_list_stack` and `_kroki_generator` are renamed with all direct references updated.
Final Validation: `sbt --batch test`, invocation `95651-20260901T220319Z` (341 succeeded, 0 failed).
Acceptance Commit: this hygiene-batch acceptance commit.

## HYG-P10-02

Status: OPEN

Repository/path: `smartdox`, `src/main/scala/org/smartdox/parser/DoxInlineParser.scala`,
`src/main/scala/org/smartdox/parser/Dox2Parser.scala`, and
`src/main/scala/org/smartdox/service/operations/PdfOperationClass.scala`.

Evidence: the Phase 10 full review recorded `DoxInlineParser.scala` at 1,748
lines, `Dox2Parser.scala` at 889 lines, and `PdfOperationClass.scala` growing
from 651 to 828 lines, crossing the repository's split-evaluation band.

Category/risk: Hygiene / maintainability.

Separation: a safe split must retain parser and PDF-operation behavior under a
dedicated decomposition boundary with executable coverage.

Proposed grouping: SmartDox parser/PDF operation decomposition.

Task/commit: not admitted to Phase 10.
