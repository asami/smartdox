# PDF Input Reader Hygiene

Status: OPEN
Date: 2026-09-08

This non-normative ledger preserves the nonblocking input-reader maintenance
finding from the focused SmartDox review. It does not alter the accepted PDF
operation behavior, the Document Project public-URL contract, or the active
Phase 13 decomposition boundary.

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
