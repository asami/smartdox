# Phase 11 Hygiene Follow-up

Status: OPEN
Date: 2026-08-30

This non-normative ledger preserves the nonblocking maintenance finding from
the Phase 11 full review. It does not change the accepted source-root behavior
and was not admitted to the Phase 11 repair boundary.

## HYG-P11-FULL-001

Status: OPEN

Repository/path: `smartdox`, `src/main/scala/org/smartdox/parser/DoxInlineParser.scala` and `src/main/scala/org/smartdox/doxsite/DoxSite.scala`.

Evidence: the Phase 11 full review measured `DoxInlineParser.scala` at 1,787 lines (Phase base: 1,748) and `DoxSite.scala` at 2,569 lines (Phase base: 2,555), each already above its repository split-evaluation band.

Category/risk: Hygiene / maintainability.

Separation: a behavior-preserving split must retain parser source-origin and DoxSite source-root behavior under a dedicated decomposition boundary with executable coverage.

Proposed grouping: SmartDox parser/DoxSite decomposition.

Task/commit: not admitted to Phase 11.
