# Phase 12 Checklist: Structured Rendering Diagnostics and Terminal Failure Semantics

This checklist is the authoritative progress ledger for Phase 12. It is not a
normative behavior contract.

Phase Status: CLOSED THROUGH THE FINAL RELEASE BOUNDARY

Predecessor: Phase 11 release closure.

## DIAG12-01: Diagnostic Vocabulary and Boundary Contract

Stage Status:
- Current status: COMMITTED
- Commit: feeacbd9a36ed7a0fab73ff1498295f292f931c7
- Owner: SmartDox parser, operation result, and CLI diagnostic boundary
- Update rule: Advance only after the closed vocabulary, facets, and
  retryability semantics are accepted together.

- [x] Define the closed rendering-stage vocabulary.
- [x] Define `document.syntax.invalid` with path, line, column, and authored
      token context.
- [x] Define typed cause and terminal/retryable semantics.
- [x] Define aligned Record/JSON and human-readable CLI projections.

## DIAG12-02: Parser and PDF Stage Propagation

Stage Status:
- Current status: COMMITTED
- Commits: 67390bc6c39a821c4c2f54f8acaf370c5e3c4f6e,
  8fd5a4e04d5dd6358b09e45c64021db38a7d2739, and
  ba012304db7d0458898c5f493000965107449ea1
- Owner: SmartDox parser and PDF operation pipeline
- Update rule: Do not conflate successful entry into one stage with entry into
  the next stage.

- [x] Convert parser-internal terminal state dumps into structured syntax
      diagnostics.
- [x] Propagate parse, locale-selection, diagram-generation, and typesetting
      stage identity without relabeling.
- [x] Preserve structured diagnostics before generic Throwable conversion.
- [x] Mark deterministic input failures terminal and non-retryable before any
      external process starts.

## DIAG12-03: Executable Specification and Acceptance

Stage Status:
- Current status: CLOSED THROUGH THE FINAL RELEASE BOUNDARY
- Owner: SmartDox Phase 12
- Update rule: Accept only with focused parser/PDF evidence, independent Phase
  review, and final full validation at the release gate.

- [x] Reproduce unsupported `~~~text` input and assert exact
      `document.syntax.invalid` location evidence.
- [x] Assert that a parse failure starts neither PlantUML/Kroki nor a
      typesetting process.
- [x] Cover locale-selection, diagram-generation, process-start, nonzero-exit,
      and timeout failure classifications.
- [x] Cover Record/JSON/CLI projection consistency and retryability.
- [x] Run focused parser and PDF validation (`P12-FULL-REPAIR-VAL-013`: 111
      passing tests).
- [x] Complete independent Phase review and close its accepted repair cycles.
- [x] Gate the final full validation and distinct release commit as this Phase
      closure boundary.
