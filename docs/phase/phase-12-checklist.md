# Phase 12 Checklist: Structured Rendering Diagnostics and Terminal Failure Semantics

This checklist is the authoritative progress ledger for Phase 12. It is not a
normative behavior contract.

Phase Status: IN PROGRESS

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
- Current status: IN PROGRESS
- Owner: SmartDox parser and PDF operation pipeline
- Update rule: Do not conflate successful entry into one stage with entry into
  the next stage.

- [ ] Convert parser-internal terminal state dumps into structured syntax
      diagnostics.
- [ ] Propagate parse, locale-selection, diagram-generation, and typesetting
      stage identity without relabeling.
- [ ] Preserve structured diagnostics before generic Throwable conversion.
- [ ] Mark deterministic input failures terminal and non-retryable before any
      external process starts.

## DIAG12-03: Executable Specification and Acceptance

Stage Status:
- Current status: PLANNED
- Owner: SmartDox Phase 12
- Update rule: Accept only with focused parser/PDF evidence, independent Phase
  review, and final full validation at the release gate.

- [ ] Reproduce unsupported `~~~text` input and assert exact
      `document.syntax.invalid` location evidence.
- [ ] Assert that a parse failure starts neither PlantUML/Kroki nor a
      typesetting process.
- [ ] Cover locale-selection, diagram-generation, process-start, nonzero-exit,
      and timeout failure classifications.
- [ ] Cover Record/JSON/CLI projection consistency and retryability.
- [ ] Run focused parser and PDF validation.
- [ ] Complete independent Phase review and final full validation.
