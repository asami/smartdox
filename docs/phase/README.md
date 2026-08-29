# docs/phase

Purpose: engineering work management.

Current phase state:

- No execution phase is active.
- Most recently closed phase: `phase-10.md`: Markdown Image Admission and PDF
  Image Semantics.
- No successor phase is selected. A future Phase requires a fresh explicit
  `$cncf-goal-phase` invocation.
- Phase 7 was accepted in the `TAG7-01` Step commit
  `496fa8c1af8ef2e75d473f4b05c25061397fb5d8` (`Phase 7: complete generic
  inline open-tag grammar`). Its mandatory full Phase review found one bounded
  parser blocker, which the accepted focused closure review closed. The final
  full-suite validation passed before this release commit.
- Phase 8's accepted implementation Step commit is
  `b84e7959011d1989a0d5ad660cf808eebb85268b` (`Phase 8: project deterministic
  RDF literal labels`). Its mandatory Terra/high Phase full review returned
  only `CB-P8-FULL-001`; one authorized documentation-only closure correction
  and focused closure re-review resolved it with `SEALED_LEDGER PASS`.
  The frozen final suite uses logical argv `["--batch", "test"]` and must pass
  before this release commit. Phase 8 is closed; Phase 9 was its accepted
  successor. This closure becomes authoritative only if that suite passes and
  the release commit succeeds.

Belongs:

- current phase or stage status;
- completion criteria;
- checklist ledger;
- remaining work; and
- handoff position.

Not allowed:

- exploratory design notes;
- speculative requirements;
- historical diary-style records; or
- normative design or specification text.

This directory is the work ledger layer. See
`ai/directive/core/document-lifecycle.md`.

For provisional execution, select one Phase only and plan it to reach its
implementation, executable-specification, and focused-validation outcome in
roughly two hours. If that outcome will not fit, split the Phase before source
mutation; an internal Step is not a provisional stopping point.

Phase 9 is closed through its release boundary. Its canonical ledger is
`phase-9.md` plus `phase-9-checklist.md`; the accepted handoff fixes the
PDF-role contract and single-document locale selector. Phase 9.1 is closed
through its release boundary; its authority is `phase-9.1.md` plus
`phase-9.1-checklist.md`. SmartDox owns the registry and article/Notice
consumer boundaries; Cozy generation and registration remain downstream.

Phase 10 is closed through its release boundary. Its authority is
`phase-10.md` plus `phase-10-checklist.md`; it admitted Markdown image syntax
to the common SmartDox image model and PDF path without a Cozy preprocessor or
PDF receipt format. No successor Phase is activated by this closure.
