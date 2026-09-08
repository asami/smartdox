# docs/phase

Purpose: engineering work management.

Current phase state:

- Active phase: `phase-13.md`: Parser and PDF Operation Responsibility
  Decomposition. It preserves established parser and PDF behavior while
  separating their internal responsibilities behind the existing facades.
- Planned phase: `phase-15.md`: Article Header Metadata and Media Actions. It
  adds a title-adjacent metadata/action region and effective-LEAD infographic
  projection shared by direct SmartDox sites and Cozy-built BoKs. Phase 14
  remains reserved for the previously recorded `PublishMetadata`
  responsibility decomposition; Phase 15 is not started.
- Most recently closed phase: `phase-12.md`: Structured Rendering Diagnostics
  and Terminal Failure Semantics. Its DIAG12-01 vocabulary, DIAG12-02 parser/
  PDF propagation, and DIAG12-03 executable-acceptance stages are committed or
  closed through this final release boundary. The focused closure review sealed
  its two-cycle repair delta after 111 focused tests passed; closure becomes
  authoritative only when the final full suite and distinct release commit
  succeed.
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
PDF receipt format. Phase 11 is closed through its final release boundary. Its
authority is `phase-11.md` plus `phase-11-checklist.md`; it completes the
DoxSite consumer follow-up with explicit physical, virtual, and absent
source-root semantics without a current-directory fallback.

Phase 12 is closed through this final release boundary in `phase-12.md` plus
`phase-12-checklist.md`. It adds structured source-located syntax diagnostics,
distinct parse/locale/diagram/typesetting stage evidence, and terminal
non-retryable semantics for deterministic input failures. Cozy retry
orchestration and Cozy launcher/local-wrapper resolution remain outside the
SmartDox Phase boundary.
