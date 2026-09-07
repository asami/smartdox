# Phase 12: Structured Rendering Diagnostics and Terminal Failure Semantics

Status: CLOSED THROUGH THE FINAL RELEASE BOUNDARY

Plan date: 2026-09-07

Predecessor: Phase 11 release closure.

Authoritative diagnostic contract:

- `docs/design/structured-rendering-diagnostics.md` (stable responsibilities
  and boundaries)
- `docs/spec/structured-rendering-diagnostics.md` (normative observable
  behavior)

## Goal

Establish the structured rendering-diagnostic contract for SmartDox document
and PDF failures. The authoritative behavior is defined by the linked design
and specification; this page records only Phase 12 scope and work status.

## Origin

The KnowledgeHub weekly-report PDF incident on 2026-09-07 exposed an internal
parser-state dump as a misattributed typesetting startup problem. The linked
authority documents preserve the resulting diagnostic boundary without making
this ledger a behavior definition.

## Phase Plan Gate

Phase Plan Gate: PROCEED

- target: approximate 6-hour delivery; preferred 4–8-hour band
- planning demand: public diagnostic vocabulary across parser and PDF stages
- accepted protected-decision profile: `gpt-5.6-terra / xhigh`
- expensive reasoning kernel: define stable failure identity and retryability
  without exposing parser implementation state or coupling callers to one
  renderer
- parent reasoning mode policy: standard
- estimated at recommended profile: 5–7 hours
- split disposition: keep one Phase because diagnostic identity, stage
  propagation, CLI projection, and executable specifications are one public
  failure contract
- no implementation, validation, publication, deployment, or commit is
  authorized by this plan alone

## Scope

In scope:

- implement and verify the authoritative structured rendering-diagnostic
  contract in the linked design and specification; and
- retain Phase 12 work status and acceptance progression in this ledger.

Out of scope:

- SmartDox grammar admission, renderer or PDF-layout semantics;
- caller-side retry loops, Cozy workflow state, and launcher/local-wrapper
  resolution; and
- publication, upload, deployment, or downstream consumer regeneration.

## Stages

### DIAG12-01: Diagnostic Vocabulary and Boundary Contract

Stage Status:
- Current status: COMMITTED
- Commit: feeacbd9a36ed7a0fab73ff1498295f292f931c7
- Owner: SmartDox parser, operation result, and CLI diagnostic boundary
- Update rule: Advance only after the closed vocabulary, facets, and
  retryability semantics are accepted together.

- Establish the linked design/spec authority for diagnostic identity, stage,
  source facets, cause, terminality, retryability, and projections.

### DIAG12-02: Parser and PDF Stage Propagation

Stage Status:
- Current status: COMMITTED
- Commits: 67390bc6c39a821c4c2f54f8acaf370c5e3c4f6e,
  8fd5a4e04d5dd6358b09e45c64021db38a7d2739, and
  ba012304db7d0458898c5f493000965107449ea1
- Owner: SmartDox parser and PDF operation pipeline
- Update rule: Do not conflate successful entry into one stage with entry into
  the next stage.

- Implement the accepted contract without broadening its grammar, retry, or
  external-renderer boundary.

### DIAG12-03: Executable Specification and Acceptance

Stage Status:
- Current status: CLOSED THROUGH THE FINAL RELEASE BOUNDARY
- Owner: SmartDox Phase 12
- Update rule: Accept only with focused parser/PDF evidence, independent Phase
  review, and final full validation at the release gate.

- Prove the linked specification through focused executable evidence and the
  required review and release gates.

## Completion Criteria

Phase 12 is complete only when the authoritative specification has focused
executable evidence and the required review and release gates pass. The
focused closure review sealed the two-cycle repair delta after
`P12-FULL-REPAIR-VAL-013` passed 111 focused tests. This document becomes the
authoritative closed ledger only with this distinct release commit after the
final full SmartDox suite passes; it does not claim publication, deployment,
or downstream Cozy acceptance.
