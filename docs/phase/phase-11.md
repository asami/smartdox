# Phase 11: DoxSite Source-Root Semantics for Local Resources

Status: IN PROGRESS / DSROOT11-01 ACCEPTED AND COMMITTED; DSROOT11-02 ACCEPTED / STEP COMMITTED; DSROOT11-03 IN PROGRESS / EXECUTABLE EVIDENCE AND FOCUSED VALIDATION COMPLETE; PHASE REVIEW SEALED; FINAL RELEASE VALIDATION PENDING

Plan date: 2026-08-30

Predecessor: Phase 10 release closure.

## Goal

Give every DoxSite Markdown-image consumer an explicit physical, virtual, or
absent source-root context, so that site generation admits valid local image
paths deterministically, rejects root-absent images without a current-directory
fallback, and does not allow paths outside the selected source root.

## Origin

Phase 10's full review recorded `DEV-P10-01`: rootless DoxSite Markdown parser
consumers reject images because they do not establish `withResourceRoot`. This
Phase owns the DoxSite consumer contract; it does not reopen Phase 10's parser
grammar or PDF semantics.

## Phase Plan Gate

Phase Plan Gate: PROCEED

- target: approximate 6-hour delivery; preferred 4–8-hour band
- planning demand: protected source-origin and containment contract
- recommended parent profile: `gpt-5.6-terra / xhigh`
- expensive reasoning kernel: select physical versus virtual source-root
  semantics without exposing host paths or weakening resource containment
- parent reasoning mode policy: standard
- estimated at recommended profile: 5–7 hours
- split disposition: keep one Phase because parser context, DoxSite origin
  propagation, virtual output ownership, cache identity, and executable
  containment evidence form one security-sensitive contract
- no implementation, validation, publication, deployment, or commit is
  authorized by this plan alone

## Authoritative proposed contract

- `docs/spec/doxsite-source-root-semantics.md`
- `docs/design/doxsite-source-root-semantics.md`
- Phase 10's accepted Markdown image contract in
  `docs/spec/markdown-image-admission.md`

## Scope

In scope:

- introduce a typed physical/virtual/absent source-root context at the DoxSite
  parser boundary;
- resolve origin-backed pages and bibliography-scanned source documents from
  the canonical parent directory of their input document;
- derive a lexical virtual root from a stable Realm document pathname when no
  physical origin exists;
- preserve normalized root-relative `ReferenceImg` identity without adding an
  absolute host path to the public Dox model;
- reject root-absent and root-escaping Markdown image paths deterministically;
- make cache identity safe when the same pathname can be parsed under distinct
  source roots; and
- add Given/When/Then executable specifications for physical, virtual, absent,
  traversal, cache, and compatibility behavior.

Out of scope:

- Phase 10 grammar/model/PDF redesign;
- current-directory fallback, remote resource retrieval, parser-time existence
  probing, or a generic host-filesystem resolver for virtual pages;
- virtual-to-PDF materialization;
- `site:[...]` link semantics, established bracket images, or ordinary Markdown
  link behavior;
- Cozy preprocessing, receipt handling, publication, upload, deployment, or
  downstream consumer acceptance.

## Stages

### DSROOT11-01: Contract and Parser Context

Stage Status:
- Current status: ACCEPTED / STEP COMMITTED
- Owner: SmartDox parser and DoxSite boundary
- Update rule: Preserve accepted/committed status unless a separately accepted Step or current-boundary repair changes its lifecycle evidence.

- Finalize the physical, virtual, and absent source-root contract.
- Introduce the typed parser context while retaining compatible physical-root
  configuration.
- Specify normalized identity and deterministic root-escape diagnostics.

### DSROOT11-02: DoxSite Propagation and Output Boundary

Stage Status:
- Current status: ACCEPTED / STEP COMMITTED
- Owner: DoxSite page, bibliography, cache, and site-output paths
- Update rule: Do not mark accepted/committed until open review blockers are repaired, current final-tree validation/review accepts it, and its exact Step commit succeeds.

- Propagate physical roots for origin-backed pages and bibliography sources.
- Propagate virtual roots for stable Realm page pathnames.
- Preserve virtual resource boundaries through DoxSite/HTML or Antora output
  without a host-filesystem fallback.
- Bind DoxSite cache identity to the source-root context when needed.

### DSROOT11-03: Executable Acceptance

Stage Status:
- Current status: IN PROGRESS / EXECUTABLE EVIDENCE AND FOCUSED VALIDATION COMPLETE; PHASE REVIEW SEALED; FINAL RELEASE VALIDATION PENDING
- Owner: SmartDox Phase 11
- Update rule: Mark individual evidence/validation only when bound to current-tree receipts; Phase closure requires no open review blocker, final full validation, and release commit.

- Add Given/When/Then executable specifications for all source-origin kinds,
  containment failures, deterministic reuse, and compatibility.
- Run focused validation, independent Phase review, and final full validation
  only at the release gate.

## Completion Criteria

Phase 11 is complete only when origin-backed and virtual DoxSite Markdown
pages use an explicit, deterministic source root; root-absent pages reject
Markdown images with the stable unsupported-resource diagnostic without a
process-current-directory fallback; no image path can escape its selected root;
DoxSite cache and output behavior preserve that context; Phase 10 PDF behavior
and existing link forms remain compatible; and executable evidence plus the
required review and release gates pass.

Current lifecycle position: DSROOT11-01 is accepted and committed; DSROOT11-02
is accepted and committed by this Step; and DSROOT11-03 is in progress with
executable evidence and focused validation complete and the Phase review
sealed. Final release validation remains pending. No Phase closure,
publication, push, deployment, or downstream consumer acceptance is claimed.
