# Phase 11 Checklist: DoxSite Source-Root Semantics for Local Resources

This checklist is the authoritative progress ledger for Phase 11. It is not a
normative behavior contract.

Phase Status: IN PROGRESS / DSROOT11-01 ACCEPTED AND COMMITTED; DSROOT11-02 ACCEPTED / STEP COMMITTED; DSROOT11-03 IN PROGRESS / EXECUTABLE EVIDENCE AND FOCUSED VALIDATION COMPLETE; PHASE REVIEW SEALED; FINAL RELEASE VALIDATION PENDING

Predecessor: Phase 10 release closure and its accepted Markdown-image
contract.

## DSROOT11-01: Contract and Parser Context

Stage Status:
- Current status: ACCEPTED / STEP COMMITTED
- Owner: SmartDox parser and DoxSite boundary
- Update rule: Preserve accepted/committed status unless a separately accepted Step or current-boundary repair changes its lifecycle evidence.

- [x] Finalize the physical, virtual, and absent source-root contract.
- [x] Add a typed parser context without breaking physical-root configuration.
- [x] Preserve normalized root-relative image identity and stable diagnostics.

## DSROOT11-02: DoxSite Propagation and Output Boundary

Stage Status:
- Current status: ACCEPTED / STEP COMMITTED
- Owner: DoxSite page, bibliography, cache, and site-output paths
- Update rule: Do not mark accepted/committed until open review blockers are repaired, current final-tree validation/review accepts it, and its exact Step commit succeeds.

- [x] Establish physical roots for origin-backed page and bibliography inputs.
- [x] Establish virtual roots for stable Realm page pathnames.
- [x] Preserve virtual resource containment through DoxSite/HTML or Antora
      output without host-filesystem fallback.
- [x] Bind cache reuse to source-root identity where required.

## DSROOT11-03: Executable Acceptance

Stage Status:
- Current status: IN PROGRESS / EXECUTABLE EVIDENCE AND FOCUSED VALIDATION COMPLETE; PHASE REVIEW SEALED; FINAL RELEASE VALIDATION PENDING
- Owner: SmartDox Phase 11
- Update rule: Mark individual evidence/validation only when bound to current-tree receipts; Phase closure requires no open review blocker, final full validation, and release commit.

- [x] Add Given/When/Then specifications for physical, virtual, absent,
      traversal, cache, and compatibility behavior, including explicit cache
      isolation when the same document pathname is parsed under distinct
      physical or virtual source-root identities.
- [x] Run focused validation after implementation.
- [ ] Complete independent Phase review and final release validation.

Phase 11 has an accepted and committed DSROOT11-01 parser-context Slice.
DSROOT11-02 is accepted and committed by this Step. DSROOT11-03 is in progress
with executable evidence and focused validation complete and the Phase review
sealed; final release validation remains pending. No Phase closure,
publication, push, deployment, or downstream consumer acceptance is claimed.
