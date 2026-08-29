# Phase 11 Checklist: DoxSite Source-Root Semantics for Local Resources

This checklist is the authoritative progress ledger for Phase 11. It is not a
normative behavior contract.

Phase Status: CLOSED through the final release boundary

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
- Current status: CLOSED through the final release boundary
- Owner: SmartDox Phase 11
- Update rule: Closure becomes authoritative only when the final full suite
  passes and the distinct release commit succeeds.

- [x] Add Given/When/Then specifications for physical, virtual, absent,
      traversal, cache, and compatibility behavior, including explicit cache
      isolation when the same document pathname is parsed under distinct
      physical or virtual source-root identities.
- [x] Run focused validation after implementation.
- [x] Complete independent Phase review and prepare the final release
      validation.

Phase 11 has an accepted and committed DSROOT11-01 parser-context Slice.
DSROOT11-02 is accepted and committed by this Step. DSROOT11-03 has executable
evidence and focused validation complete, and the Phase review is sealed.
Phase 11 is CLOSED through this release boundary. Its authoritative closure
requires the final full suite and the distinct release commit. No publication,
push, deployment, or downstream consumer acceptance is claimed.
