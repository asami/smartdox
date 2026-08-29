# Phase 11 Checklist: DoxSite Source-Root Semantics for Local Resources

This checklist is the authoritative progress ledger for Phase 11. It is not a
normative behavior contract.

Phase Status: IN PROGRESS / DSROOT11-01 ACCEPTED AND COMMITTED; DSROOT11-02 PENDING

Predecessor: Phase 10 release closure and its accepted Markdown-image
contract.

## DSROOT11-01: Contract and Parser Context

Stage Status:
- Current status: ACCEPTED / STEP COMMITTED
- Owner: SmartDox parser and DoxSite boundary

- [x] Finalize the physical, virtual, and absent source-root contract.
- [x] Add a typed parser context without breaking physical-root configuration.
- [x] Preserve normalized root-relative image identity and stable diagnostics.

## DSROOT11-02: DoxSite Propagation and Output Boundary

Stage Status:
- Current status: PLANNED / NOT STARTED
- Owner: DoxSite page, bibliography, cache, and site-output paths

- [ ] Establish physical roots for origin-backed page and bibliography inputs.
- [ ] Establish virtual roots for stable Realm page pathnames.
- [ ] Preserve virtual resource containment through DoxSite/HTML or Antora
      output without host-filesystem fallback.
- [ ] Bind cache reuse to source-root identity where required.

## DSROOT11-03: Executable Acceptance

Stage Status:
- Current status: PLANNED / NOT STARTED
- Owner: SmartDox Phase 11

- [ ] Add Given/When/Then specifications for physical, virtual, absent,
      traversal, cache, and compatibility behavior, including explicit cache
      isolation when the same document pathname is parsed under distinct
      physical or virtual source-root identities.
- [ ] Run focused validation after implementation.
- [ ] Complete independent Phase review and final release validation.

Phase 11 has an accepted and committed DSROOT11-01 parser-context Slice.
DSROOT11-02 and DSROOT11-03 remain planned; no Phase closure, publication,
push, deployment, or downstream consumer acceptance is claimed.
