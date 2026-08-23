# Phase 8 Checklist: RDF Graph Literal Label Projection

This checklist is the authoritative progress ledger for Phase 8.

Phase Status: IN PROGRESS

## LITERAL8-01: Deterministic Label Semantics

Status: COMPLETE

- [x] Specify deterministic fallback display semantics for empty
      language-tagged literal values.
- [x] Specify deterministic fallback display semantics for empty typed literal
      values.
- [x] Specify deterministic fallback display semantics for empty plain literal
      values.
- [x] Require every generated `metadata/rdf/graph.json` node label to be
      non-empty while preserving ordinary nonempty literal labels.

## LITERAL8-02: SmartDox Projection and Executable Specifications

Status: COMPLETE

- [x] Implement the bounded SmartDox RDF graph projection correction only.
- [x] Add Given/When/Then executable specifications for empty language-tagged,
      typed, and plain literal variants.
- [x] Preserve RDF node IDs, triples, JSON-LD/Turtle semantics, and graph
      ordering.
- [x] Preserve SmartDox 2.4.18-SNAPSHOT status and do not change Scala, tests,
      build, or version/configuration files outside the admitted implementation
      and executable specifications.

## LITERAL8-03: Focused Regression and Downstream Acceptance

Status: IN PROGRESS

- [x] Run focused SmartDox regression validation for the literal-label
      projection.
- [ ] Obtain separately authorized SimpleModeling.org site regeneration, then
      prove every generated graph-node label is non-empty, including
      `literal::en` and `literal::ja`.
- [ ] Run `cozy bok finalize-metadata` over the regenerated tree and record
      before/after inventories and hashes proving that only the metadata
      allowlist changed.
- [ ] Verify its finalized manifest and declared glossary/RDF children through
      the Textus BoK reader contract.
- [ ] Verify WIP and production wrapper failure paths for missing or
      incompatible BoK metadata on the regenerated output.
- [x] Run `git diff --check` successfully.

Scope-transfer record (2026-08-23): Cozy Phase 29 `BM29-03` is closed on the
bounded Cozy finalization contract. This SmartDox stage owns the separately
authorized generated-site and consumer acceptance evidence above. Focused
`DoxSiteDashboardSpec` validation passed (6/6) and independent review
converged. The existing SimpleModeling.org `doxsite.d` graph remains a
pre-Phase-8 artifact; it is not evidence of this implementation until a new
site generation is authorized and completed.

## LITERAL8-04: Strategy and Hygiene Ledger Synchronization

Status: IN PROGRESS

- [ ] Reconcile the strategy display with the authoritative Phase 7 closed and
      Phase 8 in-progress statuses.
- [ ] Create the durable Phase 8 Hygiene ledger record for `HYG-8-01` as
      `SCHEDULED`.
