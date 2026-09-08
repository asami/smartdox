# Phase 14 Checklist: PublishMetadata Responsibility Decomposition

This checklist is the authoritative progress ledger for Phase 14. It is not a
normative behavior contract.

Phase Status: IN PROGRESS

Predecessor: Phase 13 release closure
`f45f8935ebc501ce0b67dae8b2e4732ec32c3a0f`.

## PMD14-01: Contract and Regression Coverage

Stage Status:
- Current status: COMPLETED
- Acceptance commit: `e0c7c0d130a276f8cffc18da215844eadf503223`
- Owner: `PublishMetadata` compatibility boundary
- Update rule: Mark an item complete only after its exact-tree executable
  evidence and independent Step review are accepted.

- [x] Establish the Phase 14 design and compatibility specification as the
      stable decomposition boundary.
- [x] Prove bundle selection over standalone metadata without changing the
      established loading behavior.
- [x] Prove standalone metadata loading and the observable catalog page
      projection.
- [x] Prove configured RDF artifact merge and warn/fail missing-artifact
      behavior.

## PMD14-02: Article-Media Internal Extraction

Stage Status:
- Current status: COMPLETED
- Acceptance commit: `dfa589c848209e3971416079c18bdbf7f9de99b1`
- Owner: internal article-media collaborators
- Update rule: Mark an item complete only after facade compatibility, focused
  validation, and independent Step review.

- [x] Extract article-media parsing, normalization, and exact-locale
      resolution behind the existing facade.
- [x] Preserve native roles, legacy video compatibility, diagnostics, and
      article-media projection behavior.

## PMD14-03: Registry, Catalog, and RDF Internal Extraction

Stage Status:
- Current status: IN PROGRESS
- Owner: internal registry, catalog, and RDF collaborators
- Update rule: Mark an item complete only after registry, page, realm, video,
  and RDF compatibility evidence is accepted.

- [ ] Extract registry and catalog-page responsibilities without changing
      public page paths, realm entries, or nested public model identities.
- [ ] Extract video publication and RDF artifact responsibilities while
      preserving configured merge and missing-policy behavior.

## PMD14-04: Acceptance and Closure

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 14
- Update rule: Close only after all prior items, independent review, final
  validation, and the distinct release boundary are complete.

- [ ] Complete PMD14-01 through PMD14-03 focused validation and independent
      Step reviews.
- [ ] Complete the mandatory Phase review and bounded convergence, if any.
- [ ] Run final full validation on the exact closure tree.
- [ ] Create the distinct Phase 14 release boundary and record the preserved
      Phase 15 sequencing.

## Exclusions

No checklist item admits behavior changes, public API changes, article-media
schema changes, locale or media projection changes, Cozy, publication,
deployment, or Phase 15 implementation.
