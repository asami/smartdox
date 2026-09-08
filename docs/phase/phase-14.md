# Phase 14: PublishMetadata Responsibility Decomposition

Status: IN PROGRESS

Plan date: 2026-09-08

Predecessor: Phase 13 release closure
`f45f8935ebc501ce0b67dae8b2e4732ec32c3a0f`.

Authoritative responsibility boundary:

- `docs/design/publish-metadata-responsibility-decomposition.md`
- `docs/spec/publish-metadata-decomposition-compatibility.md`

## Goal

Decompose the 1,477-line `PublishMetadata` implementation into cohesive
internal collaborators while preserving its public facade methods and nested
public model identities. This is the only goal of Phase 14.

## Scope

The decomposition is behavior-preserving and remains behind the existing
`PublishMetadata` facade. Public loading, realm, page, article-media, video,
identity, and RDF-artifact behavior remains governed by the linked design and
compatibility specification.

## Stages

### PMD14-01: Contract and Regression Coverage

Stage Status:
- Current status: IN PROGRESS
- Owner: `PublishMetadata` compatibility boundary
- Update rule: Advance only after the established loading, catalog, and RDF
  behavior has executable regression coverage and focused validation.

Add the compatibility specification and deterministic executable coverage for
bundle/standalone loading selection, catalog page projection, and configured
RDF artifact merge and missing-policy behavior.

### PMD14-02: Article-Media Internal Extraction

Stage Status:
- Current status: OPEN
- Owner: internal article-media loading, normalization, and projection
  collaborators
- Update rule: Advance only after exact-locale, role, legacy compatibility,
  and projection behavior remains facade-compatible.

Extract cohesive article-media responsibilities without changing registry
schemas, locale semantics, media projection, or public model identities.

### PMD14-03: Registry, Catalog, and RDF Internal Extraction

Stage Status:
- Current status: OPEN
- Owner: internal publication registry, catalog-page, and RDF collaborators
- Update rule: Advance only after registry selection, catalog pages, public
  realm, video publication, and RDF merge behavior remains compatible.

Extract cohesive registry/catalog/RDF responsibilities behind the existing
facade and configured policy boundary.

### PMD14-04: Acceptance and Closure

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 14
- Update rule: Close only after every checklist item has exact-tree evidence,
  independent review, final validation, and a distinct release boundary.

Complete focused and full acceptance, independent review, and release closure.

## Explicit Exclusions

Phase 14 does not change behavior, public API, article-media schema, locale
semantics, media projection, Cozy, publication, deployment, or any Phase 15
work. Production source, fixtures, configuration, generators, DoxSite,
semanticweb, Strategy, and README changes are not admitted by PMD14-01.

## Completion Criteria

Phase 14 closes only when the checklist is complete, the compatibility
specification is satisfied by the preserved facade and extracted collaborators,
focused and full validation pass, independent review is accepted, and the
distinct release boundary succeeds.
