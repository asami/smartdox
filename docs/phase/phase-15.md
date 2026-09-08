# Phase 15: Article Header Metadata and Media Actions

Status: IN PROGRESS

Plan date: 2026-09-08

Scheduling note: Phase 14 remains reserved for the previously recorded
`PublishMetadata` responsibility decomposition. Phase 15 is not activated by
this plan and must be sequenced against the accepted Phase 13/14 boundaries
before implementation starts.

## Goal

Make registered article media immediately discoverable in generated SmartDox
site pages by introducing a title-adjacent metadata/action region and an
inline infographic below the effective LEAD, with the same projection usable
by direct DoxSite consumers and Cozy-built BoKs.

## Origin

The 2026-09-08 site-page review found that the existing plain video and PDF
links are difficult to find and that the registered infographic is absent from
the article page. The design direction and unresolved presentation questions
are recorded in
`docs/notes/article-header-metadata-and-media-actions.md`.

## Scope

In scope:

- promote the article-header metadata/action anatomy into stable SmartDox
  design and specification contracts;
- project available video, summary-slides PDF, article PDF, and infographic
  actions directly below the title with deterministic localized semantics;
- establish a compact, extensible metadata strip beginning with reliably
  available tag/date metadata and leaving room for later metadata kinds;
- project a registered infographic as an accessible inline figure immediately
  below the effective LEAD, with a deterministic no-LEAD fallback;
- preserve exact-locale publication resolution and Notice projections; and
- prove direct SmartDox and Cozy BoK consumption of the same common projection.

Out of scope:

- changing media registry roles, filename inference, locale fallback, artifact
  generation, upload, or publication;
- introducing a Cozy-only HTML transformation or duplicate registry schema;
- manufacturing metadata that is not represented reliably by SmartDox; and
- redesigning unrelated article-body, dashboard, feed, or Notice-card content.

## Stages

### AHEAD15-01: Page Anatomy and Metadata Contract

Stage Status:
- Current status: ACCEPTED
- Owner: SmartDox article-page projection
- Update rule: Accepted after the header, metadata, action, fallback, and
  consumer-ownership decisions are promoted into design and specification and
  the bounded review-convergence ledger is clean.

- Define semantic regions, stable ordering, localized labels, accessibility,
  responsive/print behavior, and the first canonical metadata fields.

### AHEAD15-02: Header Actions and Inline Infographic Projection

Stage Status:
- Current status: OPEN
- Owner: SmartDox DoxSite and article-media projection
- Update rule: Advance only after all partial-media, LEAD/no-LEAD, locale, and
  no-media cases have deterministic executable evidence.

- Implement the title-adjacent metadata and action groups without changing the
  existing publication registry or Notice contract.
- Render the registered infographic below the effective LEAD and connect its
  header action to the inline figure/full-size asset.

### AHEAD15-03: Common Consumer Acceptance

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 15
- Update rule: Close only after direct DoxSite and Cozy BoK fixture acceptance,
  independent Phase review, final full validation, and release closure.

- Prove that Cozy BoK consumes the common SmartDox projection without a
  consumer-specific rewrite.

## Completion Criteria

Phase 15 is complete only when the checklist is fully closed, the promoted
design/specification fixes the common page contract, available media actions
are discoverable below the title, infographic placement follows the effective
LEAD contract, metadata remains extensible and accessible, direct SmartDox and
Cozy BoK fixtures pass, and the mandatory review/full-validation/release gates
complete. Planning this Phase does not activate it or claim implementation,
publication, deployment, or downstream production acceptance.

## References

- `docs/phase/phase-15-checklist.md`
- `docs/notes/article-header-metadata-and-media-actions.md`
- `docs/journal/2026/09/2026-09-08-article-header-media-discoverability-decision.md`
- `docs/design/article-media-publication.md`
- `docs/spec/article-media-publication.md`
