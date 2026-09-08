# Phase 15: Article Header Metadata and Media Actions

Status: IN PROGRESS

Plan date: 2026-09-08

Scheduling note: Phase 14 remains reserved for the previously recorded
`PublishMetadata` responsibility decomposition. Phase 15 is active after the
accepted Phase 13/14 boundaries. AHEAD15-02 is accepted; AHEAD15-03 remains in
progress for its independent review, final full validation, and release gates.
The actual Cozy BoK fixture acceptance is recorded as Future Development
Candidate `DEV-006` and is not started by this closure contract.

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
- prove direct SmartDox consumption of the same common projection while
  retaining actual Cozy BoK fixture acceptance as the deferred `DEV-006`
  requirement.

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
- Current status: ACCEPTED
- Step commit: `ba7b672c2bc1ba5c50e1b51e724d7b0d5e031a2b`
- Focused validation: receipt `P15-AHEAD15-02-VAL-006` (24 succeeded,
  0 failed)
- Owner: SmartDox DoxSite and article-media projection
- Update rule: Preserve accepted status against the exact Step commit and
  focused receipt; later closure remains governed by the Phase checklist.

- Implement the title-adjacent metadata and action groups without changing the
  existing publication registry or Notice contract.
- Render the registered infographic below the effective LEAD and connect its
  header action to the inline figure/full-size asset.

### AHEAD15-03: Common Consumer Acceptance

Stage Status:
- Current status: IN_PROGRESS
- Owner: SmartDox Phase 15
- Update rule: Close only after the direct DoxSite proof, independent Phase
  review, final full validation, and release closure. The actual Cozy fixture
  requirement is retained as Future Development Candidate `DEV-006` and is not
  claimed by the direct-DoxSite evidence.

- Complete the direct DoxSite proof and preserve the common projection contract
  for the later actual Cozy BoK fixture acceptance.

## Completion Criteria

Phase 15 is complete only when the checklist is fully closed, the promoted
design/specification fixes the common page contract, available media actions
are discoverable below the title, infographic placement follows the effective
LEAD contract, metadata remains extensible and accessible, direct DoxSite proof
is accepted, and the mandatory review/full-validation/release gates complete.
The actual Cozy BoK fixture remains a normative consumer-acceptance requirement
in the design and specification, but is explicitly deferred as `DEV-006` to a
later explicitly invoked cross-repository Phase. This closure contract does
not create or activate that Phase, and claims no publication, deployment, or
downstream production acceptance.

## References

- `docs/phase/phase-15-checklist.md`
- `docs/notes/article-header-metadata-and-media-actions.md`
- `docs/journal/2026/09/2026-09-08-article-header-media-discoverability-decision.md`
- `docs/design/article-media-publication.md`
- `docs/spec/article-media-publication.md`
