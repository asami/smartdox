# SmartDox Development Strategy

Date: 2026-08-16

Status: active

## Direction

SmartDox evolves the structured-document and site-generation foundation used
by direct sites such as SimpleModeling.org and by Cozy-built BoKs. It owns
provider-neutral document, publication-metadata, and site-projection behavior;
it does not own site-specific widget styling, external video hosting, or BoK
artifact storage.

## Phase Roadmap

### Phase 1: Article Media Publication and Site Projection

Status: closed.

Purpose:

- define a provider-neutral article/media publication input that associates an
  article and locale with a detailed infographic and a video presentation;
- project that association consistently into SmartDox article pages and Notice
  metadata for global and category pages;
- preserve `VideoPublication` as a compatible existing input; and
- provide the metadata boundary consumed by SimpleModeling.org widgets and by
  Cozy BoK site builds.

Primary reference:

- `docs/phase/phase-1.md`
- `docs/phase/phase-1-checklist.md`
- `docs/notes/article-media-publication-and-site-projection.md`

### Phase 2: RDF Term Syntax and Resolution

Status: closed.

Purpose:

- implement the parser, AST, CURIE/IRI normalization, and deterministic
  resolution foundation for RDF term forms; and
- prove accepted and rejected source forms through focused executable
  specifications.

Primary reference:

- `docs/phase/phase-2.md`
- `docs/phase/phase-2-checklist.md`

### Phase 3: RDF Term Display and Occurrence Metadata

Status: closed.

Purpose:

- render a resolved concept as locale-aware visible and spoken forms; and
- retain its RDF identity in stable HTML occurrence metadata.

Primary reference:

- `docs/phase/phase-3.md`
- `docs/phase/phase-3-checklist.md`

### Phase 4: RDF Term Knowledge-Graph Projection

Status: closed.

Purpose:

- emit the resolved concept IRI through RDF/JSON-LD and BoK-ready occurrence
  records without label-based identity reconstruction.

Primary reference:

- `docs/phase/phase-4.md`
- `docs/phase/phase-4-checklist.md`

### Phase 5: RDF Term Compatibility

Status: closed.

Purpose:

- preserve existing supported glossary behavior while introducing precise RDF
  term controls and executable compatibility evidence.

Primary reference:

- `docs/phase/phase-5.md`
- `docs/phase/phase-5-checklist.md`

### Phase 6: SimpleModeling.org RDF Term Acceptance

Status: planned.

Purpose:

- prove the bounded external terminology fixtures and record the downstream
  BoK/MCP handoff without implementing those services.

Primary reference:

- `docs/phase/phase-6.md`
- `docs/phase/phase-6-checklist.md`

### Phase 7: Generic Inline Open-Tag Grammar

Status: planned.

Purpose:

- complete generic boolean-attribute and self-closing inline open-tag parsing
  deferred from Phase 2; and
- preserve established quoted-attribute and RDF terminology behavior.

Primary reference:

- `docs/phase/phase-7.md`
- `docs/phase/phase-7-checklist.md`

## Current Priority

Phase 5 is closed by Step commit
`312a301b2d06a4f1b97098cf28d18775ab944184` (`Phase 5: preserve RDF term
compatibility`), independent full Phase review
`c966d1444ee5ed69deb5d4584b5777ca6861733e..312a301b2d06a4f1b97098cf28d18775ab944184`
(`SEALED_PHASE_LEDGER PASS`, no Current Boundary Blockers), and final mandatory
full-suite invocation `53586-20260820T203315Z` with logical argv
`["--batch", "test"]` (249 succeeded / 0 failed / 4 ignored across 31 suites;
`sbt_exit=0`, `wrapper_exit=0`, `lock=released`). Phase 6 remains planned,
separate, and excluded from this closed Phase.

## 9. Development Item Status

| ID | Source | Development item | Disposition | Target | Status |
| --- | --- | --- | --- | --- | --- |
| DEV-001 | `docs/journal/2026/08/2026-08-18-phase-2-deferred-work.md` (`P2-DFW-001`) | Complete the deferred generic boolean-attribute and self-closing inline open-tag grammar paths. | NEW_PHASE | [Phase 7](../phase/phase-7.md) | ADOPTED |
