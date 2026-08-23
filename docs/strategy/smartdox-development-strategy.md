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

Status: closed.

Purpose:

- prove the bounded external terminology fixtures and record the downstream
  BoK/MCP handoff without implementing those services.

Primary reference:

- `docs/phase/phase-6.md`
- `docs/phase/phase-6-checklist.md`

### Phase 7: Generic Inline Open-Tag Grammar

Status: closed.

Purpose:

- complete generic boolean-attribute and self-closing inline open-tag parsing
  deferred from Phase 2; and
- preserve established quoted-attribute and RDF terminology behavior.

Primary reference:

- `docs/phase/phase-7.md`
- `docs/phase/phase-7-checklist.md`

### Phase 8: RDF Graph Literal Label Projection

Status: in progress.

Purpose:

- ensure every generated `metadata/rdf/graph.json` node has a deterministic,
  non-empty display label;
- define and implement SmartDox fallback display semantics for language-tagged,
  typed, and plain literal nodes whose lexical values are empty; and
- preserve RDF node identity, triples, JSON-LD/Turtle semantics, ordinary
  nonempty literal labels, and graph ordering while correcting the projection.

Observed consumer evidence:

- SimpleModeling.org generated empty labels for `literal::en` and
  `literal::ja` nodes, and Cozy public finalization rejected those empty
  labels. This phase keeps the correction in SmartDox and does not relax Cozy
  validation.

Primary reference:

- `docs/phase/phase-8.md`
- `docs/phase/phase-8-checklist.md`

## Current Priority

Phase 7 is closed by Step commit
`496fa8c1af8ef2e75d473f4b05c25061397fb5d8` (`Phase 7: complete generic
inline open-tag grammar`), its mandatory Phase-full review, the accepted
focused closure review for `CB-P7-FULL-001`, and final mandatory full-suite
invocation `49720-20260821T020637Z` with logical argv `["--batch", "test"]`
(261 succeeded / 0 failed across 32 suites; `sbt_exit=0`, `wrapper_exit=0`,
`lock=released`). Phase 8 is the active SmartDox phase for the bounded RDF
graph literal-label projection correction.

## 9. Development Item Status

| ID | Source | Development item | Disposition | Target | Status |
| --- | --- | --- | --- | --- | --- |
| DEV-001 | `docs/journal/2026/08/2026-08-18-phase-2-deferred-work.md` (`P2-DFW-001`) | Complete the deferred generic boolean-attribute and self-closing inline open-tag grammar paths. | NEW_PHASE | [Phase 7](../phase/phase-7.md) | CLOSED |
| DEV-002 | SimpleModeling.org generated RDF graph evidence and Cozy public finalization feedback | Ensure generated RDF graph literal nodes have deterministic non-empty labels through SmartDox projection fallback semantics, then separately verify regenerated-site finalization and consumer acceptance. | NEW_PHASE | [Phase 8](../phase/phase-8.md) | IN PROGRESS |
