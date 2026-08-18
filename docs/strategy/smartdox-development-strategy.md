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

Status: planned.

Purpose:

- render a resolved concept as locale-aware visible and spoken forms; and
- retain its RDF identity in stable HTML occurrence metadata.

Primary reference:

- `docs/phase/phase-3.md`
- `docs/phase/phase-3-checklist.md`

### Phase 4: RDF Term Knowledge-Graph Projection

Status: planned.

Purpose:

- emit the resolved concept IRI through RDF/JSON-LD and BoK-ready occurrence
  records without label-based identity reconstruction.

Primary reference:

- `docs/phase/phase-4.md`
- `docs/phase/phase-4-checklist.md`

### Phase 5: RDF Term Compatibility

Status: planned.

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

## Current Priority

No implementation phase is active. Phase 3 is the next planned RDF
terminology delivery boundary, but it must be selected through a new
authoritative phase decision before source mutation. Phases 3 through 6 are
separate delivery boundaries; they must not be represented as completed merely
because the terminology contract is documented. Each is intended to fit one
roughly two-hour provisional execution; if an implementation assessment says
that it cannot, the current Phase must be split before source mutation.
textus-bok service and MCP implementation remain downstream responsibilities.
