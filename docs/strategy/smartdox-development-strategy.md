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

### Phase 2: RDF-Grounded Terminology Semantics

Status: active.

Purpose:

- use one RDF URI node as the stable identity of a glossary concept across
  SmartDox, generated RDF/JSON-LD, and the BoK handoff;
- implement explicit term reference, definition, and non-term semantics rather
  than deriving ambiguous concept identity from a surface string;
- preserve deterministic, locale-aware automatic linking for safe labels while
  requiring explicit RDF-node references for ambiguous labels; and
- separate bilingual visual annotation from locale-appropriate speech so
  first-use English labels are not redundantly narrated; and
- retain the resolved concept node in generated site and knowledge metadata so
  textus-bok and MCP consumers do not reconstruct identity from labels.

Primary reference:

- `docs/phase/phase-2.md`
- `docs/phase/phase-2-checklist.md`
- SimpleModeling.org `docs/spec/glossary-entry-format.md`
- SimpleModeling.org `docs/notes/glossary-term-definition-policy.md`

## Current Priority

Phase 2 is the active priority. It completes the currently documented but not
fully implemented `<term>` semantics by grounding term definitions and
references in the same RDF resource identity used by generated knowledge data.
The first delivery boundary is the reviewed identity and grammar contract;
parser, resolution, projection, and bounded SimpleModeling.org acceptance
follow from that contract. textus-bok service and MCP implementation remain
downstream responsibilities.
