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

Status: closed.

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

### Phase 9: Localized PDF Contract and Source-Document Selection

Status: closed through the final release boundary.

Purpose:

- define the public article-PDF and summary-slides-PDF role contract; and
- select one exact locale from a bilingual SmartDox source document while
  preserving locale-neutral content and reporting invalid selections
  deterministically.

Primary reference:

- `docs/phase/phase-9.md`
- `docs/phase/phase-9-checklist.md`

### Phase 9.1: Localized PDF Registry and Article/Notice Projection

Status: closed through the final release boundary.

Purpose:

- implement the accepted PDF roles in publication metadata; and
- project the exact-locale references to ordinary articles and global and
  category-local Notices without changing existing media behavior.

Primary reference:

- `docs/phase/phase-9.1.md`
- `docs/phase/phase-9.1-checklist.md`

### Phase 10: Markdown Image Admission and PDF Image Semantics

Status: closed through the final release boundary.

Purpose:

- admit ordinary Markdown `![alt](path)` and established SmartDox image forms
  into one common image model; and
- preserve Japanese alt text and source-relative resources through PDF
  conversion without source punctuation leakage or a Cozy workaround.

Primary reference:

- `docs/phase/phase-10.md`
- `docs/phase/phase-10-checklist.md`

## Current Priority

Phase 7 is closed by Step commit
`496fa8c1af8ef2e75d473f4b05c25061397fb5d8` (`Phase 7: complete generic
inline open-tag grammar`), its mandatory Phase-full review, the accepted
focused closure review for `CB-P7-FULL-001`, and final mandatory full-suite
invocation `49720-20260821T020637Z` with logical argv `["--batch", "test"]`
(261 succeeded / 0 failed across 32 suites; `sbt_exit=0`, `wrapper_exit=0`,
`lock=released`). Phase 8 is closed by release commit
`1ba56ec` (`Phase 8: close RDF graph literal label projection`).

Phase 9 closes through this release boundary after its PDF-role contract and
strict single-document locale selector were accepted in Step commits
`fe23fc82936821d2da5c76250be6f1e9d353db10` and
`834823b3118a6e08cf2eae5b4987111382f2dd67`. The mandatory Phase review
findings `CB-P9-FULL-001` and `CB-P9-FULL-002` converged through focused
re-review; the frozen final release suite with logical argv `["--batch",
"test"]` must pass before the distinct release commit succeeds. No successor
is automatically activated.

Phase 9.1 closes through this release boundary after its localized PDF registry
and article/Notice projections were accepted in Step commits
`5f479beda29355de3a728a4bd145d322547e5181` and
`ec0a119b16f1ed95c83ae93c619760e09408903f`. The mandatory Phase full review
found `CPB-APDF91-001`; focused repair validation passed 36/36 and the
independent focused closure re-review returned `SEALED_LEDGER PASS`. The frozen
final release suite with logical argv `["--batch", "test"]` must pass before
the distinct release commit succeeds. No Cozy implementation or downstream
consumer acceptance is activated by this closure.

Phase 10 closes through this release boundary after Markdown-image grammar and
model documentation Step commit `64c8d895e86167dff1af69db16bd97175c907a91`,
parser/PDF admission Step commit
`413d0f501ab4e1d3bd5fc13fd7bd315bcce4a235`, and deterministic conversion
acceptance Step commit `9e00eaad9710dd6ba3dcdc8a5d9064843dcb0405`. The
mandatory Phase full review found only `CPB-P10-01`, which the accepted M0
closure correction resolved. The frozen final release suite with logical argv
`["--batch", "test"]` must pass before the distinct release commit succeeds.
No Cozy preprocessor or PDF receipt contract is introduced, and no successor
Phase is activated by this closure.

## 9. Development Item Status

| ID | Source | Development item | Disposition | Target | Status |
| --- | --- | --- | --- | --- | --- |
| DEV-001 | `docs/journal/2026/08/2026-08-18-phase-2-deferred-work.md` (`P2-DFW-001`) | Complete the deferred generic boolean-attribute and self-closing inline open-tag grammar paths. | NEW_PHASE | [Phase 7](../phase/phase-7.md) | CLOSED |
| DEV-002 | SimpleModeling.org generated RDF graph evidence and Cozy public finalization feedback | Ensure generated RDF graph literal nodes have deterministic non-empty labels through SmartDox projection fallback semantics, then separately verify regenerated-site finalization and consumer acceptance. | NEW_PHASE | [Phase 8](../phase/phase-8.md) | CLOSED |
| DEV-003 | Cozy Phase 39 scope decision on 2026-08-29 | Admit Markdown image syntax into the common SmartDox image model and preserve alt text, source-relative paths, and deterministic PDF-path behavior without a Cozy-local preprocessor. | NEW_PHASE | [Phase 10](../phase/phase-10.md) | CLOSED |
| DEV-P10-01 | Phase 10 full review | Define explicit physical/virtual DoxSite source-root semantics for Markdown-image consumers instead of relying on a current-directory fallback. | DEVELOPMENT_CANDIDATE | Later DoxSite consumer work | OPEN |
