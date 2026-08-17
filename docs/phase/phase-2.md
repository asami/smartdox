# Phase 2: RDF-Grounded Terminology Semantics

Status: active

Start date: 2026-08-16

## Goal

Make an RDF URI node the single stable identity of a glossary concept across
SmartDox authoring, glossary resolution, generated site metadata, RDF/JSON-LD,
and the BoK handoff. SmartDox must distinguish an explicit term reference, a
term definition, and an occurrence that is intentionally not a term, without
inferring concept identity from a surface string when that string is ambiguous.

## Scope

In scope:

- one shared RDF URI-node identity for each glossary concept;
- separation of the glossary concept node from its source document and public
  glossary page resources;
- CURIE and absolute-IRI resolution through an explicit namespace context;
- a dedicated inline term-reference form based on `<term ref="...">`;
- an RDF-node-bearing definition form based on `<dfn about="...">` while
  preserving the existing HTML-anchor responsibility of `id`;
- a dedicated non-term form based on `<noterm>...</noterm>`;
- glossary metadata for the concept node, localized canonical and short labels,
  scope, abbreviation, aliases, and bare-label linking policy;
- deterministic explicit and automatic term resolution with diagnostics for
  unresolved, duplicate, ambiguous, or label-incompatible references;
- first-use bilingual display and abbreviation expansion driven by glossary
  metadata rather than duplicated article prose;
- separation of visible term annotation from the speech form used by narration
  and text-to-speech, so a bilingual first-use label is spoken only once in the
  primary locale;
- HTML, dashboard metadata, RDF/JSON-LD, and BoK extraction that retain the
  resolved RDF node; and
- compatibility for existing `<dfn>`, safe automatic glossary links, and the
  current stable-span suppression path while migration is in progress.

Out of scope:

- natural-language or generative-AI inference of a term's identity;
- treating a source pathname, localized label, abbreviation, or generated
  dashboard slug as the canonical term identity;
- bulk migration of every existing SmartDox or SimpleModeling.org document;
- implementation of the textus-bok MCP service or its storage layer;
- editorial revision of glossary definitions unrelated to executable term
  identity and reference acceptance fixtures; and
- external ontology reconciliation beyond explicit RDF mapping assertions.

## Identity and Responsibility Boundary

### Shared Resource Identity

- A glossary concept is identified by one absolute RDF URI node.
- A CURIE in SmartDox source is expanded to that URI before semantic
  resolution; prefixes are presentation conveniences, not distinct identities.
- A source file path and a public glossary page URI are provenance and
  documentation resources. They do not replace the concept node.
- The implementation must not maintain a competing SmartDox-only `TermId`.
  If package layering prevents the Dox AST from depending directly on the RDF
  package, both layers must share one canonical resource-IRI value type.

### SmartDox

- owns term-reference syntax, AST identity, namespace expansion, glossary
  resolution, diagnostics, rendering, and extraction;
- preserves the same resolved RDF node through every generated representation;
  and
- never selects one of several matching concepts by registry order.

### SimpleModeling.org Glossary

- supplies the canonical concept node, localized labels, scope, linking policy,
  definition, and alignment evidence;
- remains the definition authority for SimpleModeling.org terminology; and
- provides bounded acceptance fixtures for ambiguous ordinary words, technical
  short labels, abbreviations, and localized first use.

### Textus BoK and MCP

- consume the RDF-node-bearing glossary and occurrence data emitted by
  SmartDox;
- do not reconstruct concept identity from a label; and
- remain downstream of this Phase. Their service implementation is not a
  SmartDox responsibility.

## Stage 2.1: Terminology and RDF Identity Contract

Stage Status:

- Current status: PLANNED
- Owner: SmartDox
- Update rule: mark work complete only from the Phase 2 checklist.
- Checklist basis: `TERM2-01`

Focus:

- promote the terminology contract into reviewed design and normative
  specification documents before parser implementation;
- define the concept-node, source-document, and public-page identity boundary;
- select the instance-resource namespace independently from the glossary
  ontology's TBox namespace;
- define CURIE expansion and absolute-IRI normalization;
- define `<term ref>`, `<dfn about>`, and `<noterm>` grammar and AST semantics;
- define canonical, short, bilingual, verbatim, and automatic display forms;
- define output-channel-specific display and speech forms, including the rule
  that a visible first-use annotation such as
  `オブジェクトモデリング（Object Modeling）` is narrated only as
  `オブジェクトモデリング` unless narration explicitly requests the English
  label;
- define RDF/JSON-LD and BoK occurrence projections.

## Stage 2.2: Parser, AST, and Resolution

Stage Status:

- Current status: PLANNED
- Owner: SmartDox
- Update rule: mark work complete only from the Phase 2 checklist.
- Checklist basis: `TERM2-02`

Focus:

- implement dedicated inline AST nodes for term reference and non-term text;
- extend definition nodes with an RDF concept resource while preserving the
  existing local `id` anchor;
- parse and validate CURIE and absolute-IRI references;
- resolve explicit references before automatic surface matching;
- enforce `linkable`, `context-only`, and `explicit-only` policies without NLP
  guessing; and
- emit source-located diagnostics instead of silently choosing an ambiguous or
  unresolved concept.

Initial context policy:

- `linkable` permits automatic linking only when the surface form resolves to
  one concept;
- `explicit-only` always requires `<term ref>`; and
- `context-only` may require `<term ref>` until an explicit, deterministic
  document or section scope is implemented. Proximity-based natural-language
  guessing is not an acceptable fallback.

## Stage 2.3: Glossary, Rendering, and Knowledge Projection

Stage Status:

- Current status: PLANNED
- Owner: SmartDox
- Update rule: mark work complete only from the Phase 2 checklist.
- Checklist basis: `TERM2-03`

Focus:

- make the shared RDF node the glossary registry key;
- retain localized preferred labels, alternate labels, abbreviations, scope,
  and bare-label policy as structured metadata;
- render links and visible first-use expansions from the resolved concept while
  projecting a locale-appropriate speech label separately;
- preserve the concept node in HTML data, term-index metadata, RDF/JSON-LD,
  and BoK occurrence records; and
- relate the concept node to its source document and public glossary page
  without conflating those resources.

## Stage 2.4: Compatibility and Downstream Acceptance

Stage Status:

- Current status: PLANNED
- Owner: SmartDox / SimpleModeling.org
- Update rule: mark work complete only from the Phase 2 checklist.
- Checklist basis: `TERM2-04`

Focus:

- preserve existing unambiguous glossary automatic linking and `<dfn>` input;
- retain `span strategy="stable"` as a compatibility mechanism while making
  `<noterm>` the precise terminology control;
- prove Japanese-first display with English at first use and abbreviation
  expansion such as `CML（Cozy Modeling Language）`;
- prove that the English annotation in a bilingual first-use display is not
  appended to the Japanese narration, and that an abbreviation expansion is
  not automatically duplicated in speech;
- prove that ordinary and technical uses of labels such as ownership,
  composition, and realization do not cross-link incorrectly; and
- provide RDF-node-bearing output that a downstream BoK/MCP consumer can use
  without label-based identity reconstruction.

## Phase Closure

Stage Status:

- Current status: PLANNED
- Owner: SmartDox
- Update rule: close only from the Phase 2 checklist and accepted validation
  evidence.
- Checklist basis: `TERM2-05`

Closure requires reviewed design and specification, executable grammar and
resolution specifications, compatible site generation, RDF/JSON-LD and BoK
projection evidence, bounded SimpleModeling.org acceptance fixtures, full
SmartDox validation, and a clean post-implementation review.

## Completion Criteria

Phase 2 closes when an author can identify a glossary concept by CURIE or
absolute RDF URI in SmartDox, deliberately suppress a non-term occurrence, and
obtain deterministic localized display and speech forms plus machine-readable
occurrence data that retain the same RDF node. A bilingual visual annotation
must not cause duplicate narration. Ambiguous bare labels must never resolve
by registry order, and existing unambiguous glossary and `<dfn>` behavior must
remain compatible.

## References

- `docs/phase/phase-2-checklist.md`
- `src/main/dox/spec/03-inline-semantics.dox`
- `src/main/dox/spec/04-tag-system.dox`
- `src/main/dox/spec/05-extraction-model.dox`
- SimpleModeling.org `docs/spec/glossary-entry-format.md`
- SimpleModeling.org `docs/notes/glossary-term-definition-policy.md`
