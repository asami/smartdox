# RDF-Grounded Terminology Specification

Status: provisional specification / implementation pending
Date: 2026-08-18

This Phase 2 Stage 2.1 contract is normative for the future terminology
implementation, but is not parser-backed stable grammar. Parser, AST,
resolution, rendering, and extraction work is deferred to Stages 2.2–2.4.

## Namespace context and identity

Each document that uses CURIE terminology references declares `term_namespaces`
in `HEAD` as a HOCON object:

```dox
# HEAD

term_namespaces = { smterm = "https://www.simplemodeling.org/glossary/" }
```

`smterm` has base `https://www.simplemodeling.org/glossary/`; `smglo` denotes
the TBox vocabulary `https://www.simplemodeling.org/glossary/ontology/0.1-SNAPSHOT#`.
For SimpleModeling.org glossary term instances, the canonical concept IRI is
`https://www.simplemodeling.org/glossary/{relative-glossary-path-without-.dox}`.
Thus `smterm:object-foundation/association` identifies
`https://www.simplemodeling.org/glossary/object-foundation/association`.

An absolute IRI is retained unchanged. CURIE expansion uses only the explicit
namespace context. Unknown prefixes and relative references are diagnostics;
there is no label, NLP, source-path, slug, or registry-order fallback. Concept,
authored source-document, and public glossary-page resources are distinct;
source/page relations never replace the concept IRI.

## Provisional inline grammar

```ebnf
term       ::= "<term" " ref=\"" iri-or-curie "\"" (" form=\"" term-form "\"")? ">" visible-text "</term>"
term-form  ::= "canonical" | "short" | "bilingual" | "verbatim"
dfn        ::= "<dfn" (" about=\"" iri-or-curie "\"")? (" id=\"" html-anchor "\"")? ">" visible-text "</dfn>"
noterm     ::= "<noterm>" visible-text "</noterm>"
```

`<term ref="IRI-or-CURIE" form="canonical|short|bilingual|verbatim">…</term>`
is an explicit reference. `form` defaults to `canonical`. The body is required;
an omitted body is a diagnostic. `verbatim` preserves compatible authored
visible text. Explicit `ref` resolution runs before automatic matching.

`<dfn about="IRI-or-CURIE" id="html-anchor">…</dfn>` defines the RDF concept.
`about` remains optional for backwards compatibility. `id` is a local HTML
anchor only and never becomes concept identity.

`<noterm>…</noterm>` suppresses terminology resolution only. It is neither
code/raw-inline nor a replacement for either semantics, and it must not nest
`<term>`, `<dfn>`, or another `<noterm>`.

## Resolution and metadata

Automatic resolution is permitted only for concepts with `linkable` bare-label
policy. `context-only` resolution needs a deterministic explicit scope in a
later implementation; without that scope it requires `<term>`. `explicit-only`
always requires `<term>`.

Concept metadata includes localized canonical labels, localized short labels,
aliases, abbreviations, scope qualifier, bare-label linking policy, and
definition/source/page relationships. The following are diagnostics with source
locations: unresolved reference, duplicate identity, ambiguous resolution,
unknown prefix, relative reference, and incompatible requested label form.

## Display, speech, and outputs

The visible Japanese first use may be `オブジェクトモデリング（Object Modeling）`,
while default Japanese narration is `オブジェクトモデリング`. Speech must not repeat
an abbreviation expansion already visible.

Future rendered HTML metadata, term-occurrence data, RDF/JSON-LD, and BoK
records preserve the resolved concept IRI, surface form, locale, occurrence
kind, resolution kind, and source path/location. These are required future
projections; no current implementation is implied.

## Deferred executable specification

No executable specification is added in this documentation-only stage. Parser,
resolution, and output executable specifications are deferred to TERM2-02,
TERM2-03, and TERM2-04.
