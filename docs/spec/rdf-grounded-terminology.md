# RDF-Grounded Terminology Specification

Status: stable Phase 2 grammar specification
Date: 2026-08-19

This normative Phase 2 contract is parser-backed stable grammar. It covers
parser AST values, explicit namespace and term resolution, and deterministic
diagnostics. Display and speech projection are Phase 3 work; RDF/JSON-LD and
BoK occurrence output are Phase 4 work; compatibility proof is Phase 5 work.

## Namespace context and identity

Each document that uses CURIE terminology references declares `term_namespaces`
in `HEAD` as a HOCON object:

```dox
# HEAD

term_namespaces {
  smterm = "https://www.simplemodeling.org/glossary/"
}
```

`smterm` has base `https://www.simplemodeling.org/glossary/`; `smglo` denotes
the TBox vocabulary `https://www.simplemodeling.org/glossary/ontology/0.1-SNAPSHOT#`.
For SimpleModeling.org glossary term instances, the canonical concept IRI is
`https://www.simplemodeling.org/glossary/{relative-glossary-path-without-.dox}`.
Thus `smterm:object-foundation/association` identifies
`https://www.simplemodeling.org/glossary/object-foundation/association`.

An absolute HTTP(S) IRI is retained unchanged. CURIE expansion uses only the
explicit namespace context. Unknown prefixes, relative references, and
non-HTTP(S) schemes are diagnostics; there is no label, NLP, source-path, slug,
or registry-order fallback. Concept, authored source-document, and public
glossary-page resources are distinct; source/page relations never replace the
concept IRI.

## Stable inline grammar

```ebnf
reference  ::= http-iri | declared-curie
term       ::= "<term" " ref=\"" reference "\"" (" form=\"" term-form "\"")? ">" visible-text "</term>"
term-form  ::= "canonical" | "short" | "bilingual" | "verbatim"
dfn        ::= "<dfn" (" about=\"" reference "\"")? (" id=\"" html-anchor "\"")? ">" visible-text "</dfn>"
noterm     ::= "<noterm>" visible-text "</noterm>"
```

`Dox.Term`, `Dox.NoTerm`, and attribute-preserving `Dox.Dfn` are parser-backed
AST values with full source locations. `<term ref="HTTP(S)-IRI-or-declared-CURIE"
form="canonical|short|bilingual|verbatim">…</term>` is an explicit reference.
`form` defaults to `canonical`; `canonical`, `short`, `bilingual`, and
`verbatim` have their parse-and-resolve semantics in this grammar. The body is
required, and `verbatim` preserves compatible authored visible text. Explicit
`ref` resolution is against definitions or supplied concepts.

`<dfn about="HTTP(S)-IRI-or-declared-CURIE" id="html-anchor">…</dfn>` defines
the RDF concept.
`about` remains optional for backwards compatibility. `id` is preserved
separately as a local HTML anchor and never becomes concept identity. Phase 2
resolves `about`; it does not claim resolver validation of `id`.

`<noterm>…</noterm>` suppresses terminology resolution only. It is neither
code/raw-inline nor a replacement for either semantics, and it must not nest
`<term>`, `<dfn>`, or another `<noterm>`.

## Resolution and diagnostics

`RdfTermResolver` reads explicit `HEAD` `term_namespaces`, expands declared
CURIEs and HTTP(S) IRIs, and has no bare-label or registry-order fallback.
An absolute reference IRI must use the `http` or `https` scheme; a non-HTTP(S)
scheme is unsupported and diagnostic. Automatic bare-label resolution is not
stable Phase 2 grammar.

Deterministic diagnostics induced by SmartDox source forms have source
locations: missing or unknown prefix; malformed, relative, or non-HTTP(S) IRI;
missing `ref`; empty visible text; unsupported form; unresolved reference;
incompatible visible form; and `noterm` nesting. Validation of supplied read-only catalog data,
including duplicate identity or conflicting canonical labels, may have no
document location.

## Display, speech, and outputs

The visible Japanese first use may be `オブジェクトモデリング（Object Modeling）`,
while default Japanese narration is `オブジェクトモデリング`. Speech must not repeat
an abbreviation expansion already visible.

Phase 3 display and speech projection must preserve the resolved concept IRI
when introduced. Phase 4 rendered HTML metadata, term-occurrence data,
RDF/JSON-LD, and BoK records must preserve the resolved concept IRI, surface
form, locale, occurrence kind, resolution kind, and source path/location.
These are future projections; no current output implementation is implied.

## Implementation status

The parser, AST, namespace resolution, and explicit term resolution are stable
Phase 2 grammar. Display and speech projection, RDF/JSON-LD and BoK occurrence
output, and compatibility proof remain separate Phase 3, Phase 4, and Phase 5
work respectively.
