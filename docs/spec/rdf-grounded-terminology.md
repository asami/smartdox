# RDF-Grounded Terminology Specification

Status: stable Phase 2 grammar, Phase 3 display, Phase 4 projection, and Phase 5 compatibility specification
Date: 2026-08-21

This normative contract covers parser-backed stable grammar, display, the
Phase 4 RDF/JSON-LD and BoK-ready occurrence projection, and the Phase 5
legacy-terminology compatibility boundary.

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
the RDF concept. A legacy `<dfn>` without `about` remains compatible
glossary/HTML syntax and is retained in the AST, but creates no RDF concept
identity, resolved definition, or RDF definition occurrence. The absence of
`about` is not a diagnostic. `id` is preserved only as a local HTML anchor and
never becomes RDF concept identity. Phase 2 resolves `about`; it does not claim
resolver validation of `id`.

`<noterm>…</noterm>` deliberately prevents terminology resolution while
preserving its authored inline content. It is neither code/raw-inline nor a
replacement for either semantics, and it must not nest `<term>`, `<dfn>`, or
another `<noterm>`.

## Resolution and diagnostics

`RdfTermResolver` reads explicit `HEAD` `term_namespaces`, expands declared
CURIEs and HTTP(S) IRIs, and has no bare-label, label, NLP, or registry-order
fallback.
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

Phase 3 uses a read-only concept-display registry keyed only by canonical
concept IRI. An entry may carry localized preferred labels, short labels,
aliases, abbreviations, scope, a locale-specific speech label, and an explicit
link policy. Aliases are display metadata only; they do not change the explicit
RDF-resolution contract.

For a resolved reference, canonical, short, bilingual, and verbatim forms
select display text without reconstructing identity from that text. The first
canonical occurrence may append a distinct counterpart-language preferred label
and an abbreviation. Speech text is selected separately and is published as an
`aria-label`; it must not duplicate visible bilingual or abbreviation markup.

Every rendered resolved term occurrence has at least these stable HTML
attributes:

```text
data-rdf-term-iri
data-rdf-term-locale
data-rdf-term-kind
data-rdf-term-resolution
data-rdf-term-form
```

An explicit reference is an HTML link only when its display-registry link
policy explicitly supplies an absolute HTTP(S) destination. Without that
policy it renders as a semantic span. A resolved definition retains its local
HTML `id` independently of `data-rdf-term-iri`.

## RDF/JSON-LD and BoK occurrence projection

`RdfTermProjection` accepts `RdfTermResolver.Result` plus explicit
source-document IRI, public-glossary-page IRI, source path, and locale. It may
also receive known concepts. It never manufactures concept, source-document, or
public-page identities from labels or filesystem paths. A legacy `<dfn>` without
`about` supplies no resolved definition, so it emits no RDF/JSON-LD or BoK
definition occurrence.

The graph has distinct concept, definition occurrence, source document, and
public glossary page resources. Each concept uses its resolved IRI, relates to
its definition occurrence and public page, and each occurrence relates to its
canonical concept and source document. JSON-LD is produced only through
`RdfRenderer` with SmartDox, glossary, Dublin Core, and Schema.org context.

Every emitted BoK-ready occurrence record contains exactly: deterministic
occurrence identifier, canonical concept IRI, authored surface form, BCP-47
locale, occurrence kind (`definition` or `reference`), resolution kind
(`explicit`), source path, and actual source location. The identifier is derived
only from the explicit source-document IRI, kind, and encounter order.
Locationless resolved entries emit no occurrence record.

## Implementation status

The parser, AST, namespace resolution, and explicit term resolution are stable
Phase 2 grammar. Display and speech projection are stable Phase 3 behavior;
RDF/JSON-LD and BoK occurrence projection are stable Phase 4 behavior. The
Phase 5 compatibility semantics fix legacy `<dfn>` without `about` as non-RDF
and keep RDF references explicit only.
