# RDF-Grounded Terminology Design

Status: stable Phase 2 grammar design
Date: 2026-08-19

## Purpose and boundary

This Phase 2 design fixes the terminology identity and authoring contract for
the implemented SmartDox parser, AST, namespace resolution, and explicit term
resolution. Display and speech projection remain Phase 3 work; RDF/JSON-LD and
BoK occurrence output remain Phase 4 work; compatibility proof remains Phase 5
work.

## Canonical identity

The single canonical identity of a glossary concept is an absolute HTTPS IRI. For
a SimpleModeling.org glossary term instance, the IRI is:

```
https://www.simplemodeling.org/glossary/{relative-glossary-path-without-.dox}
```

For example, `smterm:object-foundation/association` expands to
`https://www.simplemodeling.org/glossary/object-foundation/association`.
The expanded IRI is retained as identity; it is never normalized to a source
path, label, slug, or registry position.

The terminology TBox vocabulary and term instances remain separate:

| Role | Prefix | IRI |
| --- | --- | --- |
| Glossary ontology vocabulary | `smglo` | `https://www.simplemodeling.org/glossary/ontology/0.1-SNAPSHOT#` |
| Glossary concept instances | `smterm` | `https://www.simplemodeling.org/glossary/` |

The concept node, its authored source-document resource, and its public
glossary-page resource are three distinct resources. Source and public page may
relate to the concept, but neither substitutes for its identity. No fixed source
or public-page route is asserted as the canonical term identity.

## Namespace-context invariant

The document `HEAD` declares CURIE namespaces with a HOCON object, for example:

```dox
# HEAD

term_namespaces {
  smterm = "https://www.simplemodeling.org/glossary/"
}
```

A CURIE is expanded only through this explicit context. An unknown prefix and a
relative reference are diagnostics. An absolute HTTP(S) IRI remains unchanged;
a non-HTTP(S) scheme is unsupported and diagnostic.
`RdfTermResolver` reads this explicit `HEAD` metadata, expands declared CURIEs
and HTTP(S) IRIs, and has no bare-label or registry-order fallback.

## Ownership and resolution invariant

`<term>` is an explicit reference that accepts an absolute HTTP(S) IRI or a
CURIE declared in `HEAD` and resolves against definitions or supplied concepts.
The stable grammar does not provide bare-label, NLP, or registry-order fallback.
Authors therefore use an explicit `<term>` reference whenever a concept reference
is required.

`Dox.Term`, `Dox.NoTerm`, and attribute-preserving `Dox.Dfn` are parser-backed
AST values with full source locations. `form` (`canonical`, `short`,
`bilingual`, or `verbatim`) and `dfn` `about` semantics are checked at the
parse-and-resolve layer. `id` is preserved separately as a local anchor; Phase 2
resolves `about` and does not claim resolver validation of `id`. Diagnostics
induced by SmartDox source forms have source locations. They cover missing or
unknown prefixes, malformed, relative, or non-HTTP(S) IRIs, missing `ref`, empty
visible text, unsupported form, unresolved references, incompatible visible form,
and `noterm` nesting. Validation of supplied read-only catalog data, including duplicate
identity or conflicting canonical labels, may have no document location.

## Display and projection invariant

The resolved concept IRI is preserved in every future projection. First visible
Japanese use may be `オブジェクトモデリング（Object Modeling）`; default Japanese
speech is only `オブジェクトモデリング`. A visible abbreviation expansion is not
duplicated in speech.

Future rendered HTML metadata, term-occurrence data, RDF/JSON-LD, and BoK
records carry the resolved concept IRI, surface form, locale, occurrence kind,
resolution kind, and source path/location. This is a normative future output
contract, not a claim about current output.

## Non-goals and follow-up

This design does not create source/page routes or introduce registry-order
fallback. The parser, AST, namespace resolution, and explicit term resolution
are stable Phase 2 grammar. Display and speech projection, RDF/JSON-LD and BoK
occurrence output, and compatibility proof remain separate later-phase work.
