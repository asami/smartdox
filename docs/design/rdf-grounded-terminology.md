# RDF-Grounded Terminology Design

Status: provisional design — implementation pending
Date: 2026-08-18

## Purpose and boundary

This Phase 2 Stage 2.1 design fixes the terminology identity and authoring
contract for future SmartDox parser, AST, resolution, rendering, and extraction
work. It is not implemented grammar or behavior. Those concerns remain for
Stages 2.2–2.4.

## Canonical identity

The single canonical identity of a glossary concept is an absolute RDF IRI. For
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

term_namespaces = { smterm = "https://www.simplemodeling.org/glossary/" }
```

A CURIE is expanded only through this explicit context. An unknown prefix and a
relative reference are diagnostics. An absolute IRI remains unchanged. Resolver
selection must not depend on label-only matching, NLP, or registry order.

## Ownership and resolution invariant

`<term>` is an explicit reference and resolves before automatic matching.
Ordinary automatic resolution is limited to terms whose bare-label linking
policy is `linkable`. A `context-only` policy requires a deterministic explicit
scope supplied by a later implementation; without it, authors must use
`<term>`. An `explicit-only` policy always requires `<term>`.

Each concept carries localized canonical labels, localized short labels,
aliases, abbreviations, a scope qualifier, its bare-label linking policy, and
relationships to definition, source, and public page. Compatibility between an
authored label and requested label form is validated. Unresolved, duplicate,
ambiguous, unknown-prefix, relative-reference, and label-form compatibility
conditions are diagnostics with source locations.

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

This design does not select parser, AST, resolver, renderer, or extractor
implementation details; it does not create source/page routes; and it does not
introduce registry-order fallback. Executable parser, resolution, and output
specifications are deferred to TERM2-02 through TERM2-04.
