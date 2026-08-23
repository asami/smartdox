# RDF Graph Literal Label Projection Specification

Status: normative Phase 8 specification
Date: 2026-08-23

## Purpose and boundary

This specification defines the deterministic display label emitted for each
RDF graph node in the SmartDox site-dashboard projection. The projection is
the graph metadata serialized by `DoxSiteDashboard` for
`metadata/rdf/graph.json`.

This is a display-only contract. It does not change the RDF source model, RDF
node identity, RDF triples, JSON-LD or Turtle serialization, or any consumer's
RDF semantics. The empty-literal correction is generator-side; Cozy
validation remains strict and is not relaxed.

## Node-label contract

For an RDF literal, the model supplies a lexical value and optional datatype
IRI and language tag. The label projection MUST apply these rules in order:

1. A nonempty lexical value MUST retain the present label behavior. Values of
   at most 80 characters are emitted unchanged. Longer values are truncated
   to the first 77 characters followed by `...`, for a maximum label length of
   80 characters. The value is not trimmed or otherwise normalized before
   this decision.
2. An empty lexical value MUST start with the RDF lexical form `""`.
3. When a datatype IRI is present, append `^^<datatype-IRI>`, where
   `<datatype-IRI>` denotes the exact datatype IRI carried by the model.
4. When a language tag is present, append `@<language>`, where `<language>`
   denotes the exact language tag carried by the model.

The resulting empty-value forms are therefore:

| Model value | Required label |
| --- | --- |
| no datatype, no language | `""` |
| language only | `""@<language>` |
| datatype only | `""^^<datatype-IRI>` |
| datatype and language | `""^^<datatype-IRI>@<language>` |

The fourth form is intentional: when both optional values are present, the
datatype suffix MUST precede the language suffix. No locale fallback,
placeholder word, source-path fallback, or registry-order choice is allowed.

The notation `<language>` and `<datatype-IRI>` in this specification denotes
the model values; the datatype suffix retains RDF's angle-bracket IRI form.
An empty language tag or datatype IRI is not manufactured by this projection.

## Preserved graph behavior

Every serialized graph node MUST have a nonempty `label` field. The fallback
above is the only new behavior needed for empty literal lexical values. The
projection MUST preserve all of the following:

- node IDs, including the existing literal ID construction from lexical value,
  datatype, and language;
- graph triples, edge endpoints, predicates, and selected graph limit;
- JSON-LD and Turtle serialization semantics of the RDF source graph;
- graph order, including the existing deterministic triple and node ordering;
- URI labels and their current short-IRI behavior;
- blank-node labels and their current identifier behavior; and
- nonempty literal labels, including the 80-character truncation rule.

The nonempty-label invariant applies to URI and blank nodes through their
existing nonempty RDF identifiers and to literals through the rules above.
This specification does not introduce a new identity or repair an invalid
source RDF identifier.

## Acceptance shape

The implementation and executable specifications admitted by `LITERAL8-02`
must cover Given/When/Then cases for an empty plain literal, an empty
language-tagged literal, an empty typed literal, and a literal carrying both
datatype and language. They must also prove the preserved node, triple,
serialization, order, URI, blank-node, and nonempty-literal invariants.

## Non-goals

This specification does not authorize changes to Scala APIs outside the
`DoxSiteDashboard` display projection, RDF model construction, RDF
serialization, Cozy/SimpleModeling code, build or version configuration, site
generation, or compatibility validation.
