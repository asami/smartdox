# RDF Graph Literal Label Projection Design

Status: normative Phase 8 design
Date: 2026-08-23

## Design decision

SmartDox will correct empty RDF literal display labels at the
`DoxSiteDashboard` RDF graph display-projection boundary. The projection will
derive a nonempty RDF lexical-form label from the literal's existing lexical
value, datatype IRI, and language tag. It will not mutate the source RDF model
or alter RDF meaning.

The normative output contract is defined in
[`docs/spec/rdf-graph-literal-label-projection.md`](../spec/rdf-graph-literal-label-projection.md).

## Projection algorithm

The existing node-type dispatch remains the ownership boundary:

- URI nodes keep their current short-IRI label;
- blank nodes keep their current identifier label; and
- literal nodes keep their current nonempty lexical-value label, including the
  existing 80-character truncation (`77` characters plus `...`).

Only the empty branch for a literal changes. It starts with `""`, appends the
datatype as `^^<datatype-IRI>` when present, and appends the language as
`@<language>` when present. The append order is always datatype first and
language second, including when both optional values are carried by the same
model node. Consequently, an empty plain literal is `""`, while the combined
case is `""^^<datatype-IRI>@<language>`.

The values are read exactly as represented by the RDF model. The projection
does not trim lexical values, normalize language tags, rewrite datatype IRIs,
infer missing metadata, or choose a human-language fallback.

## Boundary and invariants

This design constrains only the display label produced for RDF graph nodes by
`DoxSiteDashboard`. Node IDs continue to be derived by the existing node-ID
projection. Graph triples are selected and ordered by the existing graph
projection. JSON-LD and Turtle serializers continue to consume the unchanged
RDF graph. URI, blank-node, and nonempty-literal labels are unchanged.

The serialized graph must contain a nonempty label for every node. For URI and
blank nodes this is inherited from their existing RDF identifiers and label
rules; for literals it is guaranteed by the nonempty branch or the RDF lexical
fallback above. This is a presentation invariant, not a source-model or RDF
semantic invariant.

The generator-side correction is deliberately compatible with strict Cozy
validation. Cozy must continue rejecting a serialized graph that violates the
nonempty-label contract; this design does not weaken that validation.

## Executable-specification boundary

The next implementation step owns focused Given/When/Then executable
specifications for all four empty-literal combinations and for the preserved
graph invariants. Those specifications and the projection implementation are
tracked separately as `LITERAL8-02`; this design establishes their required
behavior before code.

## Non-goals

No source-model redesign, RDF identity change, triple rewrite, JSON-LD/Turtle
format change, graph-ordering refactor, Cozy/SimpleModeling edit, site build,
CLI/configuration change, or unrelated dashboard rendering change is part of
this design.
