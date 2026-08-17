# Phase 4: RDF Term Knowledge-Graph Projection

Status: planned

## Goal

Emit RDF/JSON-LD and BoK-ready occurrence data that retain the resolved
concept IRI and relate it to its definition, source document, and public
glossary resource without conflating those identities.

## Scope

In scope:

- RDF and JSON-LD term projections;
- occurrence records for downstream BoK consumption; and
- source/document/public-page relations for a concept node.

Out of scope:

- BoK or MCP service and storage implementation;
- legacy SmartDox compatibility proof (Phase 5); and
- SimpleModeling.org fixture acceptance (Phase 6).

## Completion Criteria

This Phase completes only when one resolved IRI is preserved in generated
RDF/JSON-LD and BoK-ready records, executable projection specifications pass,
and focused validation passes.

## References

- `docs/phase/phase-4-checklist.md`
- `docs/design/rdf-grounded-terminology.md`
- `docs/spec/rdf-grounded-terminology.md`
