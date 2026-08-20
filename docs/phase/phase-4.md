# Phase 4: RDF Term Knowledge-Graph Projection

Status: closed

Start date: 2026-08-20

## Goal

Emit RDF/JSON-LD and BoK-ready occurrence data that retain the resolved
concept IRI and relate it to its definition, source document, and public
glossary resource without conflating those identities.

## Scope

In scope:

- RDF and JSON-LD term projections;
- occurrence records for downstream BoK consumption; and
- source/document/public-page relations for a concept node.

The accepted implementation covers the frozen projection boundary only; later
compatibility and downstream acceptance work remain separate phases.

Out of scope:

- BoK or MCP service and storage implementation;
- legacy SmartDox compatibility proof (Phase 5); and
- SimpleModeling.org fixture acceptance (Phase 6).

## Completion Criteria

This Phase completes only when one resolved IRI is preserved in generated
RDF/JSON-LD and BoK-ready records, executable projection specifications pass,
and focused validation passes.

## Phase Closure

Stage Status:

- Current status: CLOSED
- Owner: SmartDox
- Update rule: reopen only through a new authoritative phase decision;
  compatibility and downstream acceptance remain separate successor phases.
- Checklist basis: `TERM4-01` through `TERM4-03`.

Closure evidence:

- RDF and JSON-LD preserve the canonical resolved concept IRI while keeping
  the concept, definition occurrence, source document, and public glossary
  page as distinct resources.
- Executable specifications cover RDF graph relations, JSON-LD rendering,
  deterministic BoK-ready occurrence fields, locale behavior, and stable
  source-location literals.
- The Phase full review and accepted focused closure re-review found no
  remaining Current Phase Blocker. The nonblocking `HYG-009` record remains in
  the canonical Phase hygiene ledger.
- Focused projection validation passed before release preparation. The final
  release gate runs the full SmartDox test suite from this frozen release tree
  before accepting this closure record.

## References

- `docs/phase/phase-4-checklist.md`
- `docs/design/rdf-grounded-terminology.md`
- `docs/spec/rdf-grounded-terminology.md`
- `docs/journal/2026/08/2026-08-21-phase-4-hygiene-ledger.md`
