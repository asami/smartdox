# Phase 2: RDF Term Syntax and Resolution

Status: active

Start date: 2026-08-18

## Goal

Make an explicit RDF term reference a parsed and resolved SmartDox program
value. Authors must be able to use `<term ref>`, `<dfn about>`, and
`<noterm>` without SmartDox inferring an ambiguous glossary concept from a
surface string.

## Scope

In scope:

- parse the RDF-term source forms and retain source locations;
- represent term references, definitions, and non-term spans in the AST;
- expand CURIEs and normalize absolute IRIs through explicit namespaces;
- resolve explicit references before automatic matching; and
- report unresolved, invalid, duplicate, ambiguous, and label-incompatible
  references deterministically.

Out of scope:

- visible label and narration projection (Phase 3);
- RDF/JSON-LD and BoK occurrence projection (Phase 4);
- legacy compatibility proof (Phase 5); and
- SimpleModeling.org acceptance fixtures (Phase 6).

## Completion Criteria

This Phase completes only when parser and resolver behavior is implemented,
executable specifications cover accepted and rejected inputs, and its focused
validation passes. Documentation alone is a prerequisite, not a Phase result.

## Current Work

- Work A: implement `TERM2-01` through `TERM2-03` as one vertical parser,
  AST, and resolution slice.
- The RDF terminology contract recorded in
  `docs/journal/2026/08/2026-08-18-phase-2-rdf-terminology-contract-provisional.md`
  is a historical prerequisite. It does not claim parser support or Phase
  completion.

## References

- `docs/phase/phase-2-checklist.md`
- `docs/design/rdf-grounded-terminology.md`
- `docs/spec/rdf-grounded-terminology.md`
