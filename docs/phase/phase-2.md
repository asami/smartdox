# Phase 2: RDF Term Syntax and Resolution

Status: closed

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
executable specifications cover accepted and rejected inputs, focused and full
validation pass, and the reviewed release tree is committed. Documentation
alone is a prerequisite, not a Phase result.

## Completed Work

- `TERM2-01` through `TERM2-03` delivered one vertical parser, AST, and
  resolver slice for source-located explicit RDF terminology forms.
- The accepted grammar admits an absolute HTTP(S) IRI or a CURIE declared in
  `HEAD` `term_namespaces`; other URI schemes and malformed or unresolved
  references produce deterministic diagnostics.
- Definitions retain a local `id` anchor independently from their optional
  `about` resolver input. Explicit references are resolved before any
  automatic surface matching.
- The RDF terminology contract recorded in
  `docs/journal/2026/08/2026-08-18-phase-2-rdf-terminology-contract-provisional.md`
  is a historical prerequisite. It does not claim parser support or Phase
  completion.

## Phase Closure

Stage Status:

- Current status: CLOSED
- Owner: SmartDox
- Update rule: reopen only through a new authoritative phase decision;
  display, output, compatibility, and downstream acceptance work remain
  separate successor phases.
- Checklist basis: `TERM2-01` through `TERM2-04`.

Closure evidence:

- The parser, AST, resolver, specification, and documentation increments were
  committed as coherent reviewed Steps.
- Focused parser and resolver validation passed before source Step acceptance.
- The Phase full review found no Current Boundary Blockers and retained the
  existing hygiene and deferred-work records without expanding Phase 2.
- The final release gate runs the full SmartDox test suite from this frozen
  release tree before accepting this closure record.
- Phase 2 introduces no display/speech, RDF/JSON-LD/BoK occurrence output,
  compatibility migration, or SimpleModeling.org mutation; those boundaries
  remain Phases 3 through 6.

## References

- `docs/phase/phase-2-checklist.md`
- `docs/design/rdf-grounded-terminology.md`
- `docs/spec/rdf-grounded-terminology.md`
