# Phase 5: RDF Term Compatibility

Status: IN PROGRESS

## Goal

Preserve supported SmartDox glossary behavior while introducing precise RDF
term controls. Existing documents must remain usable where their behavior is
unambiguous, and ambiguous ordinary-language uses must remain unlinked unless
explicitly identified as terms.

Representative acceptance `TERM5-COMPAT-VAL-001` initially failed because generic
visitor dispatch rejected supported compatibility markup before site construction.
Focused validation subsequently passed: representative invocation
`38805-20260820T201026Z` passed `DoxSiteSpec` (15/0), and accumulator invocation
`39673-20260820T201108Z` passed `RdfTermResolverSpec` plus `DoxSiteSpec` (22/0);
both completed with `sbt_exit=0`, `wrapper_exit=0`, and `lock=released`.
Implementation and focused validation are complete. Step review, Step commit,
full Phase review, final full suite, and Phase closure remain pending. Phase 6
remains a separate and excluded external acceptance phase.

## Scope

In scope:

- compatible `<dfn>` input without `about`;
- existing unambiguous automatic glossary linking;
- stable-span migration behavior and `<noterm>` suppression; and
- compatibility diagnostics and regression coverage.

Out of scope:

- new SimpleModeling.org terminology fixtures and external acceptance
  (Phase 6); and
- bulk migration of existing site content.

## Completion Criteria

This Phase completes only when the supported compatibility paths and controlled
non-term paths have executable regression specifications and focused validation
passes.

## References

- `docs/phase/phase-5-checklist.md`
- `docs/spec/rdf-grounded-terminology.md`
