# Phase 5: RDF Term Compatibility

Status: CLOSED

## Goal

Preserve supported SmartDox glossary behavior while introducing precise RDF
term controls. Existing documents must remain usable where their behavior is
unambiguous, and ambiguous ordinary-language uses must remain unlinked unless
explicitly identified as terms.

Phase 5 is closed. Step commit
`312a301b2d06a4f1b97098cf28d18775ab944184` (`Phase 5: preserve RDF term
compatibility`) preserves the completed scope. The independent full Phase review
of `c966d1444ee5ed69deb5d4584b5777ca6861733e..312a301b2d06a4f1b97098cf28d18775ab944184`
returned `SEALED_PHASE_LEDGER PASS` with no Current Boundary Blockers. Final
mandatory full-suite invocation `53586-20260820T203315Z` with logical argv
`["--batch", "test"]` passed 249 succeeded / 0 failed / 4 ignored across 31
suites (`sbt_exit=0`, `wrapper_exit=0`, `lock=released`). Phase 6 remains a
planned, separate, and excluded external-acceptance phase.

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
