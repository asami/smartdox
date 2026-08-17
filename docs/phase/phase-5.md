# Phase 5: RDF Term Compatibility

Status: planned

## Goal

Preserve supported SmartDox glossary behavior while introducing precise RDF
term controls. Existing documents must remain usable where their behavior is
unambiguous, and ambiguous ordinary-language uses must remain unlinked unless
explicitly identified as terms.

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
