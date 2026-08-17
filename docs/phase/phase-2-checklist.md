# Phase 2 Checklist: RDF Term Syntax and Resolution

This checklist is the authoritative progress ledger for Phase 2.

## TERM2-01: Parsed RDF-Term Forms

Status: OPEN

- [ ] Parse `<term ref>`, `<dfn about>`, and `<noterm>` into distinct,
      source-located AST representations.
- [ ] Preserve an existing `<dfn>` local `id` anchor separately from its
      optional concept resource.
- [ ] Parse explicit namespace declarations, CURIEs, and absolute IRIs.

## TERM2-02: Deterministic Resolution

Status: OPEN

- [ ] Normalize an explicit resource to the canonical concept IRI before
      display or extraction decisions.
- [ ] Resolve explicit references before automatic surface matching.
- [ ] Diagnose unknown prefixes, malformed IRIs, unresolved references,
      duplicate nodes, ambiguous labels, and incompatible selected forms.
- [ ] Never choose a concept by registry insertion order.

## TERM2-03: Executable Acceptance

Status: OPEN

- [ ] Add executable parser and resolver specifications for valid CURIE and
      absolute-IRI references, definitions, and non-term spans.
- [ ] Add executable rejection specifications for each deterministic
      diagnostic class in `TERM2-02`.
- [ ] Run the focused parser/resolution validation successfully.
- [ ] Run `git diff --check` successfully.
