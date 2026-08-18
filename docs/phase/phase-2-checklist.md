# Phase 2 Checklist: RDF Term Syntax and Resolution

This checklist is the authoritative progress ledger for Phase 2.

## TERM2-01: Parsed RDF-Term Forms

Status: DONE

- [x] Parse `<term ref>`, `<dfn about>`, and `<noterm>` into distinct,
      source-located AST representations.
- [x] Preserve an existing `<dfn>` local `id` anchor separately from its
      optional concept resource.
- [x] Parse explicit namespace declarations, CURIEs, and absolute HTTP(S)
      IRIs.

## TERM2-02: Deterministic Resolution

Status: DONE

- [x] Normalize an explicit resource to the canonical concept IRI before
      display or extraction decisions.
- [x] Resolve explicit references before automatic surface matching.
- [x] Diagnose unknown prefixes, malformed or non-HTTP(S) references,
      unresolved references, duplicate nodes, ambiguous labels, and
      incompatible selected forms.
- [x] Never choose a concept by registry insertion order.

## TERM2-03: Executable Acceptance

Status: DONE

- [x] Add executable parser and resolver specifications for valid CURIE and
      absolute-HTTP(S)-IRI references, definitions, and non-term spans.
- [x] Add executable rejection specifications for each deterministic
      diagnostic class in `TERM2-02`.
- [x] Run the focused parser/resolution validation successfully.
- [x] Run `git diff --check` successfully.

## TERM2-04: Phase Closure

Status: DONE

- [x] Complete a read-only Phase full review and retain any unrelated hygiene
      or development-candidate records.
- [x] Run the frozen-tree full SmartDox test suite successfully.
- [x] Commit the reviewed Phase 2 release tree.
- [x] Close Phase 2 from these checklist results without starting Phase 3.
