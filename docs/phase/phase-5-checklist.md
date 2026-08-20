# Phase 5 Checklist: RDF Term Compatibility

This checklist is the authoritative progress ledger for Phase 5.

Phase Status: IN PROGRESS

Representative acceptance `TERM5-COMPAT-VAL-001` initially failed because generic
visitor dispatch rejected supported compatibility markup before site construction.
Focused validation subsequently passed: representative invocation
`38805-20260820T201026Z` passed `DoxSiteSpec` (15/0), and accumulator invocation
`39673-20260820T201108Z` passed `RdfTermResolverSpec` plus `DoxSiteSpec` (22/0);
both completed with `sbt_exit=0`, `wrapper_exit=0`, and `lock=released`. The
bounded repair remains in progress. Step review, Step commit, full Phase review,
final full suite, and Phase closure remain pending. Phase 6 remains separate and
excluded from this Phase.

## TERM5-01: Preserved Inputs

Status: DONE

- [x] Preserve existing `<dfn>` behavior when `about` is absent.
- [x] Preserve unambiguous automatic glossary links.
- [x] Preserve `span strategy="stable"` migration behavior.

## TERM5-02: Precise Suppression

Status: DONE

- [x] Make `<noterm>` suppress terminology resolution without changing the
      surrounding inline content meaning.
- [x] Keep ambiguous or ordinary-language terms unlinked unless resolution is
      explicitly permitted.

## TERM5-03: Executable Acceptance

Status: DONE

- [x] Add executable regression specifications for all preserved and
      suppressed paths.
- [x] Run the focused compatibility validation successfully.
- [x] Run `git diff --check` successfully.
