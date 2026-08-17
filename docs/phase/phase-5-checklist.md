# Phase 5 Checklist: RDF Term Compatibility

This checklist is the authoritative progress ledger for Phase 5.

## TERM5-01: Preserved Inputs

Status: OPEN

- [ ] Preserve existing `<dfn>` behavior when `about` is absent.
- [ ] Preserve unambiguous automatic glossary links.
- [ ] Preserve `span strategy="stable"` migration behavior.

## TERM5-02: Precise Suppression

Status: OPEN

- [ ] Make `<noterm>` suppress terminology resolution without changing the
      surrounding inline content meaning.
- [ ] Keep ambiguous or ordinary-language terms unlinked unless resolution is
      explicitly permitted.

## TERM5-03: Executable Acceptance

Status: OPEN

- [ ] Add executable regression specifications for all preserved and
      suppressed paths.
- [ ] Run the focused compatibility validation successfully.
- [ ] Run `git diff --check` successfully.
