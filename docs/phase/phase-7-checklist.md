# Phase 7 Checklist: Generic Inline Open-Tag Grammar

This checklist is the authoritative progress ledger for Phase 7.

## TAG7-01: Deferred Open-Tag Paths

Status: COMPLETED

- [x] Specify accepted boolean-attribute and self-closing inline-tag forms.
- [x] Complete `OpenTagState.resultOpenEnd`.
- [x] Complete the terminal `>` and `/` transitions in `TagAttributeState`.

## TAG7-02: Preserved Grammar

Status: COMPLETED

- [x] Preserve quoted-attribute open/close-tag behavior.
- [x] Preserve parser source locations and deterministic diagnostics for
      unsupported or malformed forms.
- [x] Do not change RDF term resolution behavior.

## TAG7-03: Executable Acceptance

Status: COMPLETED

- [x] Add executable parser specifications for the admitted forms and their
      malformed counterparts.
- [x] Run focused parser and RDF-term regression validation successfully.
- [x] Run `git diff --check` successfully.
