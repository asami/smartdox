# Phase 7 Checklist: Generic Inline Open-Tag Grammar

This checklist is the authoritative progress ledger for Phase 7.

## TAG7-01: Deferred Open-Tag Paths

Status: OPEN

- [ ] Specify accepted boolean-attribute and self-closing inline-tag forms.
- [ ] Complete `OpenTagState.resultOpenEnd`.
- [ ] Complete the terminal `>` and `/` transitions in `TagAttributeState`.

## TAG7-02: Preserved Grammar

Status: OPEN

- [ ] Preserve quoted-attribute open/close-tag behavior.
- [ ] Preserve parser source locations and deterministic diagnostics for
      unsupported or malformed forms.
- [ ] Do not change RDF term resolution behavior.

## TAG7-03: Executable Acceptance

Status: OPEN

- [ ] Add executable parser specifications for the admitted forms and their
      malformed counterparts.
- [ ] Run focused parser and RDF-term regression validation successfully.
- [ ] Run `git diff --check` successfully.
