# Phase 7: Generic Inline Open-Tag Grammar

Status: planned

## Goal

Complete the deferred generic boolean-attribute and self-closing inline
open-tag paths so supported SmartDox inline tags parse deterministically
without expanding RDF terminology semantics.

## Scope

In scope:

- complete `OpenTagState.resultOpenEnd` for self-closing open tags;
- complete the `>` and `/` branches in `TagAttributeState` for a final
  boolean attribute and self-closing tag form;
- preserve existing quoted-attribute open/close-tag behavior; and
- add executable parser acceptance and rejection coverage for those forms.

Out of scope:

- new RDF term forms, resolution, display, output, or compatibility behavior;
- generic inline-tag schema or rendering-policy changes; and
- the parser-state decomposition recorded as `P2-HYG-002`.

## Dependencies

- Phase 2 is closed; this is its separately admitted `P2-DFW-001` parser
  follow-up.
- This Phase does not activate, merge, or block RDF terminology Phases 3
  through 6.

## Completion Criteria

This Phase completes only when the deferred grammar paths are specified and
implemented, focused parser and RDF-term regression specifications pass, the
full Phase validation passes, and the reviewed release tree is committed.

## References

- `docs/phase/phase-7-checklist.md`
- `docs/journal/2026/08/2026-08-18-phase-2-deferred-work.md`
- `docs/strategy/smartdox-development-strategy.md#9-development-item-status`
