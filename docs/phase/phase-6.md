# Phase 6: SimpleModeling.org RDF Term Acceptance

Status: closed

## Goal

Prove the bounded SimpleModeling.org terminology cases that exercise the RDF
term contract, without bulk-migrating its glossary or turning SmartDox into a
site-specific implementation.

## Scope

In scope:

- bounded authoritative fixtures for canonical labels, short labels,
  abbreviations, and ambiguous ordinary words;
- Japanese-first visible display and non-duplicated Japanese narration;
- explicit references for technical ownership, composition, and realization;
- a handoff boundary for textus-bok/MCP consumers.

Out of scope:

- bulk editorial glossary migration; and
- Textus BoK/MCP service or storage implementation.

## Completion Criteria

This Phase completes only when the bounded external fixtures exercise the
SmartDox implementation end to end, the corresponding executable acceptance
specifications pass, and the downstream handoff contains RDF-node-bearing
evidence.

The `TERM6-ACCEPTANCE` Step was accepted in commit
`98dc7059fc6230be64fc0794b167136957ebe5bb` (`Phase 6: accept RDF term
integration`). The closure re-review and final full-suite validation passed;
this release commit closes Phase 6 without starting Phase 7. See
`docs/phase/phase-6-checklist.md` and
`docs/journal/2026/08/2026-08-21-phase-6-rdf-term-handoff.md`.

## References

- `docs/phase/phase-6-checklist.md`
- SimpleModeling.org `docs/spec/glossary-entry-format.md`
- SimpleModeling.org `docs/notes/glossary-term-definition-policy.md`
