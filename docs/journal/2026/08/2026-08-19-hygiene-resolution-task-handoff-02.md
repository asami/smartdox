# Hygiene Resolution Task Handoff

Status: SUPERSEDED
Created: 2026-08-19
Source Repository: /Users/asami/src/dev2025/smartdox
Target Repository: /Users/asami/src/dev2025/smartdox
Superseded By: `docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md`

## Purpose

Decompose the oversized inline-parser state implementation without changing
the supported inline grammar.

## Included Hygiene

| ID | Source | Evidence | Target | Risk | Required outcome |
| --- | --- | --- | --- | --- | --- |
| HYG-002 (`P2-HYG-002`) | `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md` | `DoxInlineParser.scala` was 1,663 lines and combines multiple state-machine responsibilities. | `src/main/scala/org/smartdox/parser/DoxInlineParser.scala` and extracted parser-state sources | State-transition regression | Reduce source-size debt while preserving inline parser transitions and results. |

## Frozen Boundary

- Allowed repositories: `/Users/asami/src/dev2025/smartdox`.
- Allowed target programs/files: `DoxInlineParser.scala`, newly extracted parser-state Scala sources, and tests required to preserve existing behavior.
- Allowed behavior change: none.
- Prohibited expansion: completing deferred generic boolean/self-closing grammar, changing RDF resolution, or broad line-parser refactoring.

## Required Validation

1. Run focused inline-parser and RDF-term resolver specifications.
2. Confirm parser-state extraction and source-size result through repository static inspection.
3. Run the full validation required by `cncf-goal-task` before resolving this item.

## Completion Contract

- Resolve HYG-002 or report it unchanged with evidence.
- Update the source Hygiene record with commit and validation evidence.
- Change the source status to RESOLVED only after accepted validation and commit.
- Do not absorb Development Candidates or unrelated Hygiene.

## Dependencies and Ordering

Phase 7 owns generic boolean and self-closing tag behavior; this task must not
implement it.

## Non-goals

- Adding or changing inline-tag grammar.
- Phase 3 display, speech, or occurrence metadata.
