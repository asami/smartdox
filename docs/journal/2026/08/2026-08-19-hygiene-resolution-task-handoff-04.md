# Hygiene Resolution Task Handoff

Status: SUPERSEDED
Created: 2026-08-19
Source Repository: /Users/asami/src/dev2025/smartdox
Target Repository: /Users/asami/src/dev2025/smartdox
Superseded By: `docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md`

## Purpose

Split the oversized line-parser implementation by responsibility while
preserving line-to-inline parsing behavior and source locations.

## Included Hygiene

| ID | Source | Evidence | Target | Risk | Required outcome |
| --- | --- | --- | --- | --- | --- |
| HYG-004 (`P2-HYG-004`) | `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md` | `DoxLinesParser.scala` was 1,669 lines and its line-to-inline transitions are coupled to unrelated line grammars. | `src/main/scala/org/smartdox/parser/DoxLinesParser.scala` and extracted line-parser sources | Parser/location regression | Reduce source-size debt without altering logical-line parsing or propagated locations. |

## Frozen Boundary

- Allowed repositories: `/Users/asami/src/dev2025/smartdox`.
- Allowed target programs/files: `DoxLinesParser.scala`, newly extracted line-parser Scala sources, and tests necessary to preserve current behavior.
- Allowed behavior change: none.
- Prohibited expansion: inline-tag grammar changes, source-location semantic changes, list-parser completion, and RDF terminology behavior changes.

## Required Validation

1. Run focused Dox2 line/parser and RDF-term resolver specifications.
2. Confirm responsibility extraction and source-size result through repository static inspection.
3. Run the full validation required by `cncf-goal-task` before resolving this item.

## Completion Contract

- Resolve HYG-004 or report it unchanged with evidence.
- Update the source Hygiene record with commit and validation evidence.
- Change the source status to RESOLVED only after accepted validation and commit.
- Do not absorb Development Candidates or unrelated Hygiene.

## Dependencies and Ordering

None.

## Non-goals

- Completing commented or unimplemented list-parser branches.
- Implementing Phase 3 or later behavior.
