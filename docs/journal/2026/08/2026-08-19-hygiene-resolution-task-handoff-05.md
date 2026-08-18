# Hygiene Resolution Task Handoff

Status: COMPLETED
Created: 2026-08-19
Completed: 2026-08-19
Source Repository: /Users/asami/src/dev2025/smartdox
Target Repository: /Users/asami/src/dev2025/smartdox
Suggested Invocation: $cncf-goal-task /Users/asami/src/dev2025/smartdox/docs/journal/2026/08/2026-08-19-hygiene-resolution-task-handoff-05.md

## Purpose

Bring the identified private Dox helpers into the repository naming contract
without altering their behavior.

## Included Hygiene

| ID | Source | Evidence | Target | Risk | Required outcome |
| --- | --- | --- | --- | --- | --- |
| HYG-005 (`P2-HYG-005`) | `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md` | Three pre-existing private helpers in `Dox.scala` do not use `_snake_case`. | `src/main/scala/org/smartdox/Dox.scala` and direct call sites | Internal-reference compatibility | Rename only the three identified private helpers and every direct reference. |

## Frozen Boundary

- Allowed repositories: `/Users/asami/src/dev2025/smartdox`.
- Allowed target programs/files: `Dox.scala` and direct references to `toValuesAsInlineContents`, `toValueAsInlineContents`, and `withSummary`.
- Allowed behavior change: none.
- Prohibited expansion: AST splitting, public API renaming, parser changes, and unrelated naming cleanup.

## Required Validation

1. Run focused specifications covering the renamed helpers' Dox construction paths.
2. Confirm the former private-helper declarations and references are absent from Scala sources.
3. Run the full validation required by `cncf-goal-task` before resolving this item.

## Completion Contract

- Resolve HYG-005 or report it unchanged with evidence.
- Update the source Hygiene record with commit and validation evidence.
- Change the source status to RESOLVED only after accepted validation and commit.
- Do not absorb Development Candidates or unrelated Hygiene.

## Dependencies and Ordering

None. Keep this local rename separate from the AST structural split.

## Non-goals

- Any public or protected API rename.
- Any semantic or parser behavior change.

## Completion Evidence

- The three private helper declarations and direct references were renamed in
  `src/main/scala/org/smartdox/Dox.scala`.
- Focused validation: `testOnly org.smartdox.DoxSpec` passed (1 test),
  invocation `63762-20260818T204447Z`.
- Full validation: `test` passed (235 tests), invocation
  `69504-20260818T205446Z`.
- Acceptance commit: `f28a197` (`Hygiene: normalize private Dox helper names`).
