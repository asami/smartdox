# Hygiene Resolution Task Handoff

Status: READY
Created: 2026-08-19
Source Repository: /Users/asami/src/dev2025/smartdox
Target Repository: /Users/asami/src/dev2025/smartdox
Suggested Invocation: $cncf-goal-task /Users/asami/src/dev2025/smartdox/docs/journal/2026/08/2026-08-19-hygiene-resolution-task-handoff-01.md

## Purpose

Split the oversized SmartDox AST source by responsibility while preserving its
public and internal behavior.

## Included Hygiene

| ID | Source | Evidence | Target | Risk | Required outcome |
| --- | --- | --- | --- | --- | --- |
| HYG-001 (`P2-HYG-001`) | `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md` | `Dox.scala` was 4,699 lines and combines the central AST with independent element definitions. | `src/main/scala/org/smartdox/Dox.scala` and responsibility-preserving extracted AST sources | Shared-type/call-site compatibility | Reduce the source-size debt without changing the Dox AST contract. |

## Frozen Boundary

- Allowed repositories: `/Users/asami/src/dev2025/smartdox`.
- Allowed target programs/files: `Dox.scala`, newly extracted Scala AST source files under `src/main/scala/org/smartdox/`, and only call sites required by the split.
- Allowed behavior change: none; preserve source and binary-facing AST semantics.
- Prohibited expansion: parser grammar changes, RDF terminology behavior, rendering changes, and unrelated naming cleanup.

## Required Validation

1. Run focused AST/parser and RDF-term resolver specifications affected by the moved declarations.
2. Confirm the extracted responsibility boundary and source-size result through repository static inspection.
3. Run the full validation required by `cncf-goal-task` before resolving this item.

## Completion Contract

- Resolve HYG-001 or report it unchanged with evidence.
- Update the source Hygiene record with commit and validation evidence.
- Change the source status to RESOLVED only after accepted validation and commit.
- Do not absorb Development Candidates or unrelated Hygiene.

## Dependencies and Ordering

None. Do not combine this structural refactor with the local naming repair.

## Non-goals

- Implementing Phase 3 or later RDF terminology work.
- Changing the public Dox model or parser behavior.
