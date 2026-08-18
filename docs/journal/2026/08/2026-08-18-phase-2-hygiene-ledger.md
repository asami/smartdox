# Phase 2 Hygiene Ledger

Status: closed
Date: 2026-08-18

This ledger records pre-existing maintenance debt identified while delivering
Phase 2.  None of these items is enlarged or repaired by `P2-TERM2-001`.

| ID | Affected source | Evidence and disposition | Follow-up |
| --- | --- | --- | --- |
| P2-HYG-001 | `src/main/scala/org/smartdox/Dox.scala` | 4,699 lines at Step intake, exceeding the 2,000-line high-priority source-size threshold.  The file combines the central Dox AST with many independent element definitions.  A safe split changes shared types and call sites, so this Step only retains the existing Phase-2 AST additions and does not enlarge the debt. | Plan a responsibility-preserving Dox AST split in a later hygiene task. |
| P2-HYG-002 | `src/main/scala/org/smartdox/parser/DoxInlineParser.scala` | 1,663 lines at Step intake, exceeding the 1,500-line ordinary-source limit.  The parser contains multiple state-machine responsibilities.  Extracting states affects parser transition wiring, so this Step changes only the bounded multi-attribute handoff and does not enlarge the debt. | Plan a parser-state decomposition in a later hygiene task. |
| P2-HYG-003 | `src/test/scala/org/smartdox/parser/Dox2ParserSpec.scala` | Existing legacy examples do not consistently use Given/When/Then.  The new Phase-2 parser-pipeline case is Given/When/Then compliant; converting unrelated legacy examples would broaden this semantic slice. | Apply executable-spec style modernization in a dedicated test-hygiene task. |
| P2-HYG-004 | `src/main/scala/org/smartdox/parser/DoxLinesParser.scala` | 1,669 lines when it became a direct source-location propagation target, exceeding the 1,500-line ordinary-source limit.  Its line-to-inline transition is coupled to several unrelated line grammars, so the one-line Phase-2 propagation change must not become a parser rewrite. | Plan a line-parser responsibility split in a later hygiene task. |
| P2-HYG-005 | `src/main/scala/org/smartdox/Dox.scala` | Existing private helpers `toValuesAsInlineContents`, `toValueAsInlineContents`, and `withSummary` do not follow the private `_snake_case` rule.  They predate this Phase-2 AST addition and are outside the parser/resolver behavior change. | Correct these names with all references in a dedicated SmartDox naming-hygiene task. |

Hygiene Triage: HANDED_OFF
Hygiene ID: HYG-001
Handoff Journal: smartdox:docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md
Handed Off On: 2026-08-19

Hygiene Triage: HANDED_OFF
Hygiene ID: HYG-002
Handoff Journal: smartdox:docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md
Handed Off On: 2026-08-19

Hygiene Triage: HANDED_OFF
Hygiene ID: HYG-003
Handoff Journal: smartdox:docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md
Handed Off On: 2026-08-19

Hygiene Triage: HANDED_OFF
Hygiene ID: HYG-004
Handoff Journal: smartdox:docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md
Handed Off On: 2026-08-19

Hygiene Triage: HANDED_OFF
Hygiene ID: HYG-005
Handoff Journal: smartdox:docs/journal/2026/08/2026-08-19-hygiene-resolution-task-handoff-05.md
Handed Off On: 2026-08-19

## HYG-001 Resolution

Hygiene Status: RESOLVED
Resolution Batch: smartdox:docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md
Validated On: 2026-08-19
Validation Evidence: focused `95142-20260818T213358Z`; final full `18734-20260818T222028Z` (`test`, 235 passed)
Acceptance Commit: reported externally after commit

## HYG-002 Resolution

Hygiene Status: RESOLVED
Resolution Batch: smartdox:docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md
Validated On: 2026-08-19
Validation Evidence: focused `86634-20260818T212402Z`, repair `13705-20260818T220748Z`; final full `18734-20260818T222028Z` (`test`, 235 passed)
Acceptance Commit: reported externally after commit

## HYG-003 Resolution

Hygiene Status: RESOLVED
Resolution Batch: smartdox:docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md
Validated On: 2026-08-19
Validation Evidence: focused `97196-20260818T213548Z`, repair `13705-20260818T220748Z`; final full `18734-20260818T222028Z` (`test`, 235 passed)
Acceptance Commit: reported externally after commit

## HYG-004 Resolution

Hygiene Status: RESOLVED
Resolution Batch: smartdox:docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md
Validated On: 2026-08-19
Validation Evidence: focused `92055-20260818T213037Z`, repair `13705-20260818T220748Z`; final full `18734-20260818T222028Z` (`test`, 235 passed)
Acceptance Commit: reported externally after commit

## HYG-002 Closure

Hygiene Status: CLOSED
Closed On: 2026-08-19
Closure Basis: explicit user decision that the `DoxInlineParser` size is
acceptable when its `DoxInlineParseState` default event handling remains
cohesive with its protected transition hooks.
Supersedes: the support-trait extraction accepted in `d6f135a`
Final Structure: `DoxInlineParseState` directly owns its default handling;
there is no parser-state support trait or remaining size-remediation work.

## HYG-004 Closure

Hygiene Status: CLOSED
Closed On: 2026-08-19
Closure Basis: explicit user decision that the `DoxLinesParser` size is
acceptable when `DoxLinesParseState`, `ChildDoxLinesParseState`, and
`NormalState` retain their default transition behavior.
Supersedes: the support-trait extraction accepted in `d6f135a`
Final Structure: each state directly owns its default transition behavior;
there is no line-parser support trait or remaining size-remediation work.

## HYG-005 Resolution

Status: RESOLVED

- Renamed the three private `Dox.scala` helpers and their direct references to
  the required `_snake_case` form without changing public APIs or behavior.
- Focused validation: `testOnly org.smartdox.DoxSpec` passed (1 test) in SBT
  invocation `63762-20260818T204447Z`.
- Full validation: `test` passed (235 tests) in SBT invocation
  `69504-20260818T205446Z`.
- Acceptance commit: `f28a197` (`Hygiene: normalize private Dox helper names`).
