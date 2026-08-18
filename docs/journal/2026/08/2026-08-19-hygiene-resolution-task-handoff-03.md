# Hygiene Resolution Task Handoff

Status: SUPERSEDED
Created: 2026-08-19
Source Repository: /Users/asami/src/dev2025/smartdox
Target Repository: /Users/asami/src/dev2025/smartdox
Superseded By: `docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md`

## Purpose

Modernize legacy Dox2 parser examples into repository-compliant executable
specifications without changing the parser's behavior.

## Included Hygiene

| ID | Source | Evidence | Target | Risk | Required outcome |
| --- | --- | --- | --- | --- | --- |
| HYG-003 (`P2-HYG-003`) | `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md` | Legacy examples in `Dox2ParserSpec.scala` do not consistently use Given/When/Then. | `src/test/scala/org/smartdox/parser/Dox2ParserSpec.scala` | Specification-preservation | Express existing behavior with Given/When/Then and matchers. |

## Frozen Boundary

- Allowed repositories: `/Users/asami/src/dev2025/smartdox`.
- Allowed target programs/files: `src/test/scala/org/smartdox/parser/Dox2ParserSpec.scala` and narrowly necessary shared specification vocabulary.
- Allowed behavior change: none; preserve the existing asserted parser semantics.
- Prohibited expansion: production parser changes, new grammar, unrelated test rewrites, and RDF-term feature work.

## Required Validation

1. Run the focused Dox2 parser specification.
2. Inspect the target specification for Given/When/Then grouping and compliant private-helper naming.
3. Run the full validation required by `cncf-goal-task` before resolving this item.

## Completion Contract

- Resolve HYG-003 or report it unchanged with evidence.
- Update the source Hygiene record with commit and validation evidence.
- Change the source status to RESOLVED only after accepted validation and commit.
- Do not absorb Development Candidates or unrelated Hygiene.

## Dependencies and Ordering

None.

## Non-goals

- Rewriting parser behavior or broadening fixture coverage.
- Modernizing unrelated specification suites.
