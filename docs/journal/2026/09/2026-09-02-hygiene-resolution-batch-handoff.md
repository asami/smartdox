# Hygiene Resolution Batch Handoff

Status: COMPLETE
Created: 2026-09-02
Source Repository: /Users/asami/src/dev2025/smartdox
Target Repositories: /Users/asami/src/dev2025/smartdox
Suggested Invocation: $cncf-goal-hygiene /Users/asami/src/dev2025/smartdox/docs/journal/2026/09/2026-09-02-hygiene-resolution-batch-handoff.md

## Purpose

Resolve the currently recorded, non-behavioral SmartDox hygiene that has a
fully bounded local outcome. The batch preserves parser grammar, site output,
RDF terminology, PDF rendering, public API, and the user's unrelated dirty
journals.

## Baseline

- HEAD: `97c5b3091fce2d2a9d2ce91ca535eef70322bec7`
- Pre-existing unstaged preserve paths:
  - `docs/journal/2026/08/2026-08-24-generic-closing-tag-regression-repair-handoff.md`
  - `docs/journal/2026/08/2026-08-30-phase-11-hygiene-follow-up.md`
- Staged paths: none.
- Applicable instructions: repository `AGENTS.md`; no repository-local rule,
  exception, or agent-guide document is present.

## Included Hygiene

| ID | Source ledger | Work Package | Required outcome |
| --- | --- | --- | --- |
| HYG-008 | `2026-08-20-phase-3-hygiene-ledger.md` | HP-003 | Record that the cited protected helpers already conform; do not change their API. |
| HYG-010 | `2026-08-21-phase-5-hygiene-ledger.md` | HP-002 | Organize `DoxSiteSpec` into feature-level `which` groups without changing scenarios. |
| HYG-011 | `2026-08-21-phase-5-hygiene-ledger.md` | HP-001 | Rename only method-local `LinkEnabler` helpers to `_snake_case_` and update direct uses. |
| HYG-013 | `2026-08-21-phase-5-hygiene-ledger.md` | HP-002 | Add the required blank line after `# Definition` in the cited generated Dox fixture. |
| HYG-015 | `2026-08-21-phase-6-hygiene-ledger.md` | HP-002 | Assert the scanner collections are empty, matching the scenario title. |
| HYG-P7-002 | `2026-08-21-phase-7-hygiene-ledger.md` | HP-002 | Give every active legacy inline-parser scenario explicit Given/When/Then and an observable expectation. |
| HYG-P7-003 | `2026-08-21-phase-7-hygiene-ledger.md` | HP-002 | Remove dormant/no-op parser scenarios and place active parser actions before their Then expectations. |
| HYG-APDF91-002 | `2026-08-29-phase-9.1-hygiene-follow-up.md` | HP-003 | Record the already-accepted `144faca` DoxSite split after current source-size verification. |
| HYG-P10-01 | `2026-08-29-phase-10-hygiene-follow-up.md` | HP-001 | Rename `_listStack` and `_krokiGenerator` to required private `_snake_case` names and update direct uses. |

## Frozen Boundary

- Allowed repository: `/Users/asami/src/dev2025/smartdox`.
- Owned implementation paths:
  - `src/main/scala/org/smartdox/doxsite/LinkEnabler.scala`
  - `src/main/scala/org/smartdox/converters/Dox2LatexConverter.scala`
  - `src/test/scala/org/smartdox/doxsite/DoxSiteSpec.scala`
  - `src/test/scala/org/smartdox/semanticweb/SimpleModelingRdfTermAcceptanceSpec.scala`
  - `src/test/scala/org/smartdox/parser/DoxInlineParserSpec.scala`
  - `src/test/scala/org/smartdox/parser/Dox2ParserSpec.scala`
- Owned journal paths:
  - `docs/journal/2026/08/2026-08-20-phase-3-hygiene-ledger.md`
  - `docs/journal/2026/08/2026-08-21-phase-5-hygiene-ledger.md`
  - `docs/journal/2026/08/2026-08-21-phase-6-hygiene-ledger.md`
  - `docs/journal/2026/08/2026-08-21-phase-7-hygiene-ledger.md`
  - `docs/journal/2026/08/2026-08-29-phase-9.1-hygiene-follow-up.md`
  - `docs/journal/2026/08/2026-08-29-phase-10-hygiene-follow-up.md`
  - this batch journal.
- Preserve paths: every other path, specifically the two baseline unstaged
  journals above.
- Allowed behavior change: none. Existing parser results, grammar,
  diagnostics, source locations, site output, RDF links, PDF output, and
  public/protected API names must remain unchanged.
- Prohibited expansion: public API renames, compatibility shims, source-size
  or responsibility decomposition, feature work, grammar changes, rendering,
  RDF/schema changes, external worktree changes, and unrelated cleanup.

## Excluded Recorded Items

- `HYG-009`: public ontology vals require an explicit compatibility decision.
- `HYG-012`, `HYG-APDF91-001`, and `HYG-P10-02`: responsibility/source-size
  decompositions require dedicated design and executable coverage.
- `HYG-014`: external SimpleModeling.org worktree material has no bounded
  SmartDox ownership in this batch.

## HP-001 — Conform private and method-local naming

- Hygiene IDs: HYG-011, HYG-P10-01.
- Repository: /Users/asami/src/dev2025/smartdox.
- Targets: `LinkEnabler.scala`; `Dox2LatexConverter.scala`; their direct
  reference sites only.
- Allowed repair: rename the six method-local glossary-boundary helpers to
  `_snake_case_`; rename the two private PDF members to `_snake_case`; update
  direct references without changing conditions, data flow, or visibility.
- Prohibited expansion: any glossary matching, PDF, public/protected API, or
  rendering behavior change.
- Focused validation: `testOnly org.smartdox.doxsite.DoxSiteSpec org.smartdox.converters.Dox2LatexConverterSpec`; static reference inspection.
- Dependencies: None.

## HP-002 — Repair executable-spec structure and evidence

- Hygiene IDs: HYG-010, HYG-013, HYG-015, HYG-P7-002, HYG-P7-003.
- Repository: /Users/asami/src/dev2025/smartdox.
- Targets: `DoxSiteSpec.scala`; `SimpleModelingRdfTermAcceptanceSpec.scala`;
  `DoxInlineParserSpec.scala`; `Dox2ParserSpec.scala`.
- Allowed repair: insert `which` grouping only; correct the cited fixture
  whitespace; make existing parser checks explicit Given/When/Then with the
  same asserted results; delete dormant/no-op examples; and inspect the
  existing scanner's three link collections after traversal.
- Prohibited expansion: production changes, new parser grammar or scenarios,
  reactivating dormant examples, fixture-coverage expansion, altered site/RDF
  semantics, or test-framework changes.
- Focused validation: `testOnly org.smartdox.doxsite.DoxSiteSpec org.smartdox.semanticweb.SimpleModelingRdfTermAcceptanceSpec org.smartdox.parser.DoxInlineParserSpec org.smartdox.parser.Dox2ParserSpec`; static executable-spec inspection.
- Dependencies: HP-001.

## HP-003 — Reconcile records already satisfied by accepted work

- Hygiene IDs: HYG-008, HYG-APDF91-002.
- Repository: /Users/asami/src/dev2025/smartdox.
- Targets: the cited Phase 3 and Phase 9.1 hygiene ledgers only.
- Allowed repair: after the final gates, add only the predeclared closure
  fields. HYG-008's cited helpers already have protected snake-case names;
  HYG-APDF91-002 is satisfied by the ancestor acceptance commit `144faca` and
  the current `DoxSite.scala` remains below the recorded 2,555-line debt size.
- Prohibited expansion: changing production code, rewriting prior evidence,
  or including unrelated Phase 11 dirty-journal closure.
- Focused validation: `git blame`/current-source identity review and line
  count for the cited files; final repository gate below.
- Dependencies: HP-002.

## Final Focused Review

- Exact target programs/files: every implementation and test target listed in
  HP-001 and HP-002; the seven owned journals; this batch handoff.
- Required checks: all nine included IDs; exact naming/reference closure;
  source and binary API containment; no parser/site/RDF/PDF behavior change;
  spec grouping and Given/When/Then placement; no active no-op cases; fixture
  spacing; scanner collections; source-record reconciliation; preserve-path
  isolation; and focused-validation evidence.
- Success verdict: `CLEAN`. Any current finding stops the batch without a
  commit and without an automatic repair loop.

## Final Full-Validation Gate

1. `/Users/asami/src/dev2025/smartdox`: `sbt --batch test`

Run exactly once on the reviewed tree through the shared serialized SBT
runner. Stop on failure.

## Completion Contract

- Commit only after the final focused review and full-validation gate pass.
- Update every included source record with `Hygiene Status: RESOLVED`, this
  batch journal, the final validation reference, and the acceptance-commit
  placeholder; make no other semantic journal edit.
- Mark this batch `COMPLETE` only in the accepted committed tree.
- Do not absorb excluded items or newly noticed Hygiene.

## Completion Evidence

- Final focused re-review: `CLEAN`; all nine included Hygiene records converged
  with no Current Boundary Blocker.
- Focused validation: SBT invocation `89789-20260901T215008Z`, 97 succeeded / 0
  failed across five suites.
- Final full validation: `sbt --batch test`, invocation
  `95651-20260901T220319Z`, 341 succeeded / 0 failed / 4 ignored across 34
  suites.
- Acceptance Commit: this hygiene-batch acceptance commit.
