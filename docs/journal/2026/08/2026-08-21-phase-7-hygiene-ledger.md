# Phase 7 Hygiene Ledger

Status: closed
Date: 2026-08-21

This non-normative, open ledger records observations kept separate from the
Phase 7 generic inline open-tag implementation boundary.

## Current Phase Blockers

- `P7-TAG7-VAL-001` — RESOLVED on 2026-08-21. The final parser-local repair
  retains explicitly accumulated empty generic self-closing nodes without
  changing global `Dox.toDox` normalization. Focused SBT invocation
  `8635-20260821T004057Z` completed with 54 succeeded and 0 failed.
- `CB-P7-FULL-001` — RESOLVED on 2026-08-21. `P7-STEP-REPAIR-003` makes the
  public parser use local result aggregation and completes the nested generic
  attribute/self-closing path while leaving global `Dox.toDox` normalization
  unchanged. Focused SBT invocation `46692-20260821T015827Z` completed with
  56 succeeded and 0 failed; the independent focused closure review converged
  with zero actionable findings.

## Step Repair Exception Decision

- Decision ID: `P7-STEP-REPAIR-001`
- Date: 2026-08-21
- Decision resolution: `AUTHORIZE_SECOND_STEP_REPAIR`.
- Decision: the developer authorized one additional, bounded Step repair for
  `P7-TAG7-VAL-001` after the ordinary test-only repair batch was consumed.
- Boundary: retain generic self-closing tag nodes in the existing
  `DoxInlineParser` result flow, with only directly affected executable specs.
  The repair must not change RDF terminology, tag schemas, rendering,
  diagnostics, parser decomposition, repository scope, public API, or Phase 7
  acceptance criteria.
- Evidence basis: focused SBT invocation `54013-20260820T232854Z` completed
  with 51 succeeded and 2 failed. Both failures demonstrate an empty recovered
  span collection for `<span/>` and `<span enabled/>`; static tracing locates
  loss of the constructed node at `NormalState.returnEndResult`.
- Required validation: rerun the frozen focused selector
  `testOnly org.smartdox.parser.DoxInlineParserSpec org.smartdox.parser.Dox2ParserSpec org.smartdox.semanticweb.RdfTermResolverSpec org.smartdox.semanticweb.SimpleModelingRdfTermAcceptanceSpec`,
  then perform the required focused Step review. No further repair is
  authorized without a new explicit decision.
- Resume state: `REVIEW_FIX`, repair cycle 2.

## Exceptional Step Repair Result

- Decision: `P7-STEP-REPAIR-001`.
- Repair result: `NormalState.returnEndResult` now preserves accumulated nodes
  and buffered text, with an executable mixed-text regression scenario. The
  focused selector nevertheless failed, so this repair does not close
  `P7-TAG7-VAL-001`.
- Validation: SBT invocation `72369-20260820T235105Z` completed with 51
  succeeded and 3 failed, all in the Phase 7 inline-tag acceptance scenarios;
  compilation succeeded and the serial lock was released.
- Root-cause update: `Dox.toDox` applies `_activate`, which deliberately
  filters an empty `Span`. The mixed-text failure retained only
  `before`, `middle`, and `after`, proving that the generic self-closing nodes
  reach aggregation but are discarded by that normalization.
- Required developer decision: the authorized repair boundary did not include
  the normalization policy. No third repair may begin without an explicit
  decision selecting either a parser-local preservation path or a broader
  normalization-policy change.

## Third Step Repair Exception Decision

- Decision ID: `P7-STEP-REPAIR-002`
- Date: 2026-08-21
- Decision resolution: `AUTHORIZE_THIRD_STEP_REPAIR_LOCAL`.
- Decision: the developer authorized one final, bounded Step repair for
  `P7-TAG7-VAL-001`, restricted to a parser-local result aggregation path.
- Boundary: `DoxInlineParser` may preserve empty generic self-closing nodes
  before the global `Dox.toDox` normalization filters them. Direct executable
  parser specs may change only as necessary to prove that preservation.
- Explicit exclusion: do not edit `Dox.scala`, alter the global `_activate`
  normalization policy, add a public API, or change RDF terminology, tag
  schemas, rendering, diagnostics, parser decomposition, whitespace grammar,
  repository scope, or Phase 7 acceptance criteria.
- Required validation: rerun the frozen focused selector
  `testOnly org.smartdox.parser.DoxInlineParserSpec org.smartdox.parser.Dox2ParserSpec org.smartdox.semanticweb.RdfTermResolverSpec org.smartdox.semanticweb.SimpleModelingRdfTermAcceptanceSpec`,
  then complete focused Step review before commit.
- Stop condition: this is the final authorized repair; no further code change
  may begin without a new explicit developer decision.
- Resume state: `REVIEW_FIX`, repair cycle 3.

## Phase Closure Repair Decision

- Decision ID: `P7-STEP-REPAIR-003`
- Date: 2026-08-21
- Decision resolution: `AUTHORIZE_P7_REPAIR_003`.
- Decision: the developer authorized one bounded Phase closure repair after
  the mandatory Phase-full review reported `CB-P7-FULL-001`.
- Boundary: make the public `DoxInlineParser.parse` result preserve empty
  supported generic self-closing nodes without changing global `Dox.toDox`,
  and complete the nested generic-tag path for terminal boolean attributes and
  immediate self-closing forms. Direct parser executable specifications may
  prove the public and nested paths.
- Explicit exclusion: do not edit `Dox.scala`, alter global normalization,
  expand RDF terminology, tag schemas, rendering, diagnostics, public API,
  parser decomposition, whitespace grammar, repository scope, or Phase 7
  acceptance criteria.
- Required validation: rerun the frozen focused selector
  `testOnly org.smartdox.parser.DoxInlineParserSpec org.smartdox.parser.Dox2ParserSpec org.smartdox.semanticweb.RdfTermResolverSpec org.smartdox.semanticweb.SimpleModelingRdfTermAcceptanceSpec`,
  then complete one focused Phase closure re-review before final full
  validation.
- Stop condition: no further code change may begin without a new explicit
  developer decision.
- Resume state: `REVIEW_FIX`, Phase closure repair cycle 1.

## Hygiene

- `HYG-P7-001` — Status: RESOLVED. The focused Step review found that the
  Current Phase Blockers entry still reported `P7-TAG7-VAL-001` as open after
  the final focused validation had passed. This M0 ledger-only update records
  the final evidence; it changes no parser behavior, executable specification,
  acceptance criterion, or Phase 7 boundary.

- `HYG-P7-002` — Status: OPEN, separate-boundary follow-up. The pre-existing
  `plain` scenarios in `DoxInlineParserSpec` lack Given/When/Then structure,
  and `simple` prints without an observable expectation. This was present at
  the Phase base and is not changed by `P7-STEP-REPAIR-003`.

- `HYG-P7-003` — Status: OPEN, separate-boundary follow-up. Pre-existing
  dormant/no-op scenarios and front-loaded Given/When/Then clauses remain in
  `Dox2ParserSpec`. This was present at the Phase base and is outside this
  parser-boundary repair.

Hygiene Triage: HANDED_OFF
Hygiene ID: HYG-P7-002
Handoff Journal: smartdox:docs/journal/2026/09/2026-09-02-hygiene-resolution-batch-handoff.md
Handed Off On: 2026-09-02

## HYG-P7-002 Resolution

Hygiene Status: RESOLVED
Resolution Batch: smartdox:docs/journal/2026/09/2026-09-02-hygiene-resolution-batch-handoff.md
Final Focused Review: CLEAN; active inline-parser scenarios have semantic Given/When/Then boundaries and observable expectations without parser behavior changes.
Final Validation: `sbt --batch test`, invocation `95651-20260901T220319Z` (341 succeeded, 0 failed).
Acceptance Commit: this hygiene-batch acceptance commit.

Hygiene Triage: HANDED_OFF
Hygiene ID: HYG-P7-003
Handoff Journal: smartdox:docs/journal/2026/09/2026-09-02-hygiene-resolution-batch-handoff.md
Handed Off On: 2026-09-02

## HYG-P7-003 Resolution

Hygiene Status: RESOLVED
Resolution Batch: smartdox:docs/journal/2026/09/2026-09-02-hygiene-resolution-batch-handoff.md
Final Focused Review: CLEAN; dormant/no-op scenarios were removed and retained parser actions precede expectations.
Final Validation: `sbt --batch test`, invocation `95651-20260901T220319Z` (341 succeeded, 0 failed).
Acceptance Commit: this hygiene-batch acceptance commit.

Existing Phase 2 hygiene records remain resolved and are not duplicated here.

## Development Candidates

No Development Candidate was admitted. Any future parser decomposition,
whitespace expansion, diagnostic redesign, or broader tag-schema work remains
outside this manifest and requires a separately frozen boundary.
