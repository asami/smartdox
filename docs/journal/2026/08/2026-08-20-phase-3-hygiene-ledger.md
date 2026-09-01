# Phase 3 Hygiene Ledger

Status: open
Date: 2026-08-20

This non-normative ledger records the hygiene findings sealed by the Phase 3
TERM3 Step review. During Phase closure, CPB-001 and CPB-002 superseded these
findings as closure repairs; they are not separate hygiene work items.

## Source Review

- Review identity: Phase 3 TERM3 Step `SEALED_LEDGER`
- Base: `eb25e5a`
- Reviewed semantic identity: beginning `39066d2`
- Result: PASS

Review-local provisional HYG-001 and HYG-002 are assigned the canonical IDs
HYG-006 and HYG-007 because HYG-001 through HYG-005 already occur in the
Phase 2 hygiene journal.

| ID | Affected source | Review evidence | Disposition | Follow-up |
| --- | --- | --- | --- | --- |
| HYG-006 | `src/main/scala/org/smartdox/transformers/HtmlTransformerBase.scala:28` | Private method parameter `isPretty` uses camelCase instead of the required private flatcase naming. | Superseded by Phase closure repair CPB-001. | None; addressed by CPB-001. |
| HYG-007 | `src/main/scala/org/smartdox/semanticweb/RdfTermDisplay.scala:22` | Private internal field `hrefValue` uses camelCase instead of the required internal flatcase naming. | Superseded by Phase closure repair CPB-002. | None; addressed by CPB-002. |
| HYG-008 | `src/main/scala/org/smartdox/transformers/Dox2DomHtmlTransformer.scala:194,197,200,203,206,209,212` and `src/main/scala/org/smartdox/transformers/HtmlTransformerBase.scala:20` | Exceptional Phase 3 full review found pre-existing protected final helpers with leading-underscore names, contrary to the protected `snake_case` policy. | Nonblocking hygiene; preserve protected-call compatibility in this Phase. | Dedicated naming-hygiene boundary. |

Hygiene Triage: HANDED_OFF
Hygiene ID: HYG-008
Handoff Journal: smartdox:docs/journal/2026/09/2026-09-02-hygiene-resolution-batch-handoff.md
Handed Off On: 2026-09-02

## HYG-008 Resolution

Hygiene Status: RESOLVED
Resolution Batch: smartdox:docs/journal/2026/09/2026-09-02-hygiene-resolution-batch-handoff.md
Final Focused Review: CLEAN; the cited helpers already use protected snake-case names.
Final Validation: `sbt --batch test`, invocation `95651-20260901T220319Z` (341 succeeded, 0 failed).
Acceptance Commit: this hygiene-batch acceptance commit.

No separate hygiene resolution is recorded here; the Phase closure repairs are
the applicable record for these findings.

## Review Exception Decision

- Decision ID: `P3-FULL-REVIEW-EXCEPTION-001`
- Date: 2026-08-21
- Decision: the developer authorized one exceptional, complete Phase 3 full
  review after the focused closure re-review returned `FULL_REVIEW_REQUIRED`
  solely because its manifest was incomplete.
- Boundary: the existing Phase 3 base through the current closure-fix delta;
  no feature, authority, repository, public-contract, or acceptance expansion
  is authorized.
- Resume state: `REVIEW` with one complete Phase-full review manifest.

## Exceptional Phase Full Review

- Review identity: `eb25e5a..b17a2c9` plus the bounded closure delta.
- Result: FINDINGS (`CPB-003`, `CPB-004`); no third full review is authorized.
- Hygiene admission: `HYG-008` is nonblocking and does not expand Phase 3.
- Required developer decision: the sole Phase Closure Fix Batch was already
  consumed by CPB-001 and CPB-002, so no additional repair can begin without
  an explicit scope/acceptance decision.

## Closure Repair Exception Decision

- Decision ID: `P3-CLOSURE-FIX-EXCEPTION-002`
- Date: 2026-08-21
- Decision: the developer authorized the recommended continuation: one
  additional, bounded closure repair for `CPB-003` and `CPB-004`.
- Boundary: property-based behavioral coverage for the Phase 3 display/link
  projection and version-header corrections in the two files named by the
  sealed full-review ledger. No other behavior, API, repository, authority, or
  acceptance expansion is authorized.
- Resume state: `PHASE_TEST_FIX`.

## Exceptional Closure Repair Result

- Decision: `P3-CLOSURE-FIX-EXCEPTION-002`.
- CPB-003: repaired by the executable property scenario
  `project generated safe absolute HTTP(S) destinations as occurrence links`.
- CPB-004: repaired by updating the `@version` headers in
  `RdfTermDisplay.scala` and `HtmlTransformerBase.scala` to `Aug. 21, 2026`.
- Validation: `testOnly org.smartdox.semanticweb.RdfTermDisplaySpec`, SBT
  invocation `32472-20260820T165224Z`, completed with 7 succeeded / 0 failed;
  `git diff --check` passed before execution.
- Repair class: M2. A fresh focused re-review is required before release, but
  the ordinary Phase closure re-review allowance was consumed already.

## Focused Re-review Exception Decision

- Decision ID: `P3-RE-REVIEW-EXCEPTION-003`
- Date: 2026-08-21
- Decision: the developer authorized one exceptional, focused independent
  re-review of the exact `CPB-003` / `CPB-004` repair delta.
- Boundary: only the repair delta and its directly affected display/link-policy
  edges; it does not authorize another repair, full review, scope expansion,
  or acceptance change.
- Resume state: `RE_REVIEW`.
