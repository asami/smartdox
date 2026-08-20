# Phase 4 Hygiene Ledger

Status: open
Date: 2026-08-21

This non-normative ledger records the hygiene finding sealed by the Phase 4
full review. The Current Phase Blockers from that review were repaired within
the one admitted closure batch and accepted by the focused closure re-review.

## Source Review

- Review identity: Phase 4 `TERM4-PROJECTION` full review
- Base: `3ad20db`
- Reviewed semantic identity: `9a0e2e9`
- Result: FINDINGS (`CPB-01` through `CPB-03`)

Review-local `HYG-01` is assigned canonical ID `HYG-009`, because
`HYG-001` through `HYG-008` already occur in the Phase 2 and Phase 3 hygiene
ledgers.

| ID | Affected source | Review evidence | Disposition | Follow-up |
| --- | --- | --- | --- | --- |
| HYG-009 | `src/main/scala/org/smartdox/semanticweb/SmartDoxOntology.scala:28-38` | Pre-existing public ontology vals `Document` through `Link` use UpperCamelCase rather than the required public-val camelCase convention. | Nonblocking hygiene; excluded from the RDF-term projection and closure-fix boundary. | Dedicated naming-hygiene boundary with compatibility review. |

## Closure Repair and Re-review

- `CPB-01`: the six newly introduced public RDF-term vocabulary vals use
  camelCase while preserving RDF URI strings and JSON-LD keys.
- `CPB-02`: the ontology header preserves the `Nov. 27, 2025` version-history
  line before its `Aug. 21, 2026` latest version.
- `CPB-03`: emitted RDF source-location literals use `ParseLocation.show`,
  with executable and Dox-contract evidence for the stable bracketed form.
- Focused closure re-review result: PASS; no Current Phase Blocker remains and
  no further full review is required.

The remaining `HYG-009` record is separate follow-up work and does not reopen
or expand Phase 4.
