# Phase 8 Checklist: RDF Graph Literal Label Projection

This checklist is the authoritative progress ledger for Phase 8.

Phase Status: CLOSED

## LITERAL8-01: Deterministic Label Semantics

Status: COMPLETE

- [x] Specify deterministic fallback display semantics for empty
      language-tagged literal values.
- [x] Specify deterministic fallback display semantics for empty typed literal
      values.
- [x] Specify deterministic fallback display semantics for empty plain literal
      values.
- [x] Require every generated `metadata/rdf/graph.json` node label to be
      non-empty while preserving ordinary nonempty literal labels.

## LITERAL8-02: SmartDox Projection and Executable Specifications

Status: COMPLETE

- [x] Implement the bounded SmartDox RDF graph projection correction only.
- [x] Add Given/When/Then executable specifications for empty language-tagged,
      typed, and plain literal variants.
- [x] Preserve RDF node IDs, triples, JSON-LD/Turtle semantics, and graph
      ordering.
- [x] Preserve SmartDox 2.4.18-SNAPSHOT status and do not change Scala, tests,
      build, or version/configuration files outside the admitted implementation
      and executable specifications.

## LITERAL8-03: Focused Regression and Downstream Acceptance

Status: COMPLETE

- [x] Run focused SmartDox regression validation for the literal-label
      projection.
- [x] Obtain separately authorized SimpleModeling.org site regeneration, then
      prove every generated graph-node label is non-empty, including
      `literal::en` and `literal::ja`, with a direct all-node scan recording
      totals and empty-label counts.
- [x] Run `cozy bok finalize-metadata` over the regenerated tree and record the
      current isolated finalizer pre/post full inventory, aggregate hash,
      nonallowlist hash, and the explicit metadata-allowlist output boundary;
      the measured allowlist and nonallowlist deltas are both empty for this
      idempotent run.
Nonblocking consumer handoff: hand off the finalized manifest and declared
glossary/RDF children to the managed Textus BoK reader contract as downstream
consumer-integration work; this handoff is not a SmartDox Phase 8 closure item
(see `docs/journal/2026/08/2026-08-24-phase-8-consumer-acceptance-boundary.md`).
- [x] Verify WIP and production wrapper failure paths for missing or
      incompatible BoK metadata on the regenerated output.
- [x] Run `git diff --check` successfully.

Acceptance evidence (2026-08-24): the separately authorized WIP wrapper
(`ce9099d5cf75417f89ce39dd4e136e24bbad047355082c9f38155c3087b900ce`)
exited zero twice, and the production wrapper
(`187f4b664ce22457fef8dfb8d62b5347089d73e443734b0c153e5b2978497bbb`)
exited zero once. Both invoke the public Cozy finalizer and assert the same
four generated resources plus the four required `knowledge-source.json`
declarations. Direct all-node scans found no empty or non-string labels:

- WIP: 254 nodes (2 blank, 89 URI, 163 literal), empty-label count 0.
- Production: 166 nodes (2 blank, 61 URI, 103 literal), empty-label count 0.

The generated roots were all Git-ignored and SimpleModeling.org's tracked and
untracked source inventory was unchanged by the runs. The WIP temporary
publication root was cleaned (`0` matching roots remained). The production
sweep saw 446 `doxsite.d`, 435 `arcadiasite.d`, 1,896 `antora.d`, and 1,251
`website.d` files; its aggregate website hash was
`9e036ce9e53b34bf6129544ab4c978b51efff49a349d169de6dd7ef50f1ed02f`.

The separately authorized isolated finalizer completed successfully with the
following exact invocation and executable evidence:

```text
command: /Users/asami/Library/Application Support/Coursier/bin/cozy bok finalize-metadata /Users/asami/src/dev2025/simplemodeling-org --strategy wip
executable SHA-256: 6757999c70cec4583539e27ef115eea53586eb03c6ca73e4dba3cf104aa2628a
escalation: granted
exit status: 0
stdout: empty
```

The current finalizer run executed only within the explicit generated metadata
allowlist. All three inventories are sorted per-file SHA-256 manifests covering
the whole tree, the allowlist, and the nonallowlist. Before and after full
inventory: 1,251 files, SHA-256
`9e036ce9e53b34bf6129544ab4c978b51efff49a349d169de6dd7ef50f1ed02f`.
Before and after allowlist inventory: 28 files, SHA-256
`a20205ec09646b1c58e2fbc503fbfccdd1adc0b3066c12ff0f0bc4700531ed84`.
Before and after nonallowlist inventory: 1,223 files, SHA-256
`f4c89c48288a7b83eef55a618c088c1d0060375ea1b322bb7241c4895df720b8`.
Therefore the allowlist delta is empty and the nonallowlist delta is empty;
no `.cozy-bok-finalize-*` directory remained. SimpleModeling.org git status
had pre-existing user changes and gained no change from this finalizer run;
those changes are not enumerated here.

The finalizer's explicit output boundary is the Cozy allowlist in
`CozyBokSieMetadata`: 9 individual RDF/metadata files and 6 metadata
subtrees. The production sweep found the 28 present files in that boundary
(two RDF files, six direct metadata files, and 20 files in the allowed
metadata subtrees). The complete isolated-finalizer evidence above closes this
acceptance item.

Acceptance evidence (2026-08-23): the separately authorized
SimpleModeling.org WIP and production wrappers both exited zero using the
SmartDox 2.4.18-SNAPSHOT runtime. The WIP graph had no empty labels and
contained `literal::en = "\"\"@en"` and `literal::ja = "\"\"@ja"`; at that time, the
production scan had no empty labels across 103 literal nodes but did not yet
establish label completeness for every graph node. An isolated Cozy finalizer
run exited zero and left no `.cozy-bok-finalize-*` stage. The
finalized production manifest declared 20 existing, nonempty resources. In
isolated copies of the
same-hash wrappers, missing glossary metadata made WIP exit 1 with the Cozy
handoff diagnostic, and schema `smartdox.rdf-graph.v0` made production exit 1
with the incompatible-graph diagnostic. The managed Textus reader remains an
open, nonblocking downstream consumer handoff and is not a SmartDox Phase 8
closure item.

Reader dependency stop record (2026-08-23; nonblocking downstream consumer
handoff): the finalized production source
was read-only at manifest/glossary/graph SHA-256
`70b2bef1b44e9241fceae88f41c4b032ff8f1d7aab16bd7f58e084a5ffc6b4df`,
`278ee3695051b40c3c7ab8821d1b5c0a5a60a4db3492409b86aea85bb3697104`, and
`4f4d037f451178dc4b35301e231c4bec24858cbd943c27928c2bfe507cb72211`.
Its 166-node/500-edge Cozy graph was presented to a no-build, loopback-only
Textus BoK reader attempt. The script's default `0.5.1-SNAPSHOT` runtime could
not load the current CARs; the declared `0.5.2-SNAPSHOT` runtime instead
rejected startup with HTTP 403 because controlled subsystem execution requires
fixed-user or authenticated-user wiring and an explicit runtime test descriptor.
The existing `check-bok-profile-selection-sar.sh` then passed unchanged on
`0.5.2-SNAPSHOT` with its descriptor-owned fixed local subject: its loopback
REST, Web, and MCP profile reads and negative isolation probes reported
`BOK_PROFILE_SELECTION_SAR_OK` and `BOK_PROFILE_SELECTION_SAR_LIFECYCLE_OK`.
That proves the current managed reader runtime can operate under its existing
private fixed-user configuration, but its four fixed fixtures do not bind this
SimpleModeling.org `website.d`; it is not evidence that the finalized source
has been read or accepted.
No SmartDox, Cozy, SimpleModeling.org, Textus BoK, SIE, or Scraper source was
changed. This is already owned by Textus BoK Phase 8 `P8-A`/`P8-C` together
with its CNCF Phase 70 activation dependency; resume this item only with that
managed reader descriptor and its fixed/authenticated test-user wiring.

Scope-transfer record (2026-08-23): Cozy Phase 29 `BM29-03` is closed on the
bounded Cozy finalization contract. This SmartDox stage owns the separately
authorized generated-site and consumer acceptance evidence above. Focused
`DoxSiteDashboardSpec` validation passed (6/6) and independent review
converged. The existing SimpleModeling.org `doxsite.d` graph remains a
pre-Phase-8 artifact; it is not evidence of this implementation until a new
site generation is authorized and completed.

## LITERAL8-04: Strategy and Hygiene Ledger Synchronization

Status: COMPLETE

- [x] Reconcile the strategy display with the authoritative Phase 7 closed and
      Phase 8 closed statuses; the phase-index display is likewise reconciled.
- [x] Create the durable Phase 8 Hygiene ledger records for `HYG-8-01` and
      `HYG-8-02`, and resolve their narrow documentation-status corrections at
      this closure/release boundary.

## Closure Evidence

- Accepted Phase implementation Step commit:
  `b84e7959011d1989a0d5ad660cf808eebb85268b` (`Phase 8: project deterministic
  RDF literal labels`).
- Mandatory Terra/high Phase full review returned only
  `CB-P8-FULL-001`.
- One authorized documentation-only closure correction and focused re-review
  resolved that finding with `SEALED_LEDGER PASS`.
- The frozen final release suite uses logical argv `["--batch", "test"]` and
  must pass before this release commit; no run result or release commit hash
  is asserted here.

Phase 8 is CLOSED with LITERAL8-01 through LITERAL8-04 complete. This closure
becomes authoritative only if the frozen final release suite passes and the
release commit succeeds; no successor Phase is activated. Textus BoK reader
acceptance and the CNCF Phase 70 activation remain nonblocking downstream
handoffs. The managed reader acceptance remains a downstream consumer-boundary
decision and is not a Phase 8 closure item.
