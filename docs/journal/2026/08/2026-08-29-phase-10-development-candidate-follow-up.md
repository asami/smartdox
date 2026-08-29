# Phase 10 Development Candidate Follow-up

Status: OPEN
Date: 2026-08-29

This non-normative ledger preserves the Phase 10 full-review candidate that
requires a separate consumer-root design decision. It is not an implementation
commitment for Phase 10.

## DEV-P10-01

Status: OPEN

Repository/path: `smartdox`, `src/main/scala/org/smartdox/doxsite/DoxSite.scala`
at the Markdown parser call sites identified by the Phase 10 full review.

Evidence: rootless DoxSite Markdown parser consumers do not establish
`withResourceRoot`. A Markdown image at that boundary therefore rejects
deterministically instead of relying on an implicit current-directory fallback.

Owner/target: later SmartDox DoxSite/publication-consumer work; no successor
Phase is selected by this record.

Dependency: explicitly select physical and virtual source-root semantics for
site consumer inputs.

Resume condition: begin only under a separately authorized DoxSite consumer
contract that fixes those source-root semantics.

Prohibited workaround: do not add a current-directory fallback or a Cozy/source
preprocessor.

Task/commit: not admitted to Phase 10.
