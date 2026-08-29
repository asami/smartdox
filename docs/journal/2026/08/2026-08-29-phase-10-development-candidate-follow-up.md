# Phase 10 Development Candidate Follow-up

Status: CLOSED
Date: 2026-08-29

This non-normative ledger preserves the Phase 10 full-review candidate that
required a separate consumer-root design decision. Phase 11 adopted and
delivered that decision; no successor Phase is selected by this closure.

## DEV-P10-01

Status: CLOSED / ADOPTED AND DELIVERED BY PHASE 11

Repository/path: `smartdox`, `src/main/scala/org/smartdox/doxsite/DoxSite.scala`
at the Markdown parser call sites identified by the Phase 10 full review.

Evidence: rootless DoxSite Markdown parser consumers do not establish
`withResourceRoot`. A Markdown image at that boundary therefore rejects
deterministically instead of relying on an implicit current-directory fallback.

Owner/target: SmartDox Phase 11 DoxSite source-root semantics.

Dependency: explicitly select physical and virtual source-root semantics for
site consumer inputs.

Resolution: Phase 11 established explicit physical, virtual, and absent
source-root semantics without a current-directory fallback.

Prohibited workaround: do not add a current-directory fallback or a Cozy/source
preprocessor.

Task/commit: adopted by Phase 11; delivered by its accepted DSROOT11-01 and
DSROOT11-02 Step commits. This final release boundary closes the candidate.
