# Phase 9.1: Localized PDF Registry and Article/Notice Projection

Status: PLANNED; NOT STARTED

Plan date: 2026-08-29

Split from Phase 9: 2026-08-29

Predecessor: Phase 9 release closure.

Successor: none in this split.

## Provenance and Dependency

This is the second delivery unit from the approved split of Phase 9. It starts
only after Phase 9 closes and consumes that Phase's accepted design/specification
contract for `article_pdf` and `summary_slides_pdf`, plus its deterministic
locale-selection semantics for bilingual source documents.

Phase 9.1 does not redesign those role or locale semantics. If the predecessor
handoff is absent or contradictory, stop for contract resolution rather than
reconstructing it locally.

## Phase Plan Gate

Phase Plan Gate: PROCEED
- target: approximate-6h packing target; preferred 4–8h band
- planning_demand: bounded-settled
- recommended_parent_profile: gpt-5.6-terra / high
- profile_cost_role: lower-cost execution
- expensive_reasoning_kernel: none; it consumes the accepted Phase 9 contract
- frozen_profile_transition_handoff: accepted Phase 9 PDF-role design/spec,
  locale-selector behavior, and executable evidence
- parent_reasoning_mode_policy: standard
- estimated_at_recommended_profile: 4.0–5.5h; within the preferred band
- merge_attempts_for_every_sub_4h_child: none
- adjacent_merge_structural_rejection_evidence: merging with Phase 9 restores
  the 8.5–10.5h pre-split scope and exceeds the 8h ceiling
- profile_cost_only_rejection_forbidden: true
- short_child_exception: none
- overhead_tradeoff: one extra closure boundary permits settled-contract
  implementation without repeating Phase 9's protected semantic decisions
- agent_reasoning_mode_policy: default standard; consider pro only at an
  eligible agent launch when the active interface supports it and frozen
  quality-first evidence justifies it
- runtime_suitability: re-evaluate in the Phase execution task
- source: approved split from Phase 9

## Goal

Implement the accepted localized PDF-role contract in SmartDox publication
metadata, then project exact-locale article and summary-slides PDF references
to ordinary articles and both global and category-local Notices.

## Scope

In scope:

- extend `PublishMetadata` and its normalized article-media variant with the
  accepted `article_pdf` and `summary_slides_pdf` document roles;
- preserve explicit role identity, site-visible URI, `application/pdf` media
  type, optional label, duplicate handling, absence behavior, and exact-locale
  resolution without filename inference;
- project each available reference into the ordinary article media block and
  the matching global/category Notice media maps;
- omit unavailable roles without empty controls and preserve infographic,
  external-video, site-hosted-video, legacy `VideoPublication`, and no-media
  behavior;
- add Given/When/Then executable specifications for parsing, validation,
  duplicate/absence behavior, exact-locale resolution, and both projections;
- run focused and full SmartDox validation, independent review, and release
  closure; and
- record the accepted SmartDox contract/release handoff for Cozy Phase 40.

Out of scope:

- redefining the PDF-role or locale-selection contract owned by Phase 9;
- summary-slide authoring, PPTX generation, PDF conversion, Cozy receipt
  management, public PPTX distribution, or external publication;
- SimpleModeling.org article content, site-theme design, deployment, or upload.

## Stages

### APDF91-01: Registry and Normalized Model

Stage Status:
- Current status: OPEN
- Checklist basis: APDF91-01
- Owner: SmartDox Phase 9.1
- Update rule: Update when the APDF91-01 checklist state changes.

- Extend registry parsing and the normalized article-media model with the two
  accepted PDF roles.
- Require explicit role identity, site-visible URI, `application/pdf` media
  type, optional label, and exact locale without filename inference.
- Preserve deterministic conflict handling and exact-locale resolution.

### APDF91-02: Article and Notice Projection

Stage Status:
- Current status: OPEN
- Checklist basis: APDF91-02
- Owner: SmartDox Phase 9.1
- Update rule: Update when the APDF91-02 checklist state changes.

- Project each available PDF role into the ordinary article media block.
- Project the same resolved references into global and category-local Notice
  data.
- Omit unavailable roles without empty controls while preserving current
  infographic/video behavior.

### APDF91-03: Executable Acceptance and Cozy Handoff

Stage Status:
- Current status: OPEN
- Checklist basis: APDF91-03
- Owner: SmartDox Phase 9.1
- Update rule: Update when the APDF91-03 checklist state changes.

- Add Given/When/Then executable specifications for parsing, validation,
  locale resolution, article projection, Notice projection, and absence cases.
- Run focused and full SmartDox validation and complete independent Phase
  review before closure.
- Record the accepted SmartDox contract and release coordinate as the input
  dependency for Cozy Phase 40.

## Completion Criteria

Phase 9.1 completes only when both accepted PDF roles are parsed and resolved
by exact locale, ordinary articles and Notices receive the same projected
references, existing media behavior remains compatible, executable
specifications and full validation pass, and the accepted SmartDox handoff is
available to Cozy Phase 40.

## References

- `docs/phase/phase-9.md`
- `docs/phase/phase-9.1-checklist.md`
- `docs/design/article-media-publication.md`
- `docs/spec/article-media-publication.md`
- `src/main/scala/org/smartdox/metadata/PublishMetadata.scala`
- `src/main/scala/org/smartdox/metadata/Notices.scala`
- `src/main/scala/org/smartdox/doxsite/DoxSite.scala`
