# Phase 9: Localized PDF Contract and Source-Document Selection

Status: IN PROGRESS

Plan date: 2026-08-29

Split date: 2026-08-29

Dependency: Phase 8 release closure. This plan does not activate Phase 9 or
alter the closed Phase 8 boundary.

## Split Record

The original planned Phase 9 was split by the attributable user invocation of
`$cncf-split-phase Phase 9`. No implementation, validation, publication, or
commit record existed when the split was applied.

- Retained identity: Phase 9 owns the public PDF-role contract and explicit
  locale selection for one SmartDox source document.
- Successor: Phase 9.1 owns the registry implementation and the article and
  Notice projections that consume the accepted Phase 9 contract.
- Split reason: the original 8.5–10.5 hour estimate exceeded the preferred
  4–8 hour delivery band; the two resulting Phases are independently closable.
- Added overhead: one planning handoff, review, final validation, and release
  boundary. The handoff lets the registry/projection Phase use the lower-cost
  settled-contract profile without rediscovering PDF/locale semantics.
- Pre-split gate evidence: the former `SPLIT_REQUIRED` gate is historical
  evidence only; it is not the current gate for this retained Phase.

## Phase Plan Gate

Phase Plan Gate: PROCEED
- target: approximate-6h packing target; preferred 4–8h band
- planning_demand: protected-decision
- recommended_parent_profile: gpt-5.6-terra / xhigh
- profile_cost_role: expensive reasoning kernel
- expensive_reasoning_kernel: define the durable PDF-role contract and the
  explicit locale-selection semantics for bilingual source documents
- frozen_profile_transition_handoff: accepted Phase 9 design/spec contract,
  locale-selector behavior, and executable evidence
- parent_reasoning_mode_policy: standard
- estimated_at_recommended_profile: 4.0–5.0h; within the preferred band
- merge_attempts_for_every_sub_4h_child: none
- adjacent_merge_structural_rejection_evidence: merging Phase 9 with Phase
  9.1 restores the 8.5–10.5h pre-split scope and exceeds the 8h ceiling
- profile_cost_only_rejection_forbidden: true
- short_child_exception: none
- overhead_tradeoff: one extra closure boundary preserves a durable contract
  handoff and avoids repeating protected semantic decisions in Phase 9.1
- agent_reasoning_mode_policy: default standard; consider pro only at an
  eligible agent launch when the active interface supports it and frozen
  quality-first evidence justifies it
- runtime_suitability: re-evaluate in the Phase execution task
- source: approved split from Phase 9

## Goal

Define the durable public contract for two localized PDF document roles—one
article PDF and one summary-slides PDF—and add the explicit locale-selection
boundary required to render one language from bilingual SmartDox authority.

The completed contract and locale-selection behavior are the prerequisite
handoff consumed by Phase 9.1; this Phase does not implement registry parsing
or site projection.

## Origin

SimpleModeling.org currently associates localized infographics and videos with
series articles through `article-media-publication`. The requested delivery
surface adds two public PDF documents per locale:

- an article PDF containing the locale-specific article; and
- a summary-slides PDF containing the public article summary deck.

SmartDox owns the PDF-role contract and source-document locale selection. Cozy
remains the producer and registration orchestrator. Phase 9.1 will own the
SmartDox registry consumer schema and site projections.

## Stages

### APDF9-01: Design and Specification Authority

Stage Status:
- Current status: REVIEWED; STEP COMMIT PENDING
- Checklist basis: APDF9-01
- Owner: SmartDox Phase 9
- Update rule: Update when the APDF9-01 checklist state changes.
- Evidence: PDF-role/locale contract has a clean lightweight review and focused
  re-review; Step commit remains pending.

- Define the exact `article_pdf` and `summary_slides_pdf` registry fields and
  their role-bearing document-reference contract for Phase 9.1.
- Specify a site-visible `public_path`, required `application/pdf` media type,
  optional label, duplicate/absence behavior, and exact-locale semantics.
- Specify compatibility invariants for infographic, external-video,
  site-hosted-video, legacy `VideoPublication`, and no-media variants.

### APDF9-02: Locale-Selected Article PDF Input

Stage Status:
- Current status: OPEN
- Checklist basis: APDF9-02
- Owner: SmartDox Phase 9
- Update rule: Update when the APDF9-02 checklist state changes.

- Add an explicit locale selection to single-document PDF generation.
- Project only the selected locale from bilingual SmartDox content while
  preserving locale-neutral content.
- Reject unsupported or malformed locale selection with structured,
  deterministic diagnostics.

### APDF9-03: Executable Contract Acceptance and Handoff

Stage Status:
- Current status: OPEN
- Checklist basis: APDF9-03
- Owner: SmartDox Phase 9
- Update rule: Update when the APDF9-03 checklist state changes.

- Add Given/When/Then executable specifications for the locale selector,
  bilingual source filtering, locale-neutral preservation, and deterministic
  invalid-locale diagnostics.
- Prove Japanese and English article-PDF input is selected from one bilingual
  authority without cross-locale fallback.
- Run focused and full SmartDox validation, complete independent Phase review,
  and hand the accepted role contract and locale behavior to Phase 9.1.

## Exclusions

- `PublishMetadata` registry parsing, normalized PDF reference implementation,
  article projection, Notice projection, and their executable coverage; these
  belong exclusively to Phase 9.1.
- Summary-slide authoring, PPTX generation, PDF conversion, Cozy receipt
  management, public PPTX distribution, site deployment, upload, or external
  publication.
- SimpleModeling.org article content or site-theme design.

## Completion Criteria

Phase 9 completes only when its design/specification authority is accepted,
locale-selected article-PDF input is deterministic, locale-neutral content is
preserved, malformed or unsupported locale values have structured deterministic
diagnostics, executable specifications and full validation pass, and the
accepted contract handoff is available to Phase 9.1.

## References

- `docs/phase/phase-9-checklist.md`
- `docs/phase/phase-9.1.md`
- `docs/design/article-media-publication.md`
- `docs/spec/article-media-publication.md`
- `src/main/scala/org/smartdox/service/operations/PdfOperationClass.scala`
