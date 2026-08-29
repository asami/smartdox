# Phase 9.1 Checklist: Localized PDF Registry and Article/Notice Projection

This checklist is the authoritative progress ledger for Phase 9.1. It is not a
normative behavior contract.

Phase Status: CLOSED through the final release boundary

Predecessor handoff: accepted Phase 9 PDF-role design/specification contract,
locale-selector behavior, and executable evidence.

## APDF91-01: Registry and Normalized Model

Stage Status:
- Current status: COMMITTED
- Owner: SmartDox Phase 9.1
- Update rule: Update this block from the checklist state below.

- [x] Extend registry parsing and the normalized article-media model with
      `article_pdf` and `summary_slides_pdf`.
- [x] Require explicit role identity, site-visible URI, required
      `application/pdf` media type, optional label, and exact locale without
      filename inference.
- [x] Preserve deterministic duplicate/conflict handling and exact-locale
      resolution.

- APDF91-01 Step commit:
  `5f479beda29355de3a728a4bd145d322547e5181`
  (`feat(metadata): add localized PDF registry roles`).
- Focused validation: receipt `63381-20260829T045340Z`, 24/24 passed through
  serialized SBT.

## APDF91-02: Article and Notice Projection

Stage Status:
- Current status: COMMITTED
- Owner: SmartDox Phase 9.1
- Update rule: Update this block from the checklist state below.

- [x] Project available PDF roles into the ordinary article media block.
- [x] Project the same resolved PDF references into global and category-local
      Notice data.
- [x] Omit unavailable roles without empty controls and preserve current
      infographic, video, legacy `VideoPublication`, and no-media behavior.

- APDF91-02 Step commit:
  `ec0a119b16f1ed95c83ae93c619760e09408903f`
  (`feat(site): project localized PDF media roles`).
- Focused validation: receipt `80513-20260829T052933Z`, 11/11 passed through
  serialized SBT.

## APDF91-03: Executable Acceptance and Cozy Handoff

Stage Status:
- Current status: CLOSED
- Owner: SmartDox Phase 9.1
- Update rule: Update this block from the checklist state below.

- [x] Add Given/When/Then executable specifications for parsing, validation,
      exact-locale resolution, article projection, Notice projection, and
      absence behavior.
- [x] Run focused and full SmartDox validation through serialized SBT.
- [x] Complete independent Phase review and synchronize Phase/checklist ledgers
      before closure.
- [x] Record the accepted SmartDox release coordinate and contract handoff for
      Cozy Phase 40.

- Focused CPB repair validation: receipt `93078-20260829T060229Z`, 36/36
  passed through serialized SBT.
- Mandatory Phase full review finding `CPB-APDF91-001` was resolved by the
  accepted focused closure re-review with `SEALED_LEDGER PASS`.
- The accepted Cozy Phase 40 contract handoff is
  `docs/journal/2026/08/2026-08-29-phase-9.1-cozy-phase-40-handoff.md`.
- Final release validation runs once against these frozen closure bytes before
  the distinct Phase release commit.

Phase 9.1 is CLOSED through this release boundary. Its authoritative closure
requires the final full suite and the distinct release commit. Phase 10 remains
PLANNED and NOT STARTED. No Cozy implementation, publication, push, deployment,
or downstream consumer acceptance is claimed.
