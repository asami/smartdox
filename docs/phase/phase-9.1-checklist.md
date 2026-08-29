# Phase 9.1 Checklist: Localized PDF Registry and Article/Notice Projection

This checklist is the authoritative progress ledger for Phase 9.1. It is not a
normative behavior contract.

Phase Status: PLANNED; NOT STARTED

Predecessor handoff: accepted Phase 9 PDF-role design/specification contract,
locale-selector behavior, and executable evidence.

## APDF91-01: Registry and Normalized Model

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 9.1
- Update rule: Update this block from the checklist state below.

- [ ] Extend registry parsing and the normalized article-media model with
      `article_pdf` and `summary_slides_pdf`.
- [ ] Require explicit role identity, site-visible URI, required
      `application/pdf` media type, optional label, and exact locale without
      filename inference.
- [ ] Preserve deterministic duplicate/conflict handling and exact-locale
      resolution.

## APDF91-02: Article and Notice Projection

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 9.1
- Update rule: Update this block from the checklist state below.

- [ ] Project available PDF roles into the ordinary article media block.
- [ ] Project the same resolved PDF references into global and category-local
      Notice data.
- [ ] Omit unavailable roles without empty controls and preserve current
      infographic, video, legacy `VideoPublication`, and no-media behavior.

## APDF91-03: Executable Acceptance and Cozy Handoff

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 9.1
- Update rule: Update this block from the checklist state below.

- [ ] Add Given/When/Then executable specifications for parsing, validation,
      exact-locale resolution, article projection, Notice projection, and
      absence behavior.
- [ ] Run focused and full SmartDox validation through serialized SBT.
- [ ] Complete independent Phase review and synchronize Phase/checklist ledgers
      before closure.
- [ ] Record the accepted SmartDox release coordinate and contract handoff for
      Cozy Phase 40.

Phase 9.1 is PLANNED and NOT STARTED. No implementation, validation,
publication, push, or downstream consumer acceptance is claimed.
