# Phase 15 Checklist: Article Header Metadata and Media Actions

This checklist is the authoritative progress ledger for Phase 15. It is not a
normative behavior contract.

Phase Status: IN PROGRESS

## AHEAD15-01: Page Anatomy and Metadata Contract

Stage Status:
- Current status: ACCEPTED
- Owner: SmartDox article-page projection
- Update rule: Close only when every AHEAD15-01 item is checked.

- [x] Promote the title, metadata strip, media action group, effective LEAD,
      inline infographic, and body ordering into design and specification.
- [x] Select canonical initial metadata fields and define omission, ordering,
      localization, symbolic-label, accessibility, responsive, and print
      semantics.
- [x] Preserve existing article-media roles, exact-locale resolution, and
      Notice projection without filename inference or Cozy-specific schema.

## AHEAD15-02: Header Actions and Inline Infographic Projection

Stage Status:
- Current status: ACCEPTED
- Step commit: `ba7b672c2bc1ba5c50e1b51e724d7b0d5e031a2b`
- Focused validation: receipt `P15-AHEAD15-02-VAL-006` (24 succeeded,
  0 failed)
- Owner: SmartDox DoxSite and article-media projection
- Update rule: Preserve accepted status against the exact Step commit and
  focused receipt; all four implementation items are checked below.

- [x] Render available video, summary-slides PDF, article PDF, and infographic
      roles as deterministic localized button-style links below the title.
- [x] Render reliable compact metadata in a distinct extensible header strip;
      omit absent values and preserve meaningful text when icons or CSS are
      unavailable.
- [x] Render an accessible registered infographic immediately below the
      effective LEAD, or at the specified no-LEAD fallback, and link to its
      full-size registered asset.
- [x] Prove partial-media combinations, unavailable-media omission, locale,
      LEAD/no-LEAD placement, metadata, no-media compatibility, keyboard use,
      responsive wrapping, and print representation.

## AHEAD15-03: Common Consumer Acceptance

Stage Status:
- Current status: IN_PROGRESS
- Owner: SmartDox Phase 15
- Update rule: Close only when every AHEAD15-03 item is checked and the Phase
  review/full-validation/release gates complete. Direct-DoxSite evidence must
  not be represented as actual Cozy BoK fixture acceptance.

- [x] Prove the direct SmartDox DoxSite projection through the physical and
      virtual source-root evidence without a Cozy-only HTML rewrite.
- [x] (Future Development Candidate) DEV-006: Defer actual Cozy BoK fixture
      acceptance to a later explicitly invoked cross-repository Phase, initially
      freezing SmartDox and Cozy, while retaining the same projection and the
      no-Cozy-only-rewrite requirement.
- [ ] Complete mandatory independent Phase review and any bounded convergence
      cycle.
- [ ] Run the final full SmartDox suite on the final release tree.
- [ ] Create the distinct Phase 15 release commit and synchronize the Strategy
      and journal disposition records.
