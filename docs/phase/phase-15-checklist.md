# Phase 15 Checklist: Article Header Metadata and Media Actions

This checklist is the authoritative progress ledger for Phase 15. It is not a
normative behavior contract.

Phase Status: PLANNED

## AHEAD15-01: Page Anatomy and Metadata Contract

Stage Status:
- Current status: OPEN
- Owner: SmartDox article-page projection
- Update rule: Close only when every AHEAD15-01 item is checked.

- [ ] Promote the title, metadata strip, media action group, effective LEAD,
      inline infographic, and body ordering into design and specification.
- [ ] Select canonical initial metadata fields and define omission, ordering,
      localization, symbolic-label, accessibility, responsive, and print
      semantics.
- [ ] Preserve existing article-media roles, exact-locale resolution, and
      Notice projection without filename inference or Cozy-specific schema.

## AHEAD15-02: Header Actions and Inline Infographic Projection

Stage Status:
- Current status: OPEN
- Owner: SmartDox DoxSite and article-media projection
- Update rule: Close only when every AHEAD15-02 item is checked and focused
  executable evidence passes on the exact implementation tree.

- [ ] Render available video, summary-slides PDF, article PDF, and infographic
      roles as deterministic localized button-style links below the title.
- [ ] Render reliable compact metadata in a distinct extensible header strip;
      omit absent values and preserve meaningful text when icons or CSS are
      unavailable.
- [ ] Render an accessible registered infographic immediately below the
      effective LEAD, or at the specified no-LEAD fallback, and link to its
      full-size registered asset.
- [ ] Prove partial-media combinations, unavailable-media omission, locale,
      LEAD/no-LEAD placement, metadata, no-media compatibility, keyboard use,
      responsive wrapping, and print representation.

## AHEAD15-03: Common Consumer Acceptance

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 15
- Update rule: Close only when every AHEAD15-03 item is checked and the Phase
  review/full-validation/release gates complete.

- [ ] Prove the same projection with a direct SmartDox fixture and a Cozy BoK
      publication fixture, without a Cozy-only HTML rewrite.
- [ ] Complete mandatory independent Phase review and any bounded convergence
      cycle.
- [ ] Run the final full SmartDox suite on the final release tree.
- [ ] Create the distinct Phase 15 release commit and synchronize the Strategy
      and journal disposition records.
