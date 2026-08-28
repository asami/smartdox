# Phase 9 Checklist: Localized PDF Contract and Source-Document Selection

This checklist is the authoritative progress ledger for Phase 9. It is not a
normative behavior contract.

Phase Status: IN PROGRESS

## APDF9-01: Design and Specification Authority

Stage Status:
- Current status: REVIEWED; STEP COMMIT PENDING
- Owner: SmartDox Phase 9
- Update rule: Update this block from the checklist state below.

- [x] Define the `article_pdf` and `summary_slides_pdf` role-bearing contract
      consumed by Phase 9.1.
- [x] Specify site-visible `public_path`, required `application/pdf` media
      type, optional label, duplicate, absence, and exact-locale behavior.
- [x] Specify compatibility with infographic, video, legacy `VideoPublication`,
      and no-media variants.

- APDF9-01 Step commit is pending.

## APDF9-02: Locale-Selected Article PDF Input

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 9
- Update rule: Update this block from the checklist state below.

- [ ] Add an explicit locale selector to single-document PDF generation.
- [ ] Preserve locale-neutral content and select only the requested localized
      content from a bilingual SmartDox source.
- [ ] Add deterministic diagnostics and executable coverage for unsupported
      or malformed locale values.

## APDF9-03: Executable Contract Acceptance and Handoff

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 9
- Update rule: Update this block from the checklist state below.

- [ ] Add Given/When/Then executable specifications for locale selection,
      bilingual source filtering, locale-neutral content, and invalid-locale
      diagnostics.
- [ ] Prove Japanese and English article PDF selection from one bilingual
      authority without cross-locale fallback.
- [ ] Run focused and full SmartDox validation through serialized SBT.
- [ ] Complete independent Phase review and synchronize Strategy, Phase, and
      checklist ledgers before closure.
- [ ] Record the accepted SmartDox PDF-role and locale-selection handoff for
      Phase 9.1.

Phase 9 is IN PROGRESS. Registry parsing and article/Notice
projection moved to `phase-9.1-checklist.md`; no implementation, validation,
publication, push, or downstream consumer acceptance is claimed.
