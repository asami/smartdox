# Phase 9 Checklist: Localized PDF Contract and Source-Document Selection

This checklist is the authoritative progress ledger for Phase 9. It is not a
normative behavior contract.

Phase Status: CLOSED

## APDF9-01: Design and Specification Authority

Stage Status:
- Current status: COMMITTED
- Owner: SmartDox Phase 9
- Update rule: Update this block from the checklist state below.

- [x] Define the `article_pdf` and `summary_slides_pdf` role-bearing contract
      consumed by Phase 9.1.
- [x] Specify site-visible `public_path`, required `application/pdf` media
      type, optional label, duplicate, absence, and exact-locale behavior.
- [x] Specify compatibility with infographic, video, legacy `VideoPublication`,
      and no-media variants.

- APDF9-01 Step commit:
  `fe23fc82936821d2da5c76250be6f1e9d353db10`
  (`docs(pdf): define localized article media contract`).

## APDF9-02: Locale-Selected Article PDF Input

Stage Status:
- Current status: COMMITTED
- Owner: SmartDox Phase 9
- Update rule: Update this block from the checklist state below.

- [x] Add an explicit locale selector to single-document PDF generation.
- [x] Preserve locale-neutral content and select only the requested localized
      content from a bilingual SmartDox source.
- [x] Add deterministic diagnostics and executable coverage for unsupported
      or malformed locale values.

- APDF9-02 Step commit:
  `834823b3118a6e08cf2eae5b4987111382f2dd67`
  (`feat(pdf): select exact localized article source`).
- Focused validation: receipt `23974-20260829T031559Z`, 32/32 passed through
  serialized SBT.

## APDF9-03: Executable Contract Acceptance and Handoff

Stage Status:
- Current status: CLOSED
- Owner: SmartDox Phase 9
- Update rule: Update this block from the checklist state below.

- [x] Add Given/When/Then executable specifications for locale selection,
      bilingual source filtering, locale-neutral content, and invalid-locale
      diagnostics.
- [x] Prove Japanese and English article PDF selection from one bilingual
      authority without cross-locale fallback.
- [x] Run focused and full SmartDox validation through serialized SBT.
- [x] Complete independent Phase review and synchronize Strategy, Phase, and
      checklist ledgers before closure.
- [x] Record the accepted SmartDox PDF-role and locale-selection handoff for
      Phase 9.1.

- Focused repair validation: receipt `39481-20260829T035155Z`, 34/34 passed
  through serialized SBT.
- Mandatory Phase review findings `CB-P9-FULL-001` and `CB-P9-FULL-002` were
  resolved by the accepted focused closure re-review with `SEALED/PASS`.
- Final release validation runs once against these frozen closure bytes before
  the distinct Phase release commit.

Phase 9 is CLOSED through this release boundary. Its authoritative closure
requires the final full suite and the distinct release commit. Registry parsing
and article/Notice projection moved to `phase-9.1-checklist.md`; Phase 9.1
remains PLANNED and NOT STARTED. No publication, push, or downstream consumer
acceptance is claimed.
