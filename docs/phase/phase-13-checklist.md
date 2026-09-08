# Phase 13 Checklist: Parser and PDF Operation Responsibility Decomposition

This checklist is the authoritative progress ledger for Phase 13.  It is not a
normative behavior contract.

Phase Status: CLOSED THROUGH FINAL RELEASE BOUNDARY

Predecessor: Phase 12 release closure
`7b88b63ec1728d0a3a8eac55c94dce13cb9f4560`.

## DECOMP13-01: Inline and Document Parser Extraction

Stage Status:
- Current status: CLOSED
- Owner: SmartDox parser facades and internal parser collaborators
- Update rule: Mark an item complete only after the exact facade-compatible
  source extraction, focused validation, and independent Step review pass.

- [x] Preserve `DoxInlineParser` facade/configuration/state compatibility while
      extracting a cohesive internal parser responsibility.
- [x] DECOMP13-01D: accept the package-internal
      `DoxInlineParserInlineMacro` extraction for complete-input recognition,
      embedded macro-name lexical splitting/validation, and Site-link/generic
      macro construction, while `DoxInlineParser` retains every public nested
      state identity.  Focused validation and independent Step review have
      passed.
- [x] Preserve `Dox2Parser` facade/configuration/context compatibility while
      extracting document assembly and metadata/front-matter responsibilities.
- [x] Prove existing inline/document grammar, AST, location, resource-origin,
      metadata, and structured-diagnostic behavior with focused executable
      specifications.

## DECOMP13-02: PDF Operation Extraction

Stage Status:
- Current status: CLOSED
- Owner: SmartDox PDF operation and renderer collaborators
- Update rule: Mark an item complete only after package-visible seam
  compatibility, focused validation, and independent Step review pass.

- [x] Preserve `PdfOperationClass` operation/command/result/renderer and
      package-visible seam compatibility while extracting PDF input/workspace
      responsibilities.
- [x] Preserve locale/site projection and renderer-invocation responsibilities
      without changing renderer process or structured-diagnostic behavior.
- [x] Prove input-root, image, locale, Site-link, renderer, and failure-order
      behavior with focused executable specifications.

## DECOMP13-03: Document Project Effective-Content Compatibility

Stage Status:
- Current status: CLOSED
- Owner: SmartDox DoxSite effective-content collaborators
- Update rule: Mark an item complete only after physical/package preservation,
  focused DoxSite validation, and independent Step review pass.

- [x] Preserve physical `xxx.dox/index.dox` source and package metadata while
      exposing one logical `xxx.dox` effective-content identity.
- [x] Make link collection, related-link projection, and Antora consume that
      identity without emitting a nested Document Project public page.
- [x] Prove relative Site-link mapping, incoming/outgoing relations, locale
      output, and nested Antora sibling paths with `DoxSiteSpec`.

## DECOMP13-04: Compatibility Acceptance and Closure

Stage Status:
- Current status: CLOSED THROUGH FINAL RELEASE BOUNDARY
- Owner: SmartDox Phase 13
- Update rule: Close only after all prior checklist items are checked, the
  mandatory Phase review is accepted, final full validation passes, and the
  distinct release commit succeeds.

- [x] Complete independent Step reviews and focused validation for each
      accepted parser, PDF, and DoxSite compatibility Slice.
- [x] Complete mandatory independent Phase review and any bounded convergence
      cycle. `P13-PHASE-REV-001` found no Current Boundary Blocker.
- [x] Run the final full SmartDox suite on the final release tree as the
      release-bound validation `P13-DECOMP13-04-FULL-VAL-001` (378 succeeded,
      0 failed; 35 suites completed and 4 ignored); this checked state becomes
      authoritative only with the release commit.
- [x] Create the distinct Phase 13 release commit and synchronize closure and
      hygiene disposition records; shared Phase 15 planning projections remain
      explicitly preserved rather than adopted by this Phase.
