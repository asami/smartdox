# Phase 13 Checklist: Parser and PDF Operation Responsibility Decomposition

This checklist is the authoritative progress ledger for Phase 13.  It is not a
normative behavior contract.

Phase Status: OPEN

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
- Current status: OPEN
- Owner: SmartDox PDF operation and renderer collaborators
- Update rule: Mark an item complete only after package-visible seam
  compatibility, focused validation, and independent Step review pass.

- [ ] Preserve `PdfOperationClass` operation/command/result/renderer and
      package-visible seam compatibility while extracting PDF input/workspace
      responsibilities.
- [ ] Preserve locale/site projection and renderer-invocation responsibilities
      without changing renderer process or structured-diagnostic behavior.
- [ ] Prove input-root, image, locale, Site-link, renderer, and failure-order
      behavior with focused executable specifications.

## DECOMP13-03: Compatibility Acceptance and Closure

Stage Status:
- Current status: OPEN
- Owner: SmartDox Phase 13
- Update rule: Close only after all prior checklist items are checked, the
  mandatory Phase review is accepted, final full validation passes, and the
  distinct release commit succeeds.

- [ ] Complete independent Step reviews and focused validation for each
      accepted extraction.
- [ ] Complete mandatory independent Phase review and any bounded convergence
      cycle.
- [ ] Run the final full SmartDox suite on the final release tree.
- [ ] Create the distinct Phase 13 release commit and synchronize the Strategy
      and hygiene disposition records.
