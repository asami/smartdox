# Phase 13: Parser and PDF Operation Responsibility Decomposition

Status: CLOSED THROUGH FINAL RELEASE BOUNDARY

Plan date: 2026-09-08

Predecessor: Phase 12 release closure
`7b88b63ec1728d0a3a8eac55c94dce13cb9f4560`.

Authoritative responsibility boundary:

- `docs/design/parser-pdf-responsibility-decomposition.md`
- `docs/spec/parser-pdf-decomposition-compatibility.md`

## Goal

Separate the cohesive parser and PDF-operation responsibilities recorded by
`HYG-P10-02`, while preserving all established observable behavior and facade
identities.  The linked design and specification define the stable boundary;
this page is the Phase work ledger.

## Origin

Phase 10 full review recorded `DoxInlineParser.scala`, `Dox2Parser.scala`, and
`PdfOperationClass.scala` as a parser/PDF responsibility decomposition
candidate.  Later Phase 12 work extracted renderer-process execution but did
not admit the remaining parser/PDF boundary into that diagnostic Phase.

## Phase Plan Gate

Phase Plan Gate: PROCEED

- target: approximately 6–8 hours at the selected profile;
- planning demand: protected parser-state, source-origin, diagnostic, and
  package-visible PDF seam preservation;
- accepted profile: user-selected `gpt-5.6-terra / xhigh`;
- split disposition: retain one Phase because each extraction preserves one
  parser-to-PDF compatibility surface and its focused executable evidence;
- no implementation, validation, publication, deployment, or commit is
  authorized by this plan alone.

## Scope

In scope:

- decompose the internal responsibilities of `DoxInlineParser` and
  `Dox2Parser` behind their existing facades;
- decompose PDF input/workspace, locale/site projection, and renderer
  invocation behind `PdfOperationClass` and `PdfRendererExecution`;
- preserve the established Document Project public-URL flattening by exposing
  `xxx.dox/index.dox` as logical `xxx.dox` to DoxSite link collection,
  related-link projection, and Antora while retaining the physical source page
  and package metadata;
- preserve or extend only behavior-preserving executable specifications; and
- maintain this Phase ledger and the Strategy record.

Out of scope:

- grammar, metadata, AST, diagnostic, rendering, or CLI behavior changes;
- DoxLinesParser, unrelated DoxSite feature work, and Cozy changes; and
- `PublishMetadata.scala` decomposition (`HYG-APDF91-001`), which is the
  separately planned Phase 14 successor responsibility only.

## Stages

### DECOMP13-01: Inline and Document Parser Extraction

Stage Status:
- Current status: CLOSED
- Owner: SmartDox parser facades and internal parser collaborators
- Update rule: Advance only after the preserved parser facades and focused
  inline/document executable evidence are accepted together.

- Separate package-internal inline-macro recognition/construction,
  document-assembly, and front-matter responsibilities without changing the
  parser entry points or observable grammar/diagnostics.  DECOMP13-01D assigns
  complete-input macro recognition, embedded macro-name lexical
  splitting/validation, and established Site-link or generic `InlineMacro`
  construction to `DoxInlineParserInlineMacro`; all public nested parser-state
  identities remain owned by `DoxInlineParser`.

### DECOMP13-02: PDF Operation Extraction

Stage Status:
- Current status: CLOSED
- Owner: SmartDox PDF operation and renderer collaborators
- Update rule: Advance only after package-visible PDF seams and focused PDF
  executable evidence preserve the existing operation behavior.

- Separate PDF input/workspace, locale/site projection, and renderer-invocation
  responsibilities without changing the PDF operation contract.

### DECOMP13-03: Document Project Effective-Content Compatibility

Stage Status:
- Current status: CLOSED
- Owner: SmartDox DoxSite effective-content collaborators
- Update rule: Advance only after the logical `xxx.dox` identity preserves the
  physical `index.dox` source/package representation, focused DoxSite
  executable evidence, and independent Step review pass together.

- Introduce one package-internal effective-content view for Document Project
  packages and consume it in link collection, related-link projection, and
  Antora generation. Preserve the established flattened public URL and all
  physical source/package metadata without creating a new DoxSite authoring or
  public-URL feature.

### DECOMP13-04: Compatibility Acceptance and Closure

Stage Status:
- Current status: CLOSED THROUGH FINAL RELEASE BOUNDARY
- Owner: SmartDox Phase 13
- Update rule: Close only after every checklist item has exact-tree focused
  evidence, independent Phase review, final full validation, and a distinct
  release commit.

- The mandatory independent Phase review `P13-PHASE-REV-001` accepted the
  frozen `73ce206..557ca5f` compatibility range without a Current Boundary
  Blocker. Its four pre-existing maintenance findings are persisted in the
  Phase Hygiene Ledger; they are not source repairs in this Phase.
- The final full-suite validation `P13-DECOMP13-04-FULL-VAL-001` passed on the
  release tree (378 succeeded, 0 failed; 35 suites completed and 4 ignored).
  The distinct release boundary is authoritative only if its release commit
  also succeeds.

## Completion Criteria

Phase 13 closes through its final release boundary: every Stage checklist item
is carried by the one release commit; the parser, PDF, and DoxSite focused
specifications preserve the linked compatibility contract; and
`P13-PHASE-REV-001` is clean of Current Boundary Blockers; and
`P13-DECOMP13-04-FULL-VAL-001` passed the full SmartDox suite. The closure
remains authoritative only if the distinct release commit succeeds. It does
not claim PublishMetadata (reserved for Phase 14 only), Cozy, publication,
deployment, or downstream-consumer acceptance.
