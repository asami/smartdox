# Phase 10: Markdown Image Admission and PDF Image Semantics

Status: PLANNED; NOT STARTED

Plan date: 2026-08-29

Predecessor: Phase 9.1 closure. This plan does not alter the active Phase 9
or the planned Phase 9.1 boundary.

## Goal

Make ordinary Markdown image syntax and established SmartDox image syntax
resolve to the same SmartDox image model. The accepted model must preserve
alt text and source-relative resource identity for all supported renderers,
without leaking Markdown punctuation into rendered output.

## Origin

Cozy Phase 39 explicitly deferred Markdown image admission to SmartDox. Cozy
continues to invoke SmartDox PDF generation; it must not add a duplicate
Markdown preprocessor or an image-model workaround.

## Stages

### MDIMG10-01: Image Grammar and Model Contract

Stage Status:
- Current status: NOT STARTED
- Owner: SmartDox parser and document model
- Update rule: complete only when the accepted forms, normalized model,
  compatibility rules, and diagnostics are specified.

- Define `![alt](path)` as a Markdown image form distinct from a Markdown
  hyperlink.
- Define the common `ReferenceImg`-based model shared with existing SmartDox
  image forms, including Japanese alt text and normalized relative paths.
- Define deterministic diagnostics for malformed, unsupported, and missing
  local image resources.
- Preserve existing link and SmartDox image behavior unless explicitly covered
  by the accepted compatibility contract.

### MDIMG10-02: Parser and Renderer Admission

Stage Status:
- Current status: NOT STARTED
- Owner: SmartDox Markdown parser and PDF conversion boundary
- Update rule: complete only when source parsing, model preservation, and
  renderer behavior have focused Executable Specification evidence.

- Parse supported Markdown image syntax into the common image model without a
  leading `!` text node.
- Preserve alt text and source-relative path resolution through PDF conversion.
- Reject unsupported or unavailable local resources before producing a
  misleading PDF artifact.
- Keep non-image Markdown links and existing SmartDox image forms compatible.

### MDIMG10-03: Executable Acceptance and Handoff

Stage Status:
- Current status: NOT STARTED
- Owner: SmartDox Phase 10
- Update rule: complete only when focused and full validation, independent
  review, and the documented handoff are complete.

- Add Given/When/Then executable specifications for Japanese alt text,
  relative paths, invalid and missing resources, punctuation non-leakage, and
  deterministic repeated conversion.
- Verify the common image model through the SmartDox PDF path without a Cozy
  source transformation.
- Run focused and full SmartDox validation, complete independent Phase review,
  and record the accepted SmartDox coordinate for downstream Cozy use.

## Exclusions

- Cozy command help, launcher portability, local configuration adaptation, and
  source preprocessing.
- A new PDF receipt or receipt persistence format. Any such receipt remains a
  separately authorized Cozy contract.
- Article content changes, PDF publication, upload, deployment, and downstream
  consumer acceptance.

## Completion Criteria

Phase 10 completes only when `![alt](path)` and the accepted SmartDox image
forms produce the same image model, Japanese alt text and source-relative
paths are preserved, unsupported or missing resources fail deterministically,
Markdown punctuation does not leak into rendered output, executable evidence
and full validation pass, and independent Phase review closes all Current
Boundary Blockers.

## Structural Phase Plan Gate

State: PROCEED

- planning demand: protected parser and rendering contract
- recommended parent profile: `gpt-5.6-terra / xhigh`
- estimate: 5-7 hours
- split disposition: keep one Phase because grammar, normalized model,
  renderer preservation, and conversion acceptance are one cohesive contract.

## References

- `docs/phase/phase-10-checklist.md`
- `docs/phase/phase-9.1.md`
- `src/main/scala/org/smartdox/parser/DoxInlineParser.scala`
- `src/main/scala/org/smartdox/service/operations/PdfOperationClass.scala`
