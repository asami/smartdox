# Phase 10: Markdown Image Admission and PDF Image Semantics

Status: CLOSED through the final release boundary

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
- Current status: COMMITTED
- Checklist basis: `MDIMG10-01`
- Owner: SmartDox parser and document model
- Update rule: update when the MDIMG10-01 checklist state changes.
- Evidence: closed in Step commit
  `64c8d895e86167dff1af69db16bd97175c907a91`
  (`docs(phase10): specify Markdown image admission`).

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
- Current status: COMMITTED
- Checklist basis: `MDIMG10-02`
- Owner: SmartDox Markdown parser and PDF conversion boundary
- Update rule: update when the MDIMG10-02 checklist state changes.
- Evidence: closed in Step commit
  `413d0f501ab4e1d3bd5fc13fd7bd315bcce4a235`
  (`feat(phase10): support Markdown images in PDF output`) after focused
  serialized validation passed 80 specifications.

- Parse supported Markdown image syntax into the common image model without a
  leading `!` text node.
- Preserve alt text and source-relative path resolution through PDF conversion.
- Reject unsupported or unavailable local resources before producing a
  misleading PDF artifact.
- Keep non-image Markdown links and existing SmartDox image forms compatible.

### MDIMG10-03: Executable Acceptance and Handoff

Stage Status:
- Current status: CLOSED
- Checklist basis: `MDIMG10-03`
- Owner: SmartDox Phase 10
- Update rule: update when the MDIMG10-03 checklist state changes.
- Evidence: deterministic conversion acceptance closed in Step commit
  `9e00eaad9710dd6ba3dcdc8a5d9064843dcb0405`; its focused serialized
  validation passed 35 specifications. The mandatory Phase full review's
  `CPB-P10-01` progress-ledger finding was resolved by the accepted M0 closure
  correction. The final full suite runs once against this closure tree before
  the distinct release commit.

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

## Closure Evidence and Cozy Handoff

Phase 10 is closed through this release boundary. Its closure becomes
authoritative only when the final full SmartDox suite passes and the distinct
release commit succeeds.

The accepted SmartDox contract and downstream boundary are recorded in
`docs/journal/2026/08/2026-08-29-phase-10-cozy-handoff.md`. Cozy may consume
that release coordinate as an input dependency; no Cozy implementation,
preprocessing, receipt management, publication, upload, deployment, or
downstream consumer acceptance is activated by this closure.

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
