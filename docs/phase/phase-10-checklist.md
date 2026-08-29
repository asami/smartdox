# Phase 10 Checklist: Markdown Image Admission and PDF Image Semantics

This checklist is the authoritative progress ledger for Phase 10. It is not a
normative behavior contract.

Phase Status: CLOSED through the final release boundary

Predecessor: Phase 9.1 closure.

## MDIMG10-01: Image Grammar and Model Contract

Stage Status:
- Current status: COMMITTED
- Owner: SmartDox parser and document model
- Update rule: update this block from the checklist state below.

- [x] Specify `![alt](path)` and its relationship to the existing SmartDox
      image forms.
- [x] Specify the common image model, Japanese alt-text retention, and
      source-relative path normalization.
- [x] Specify deterministic malformed, unsupported, and missing-resource
      diagnostics.
- [x] Specify compatibility for non-image Markdown links and existing SmartDox
      image forms.

- MDIMG10-01 Step commit:
  `64c8d895e86167dff1af69db16bd97175c907a91`
  (`docs(phase10): specify Markdown image admission`).

## MDIMG10-02: Parser and Renderer Admission

Stage Status:
- Current status: COMMITTED
- Owner: SmartDox Markdown parser and PDF conversion boundary
- Update rule: update this block from the checklist state below.

- [x] Parse admitted Markdown image syntax into the common image model.
- [x] Prevent the leading `!` or other source punctuation from leaking into
      rendered output.
- [x] Preserve alt text and source-relative resource resolution through PDF
      conversion.
- [x] Reject unsupported or missing local resources deterministically.
- [x] Preserve existing Markdown link and SmartDox image compatibility.

- MDIMG10-02 Step commit:
  `413d0f501ab4e1d3bd5fc13fd7bd315bcce4a235`
  (`feat(phase10): support Markdown images in PDF output`).
- Focused validation: receipt `27754-20260829T104125Z`, 80 passed through
  serialized SBT.

## MDIMG10-03: Executable Acceptance and Handoff

Stage Status:
- Current status: CLOSED
- Owner: SmartDox Phase 10
- Update rule: update this block from the checklist state below.

- [x] Add Given/When/Then executable specifications for valid, invalid, and
      missing image resources, Japanese alt text, relative paths, and
      punctuation non-leakage.
- [x] Verify deterministic repeated PDF conversion through the SmartDox path.
- [x] Run focused and full SmartDox validation through serialized SBT.
- [x] Complete independent Phase review and record the accepted downstream
      handoff.

- MDIMG10-03 Step commit:
  `9e00eaad9710dd6ba3dcdc8a5d9064843dcb0405`
  (`test(phase10): verify deterministic Markdown image PDF output`).
- Focused validation: receipt `34259-20260829T105633Z`, 35 passed through
  serialized SBT.
- Mandatory Phase full review `P10-FULL-REVIEW-20260829T210000JST` found
  `CPB-P10-01`; the accepted M0 closure correction reconciled the Phase and
  checklist ledgers without changing behavior.
- The accepted downstream handoff is
  `docs/journal/2026/08/2026-08-29-phase-10-cozy-handoff.md`.
- Final release validation runs once against these frozen closure bytes before
  the distinct Phase release commit.

Phase 10 is CLOSED through this release boundary. Its authoritative closure
requires the final full suite and the distinct release commit. No Cozy
preprocessor, PDF receipt contract, publication, upload, deployment, or
downstream consumer acceptance is claimed.
