# Phase 10 Checklist: Markdown Image Admission and PDF Image Semantics

This checklist is the authoritative progress ledger for Phase 10. It is not a
normative behavior contract.

Phase Status: PLANNED; NOT STARTED

Predecessor: Phase 9.1 closure.

## MDIMG10-01: Image Grammar and Model Contract

Stage Status:
- Current status: NOT STARTED
- Owner: SmartDox parser and document model
- Update rule: update this block from the checklist state below.

- [ ] Specify `![alt](path)` and its relationship to the existing SmartDox
      image forms.
- [ ] Specify the common image model, Japanese alt-text retention, and
      source-relative path normalization.
- [ ] Specify deterministic malformed, unsupported, and missing-resource
      diagnostics.
- [ ] Specify compatibility for non-image Markdown links and existing SmartDox
      image forms.

## MDIMG10-02: Parser and Renderer Admission

Stage Status:
- Current status: NOT STARTED
- Owner: SmartDox Markdown parser and PDF conversion boundary
- Update rule: update this block from the checklist state below.

- [ ] Parse admitted Markdown image syntax into the common image model.
- [ ] Prevent the leading `!` or other source punctuation from leaking into
      rendered output.
- [ ] Preserve alt text and source-relative resource resolution through PDF
      conversion.
- [ ] Reject unsupported or missing local resources deterministically.
- [ ] Preserve existing Markdown link and SmartDox image compatibility.

## MDIMG10-03: Executable Acceptance and Handoff

Stage Status:
- Current status: NOT STARTED
- Owner: SmartDox Phase 10
- Update rule: update this block from the checklist state below.

- [ ] Add Given/When/Then executable specifications for valid, invalid, and
      missing image resources, Japanese alt text, relative paths, and
      punctuation non-leakage.
- [ ] Verify deterministic repeated PDF conversion through the SmartDox path.
- [ ] Run focused and full SmartDox validation through serialized SBT.
- [ ] Complete independent Phase review and record the accepted downstream
      handoff.

Phase 10 is PLANNED and NOT STARTED. It introduces no Cozy workaround, PDF
receipt contract, publication, upload, deployment, or downstream consumer
acceptance.
