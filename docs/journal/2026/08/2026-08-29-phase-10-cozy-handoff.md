# Phase 10 SmartDox Markdown Image Contract Handoff

Status: ACCEPTED at the SmartDox release boundary
Date: 2026-08-29

## Release Coordinate

Repository: `smartdox`

Branch: `Scala12`
Release coordinate: the distinct Phase 10 release commit that introduces this
handoff. Its accepted implementation ancestors are:

- `64c8d895e86167dff1af69db16bd97175c907a91` — Markdown image grammar/model
  contract;
- `413d0f501ab4e1d3bd5fc13fd7bd315bcce4a235` — parser and PDF-path admission;
- `9e00eaad9710dd6ba3dcdc8a5d9064843dcb0405` — deterministic PDF conversion
  acceptance specification.

The coordinate is accepted only when the distinct release commit's frozen full
SmartDox suite succeeds.

## Accepted SmartDox Contract

The authority is `docs/design/markdown-image-admission.md` and
`docs/spec/markdown-image-admission.md`.

- Markdown `![alt](path)` is distinct from a Markdown hyperlink and normalizes
  to the established `ReferenceImg` model shared with existing SmartDox image
  forms.
- Japanese alt text and a source-relative normalized local path are preserved
  through the SmartDox PDF path without leaking Markdown punctuation into
  generated output.
- Malformed, unsupported, unavailable, escaping, and missing local resources
  fail deterministically before a misleading PDF artifact is produced.
- Existing Markdown links and established SmartDox image forms remain
  compatible.

## Consumer Boundary

Cozy may consume this accepted SmartDox release coordinate and contract as an
input dependency. It must not add a duplicate Markdown preprocessor, a
current-directory fallback, or a PDF receipt workaround.

This handoff does not authorize Cozy code changes, artifact generation, receipt
management, deployment, upload, publication, or downstream consumer
acceptance. Those actions require their own scoped workflow and validation.
