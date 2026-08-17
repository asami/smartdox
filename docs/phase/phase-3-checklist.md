# Phase 3 Checklist: RDF Term Display and Occurrence Metadata

This checklist is the authoritative progress ledger for Phase 3.

## TERM3-01: Concept-Keyed Display Metadata

Status: OPEN

- [ ] Key the rendering metadata by canonical concept IRI.
- [ ] Retain localized preferred labels, short labels, aliases,
      abbreviations, scope, and linking policy needed by the renderer.

## TERM3-02: Visible and Spoken Forms

Status: OPEN

- [ ] Render explicit and safe automatic links from the resolved concept.
- [ ] Emit locale-appropriate first-use and abbreviation display forms.
- [ ] Produce a separate speech form without rebuilding it from rendered
      HTML or duplicating a visible bilingual annotation.
- [ ] Retain the resolved concept IRI in HTML occurrence metadata.

## TERM3-03: Executable Acceptance

Status: OPEN

- [ ] Add executable display, speech, and occurrence-metadata specifications.
- [ ] Run the focused rendering validation successfully.
- [ ] Run `git diff --check` successfully.
