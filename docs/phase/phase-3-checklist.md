# Phase 3 Checklist: RDF Term Display and Occurrence Metadata

This checklist is the authoritative progress ledger for Phase 3.

## TERM3-01: Concept-Keyed Display Metadata

Status: DONE

- [x] Key the rendering metadata by canonical concept IRI.
- [x] Retain localized preferred labels, short labels, aliases,
      abbreviations, scope, and linking policy needed by the renderer.

## TERM3-02: Visible and Spoken Forms

Status: DONE

- [x] Render explicit and safe automatic links from the resolved concept.
- [x] Emit locale-appropriate first-use and abbreviation display forms.
- [x] Produce a separate speech form without rebuilding it from rendered
      HTML or duplicating a visible bilingual annotation.
- [x] Retain the resolved concept IRI in HTML occurrence metadata.

## TERM3-03: Executable Acceptance

Status: DONE

- [x] Add executable display, speech, and occurrence-metadata specifications.
- [x] Run the focused rendering validation successfully.
- [x] Run `git diff --check` successfully.

Evidence:

- Focused SBT invocation `8861-20260820T110654Z` ran `testOnly org.smartdox.semanticweb.RdfTermDisplaySpec`, with 6 succeeded / 0 failed.
- `git diff --check` passed.
