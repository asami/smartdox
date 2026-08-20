# Phase 3: RDF Term Display and Occurrence Metadata

Status: closed

Start date: 2026-08-20

## Goal

Project a resolved concept into locale-aware visible markup, narration text,
and stable HTML occurrence metadata without reconstructing identity from a
rendered label.

## Scope

In scope:

- glossary registry metadata keyed by the resolved concept IRI;
- preferred labels, aliases, abbreviations, scope, and linking policy needed
  for rendering;
- explicit and safe automatic link display;
- first-use bilingual and abbreviation display; and
- separate speech text so visible annotation is not duplicated in narration.

Out of scope:

- RDF/JSON-LD and BoK record emission (Phase 4);
- legacy compatibility acceptance (Phase 5); and
- SimpleModeling.org fixture acceptance (Phase 6).

## Completion Criteria

This Phase completes only when the rendering path preserves the resolved IRI,
the display/speech distinction is executable-specification covered, and focused
rendering validation passes.

## Phase Closure

Stage Status:

- Current status: CLOSED
- Owner: SmartDox
- Update rule: reopen only through a new authoritative phase decision; RDF/JSON-LD
  output, compatibility, and downstream acceptance remain separate successor
  phases.
- Checklist basis: `TERM3-01` through `TERM3-03`.

Closure evidence:

- Display metadata remains keyed by the resolved concept IRI and projects
  localized visible, spoken, link, and occurrence-metadata forms without
  reconstructing identity from labels.
- Executable specifications cover visible/speech separation, first-use forms,
  occurrence metadata, explicit link policy, and generated HTTP(S) link-policy
  inputs.
- The Phase full review and the accepted focused closure re-review found no
  remaining Current Phase Blocker. The nonblocking `HYG-008` record remains in
  the canonical Phase hygiene ledger.
- Focused rendering validation passed before release preparation. The final
  release gate runs the full SmartDox test suite from this frozen release tree
  before accepting this closure record.

## References

- `docs/phase/phase-3-checklist.md`
- `docs/design/rdf-grounded-terminology.md`
- `docs/spec/rdf-grounded-terminology.md`
