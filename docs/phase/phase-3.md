# Phase 3: RDF Term Display and Occurrence Metadata

Status: planned

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

## References

- `docs/phase/phase-3-checklist.md`
- `docs/design/rdf-grounded-terminology.md`
- `docs/spec/rdf-grounded-terminology.md`
