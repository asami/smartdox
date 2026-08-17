# Phase 4 Checklist: RDF Term Knowledge-Graph Projection

This checklist is the authoritative progress ledger for Phase 4.

## TERM4-01: RDF and JSON-LD Projection

Status: OPEN

- [ ] Emit the canonical concept IRI in RDF and JSON-LD output.
- [ ] Relate concept, definition, source document, and public glossary page
      as distinct resources.

## TERM4-02: BoK-Ready Occurrence Records

Status: OPEN

- [ ] Emit occurrence records with concept IRI, surface form, locale,
      occurrence kind, resolution kind, source path, and location.
- [ ] Ensure no downstream consumer needs to reconstruct identity from a
      displayed label.

## TERM4-03: Executable Acceptance

Status: OPEN

- [ ] Add executable RDF, JSON-LD, and occurrence-projection specifications.
- [ ] Run the focused projection validation successfully.
- [ ] Run `git diff --check` successfully.
