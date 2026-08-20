# Phase 4 Checklist: RDF Term Knowledge-Graph Projection

This checklist is the authoritative progress ledger for Phase 4.

## TERM4-01: RDF and JSON-LD Projection

Status: DONE

- [x] Emit the canonical concept IRI in RDF and JSON-LD output.
- [x] Relate concept, definition, source document, and public glossary page
      as distinct resources.

## TERM4-02: BoK-Ready Occurrence Records

Status: DONE

- [x] Emit occurrence records with concept IRI, surface form, locale,
      occurrence kind, resolution kind, source path, and location.
- [x] Ensure no downstream consumer needs to reconstruct identity from a
      displayed label.

## TERM4-03: Executable Acceptance

Status: DONE

- [x] Add executable RDF, JSON-LD, and occurrence-projection specifications.
- [x] Run the focused projection validation successfully.
- [x] Run `git diff --check` successfully.

Evidence:

- Focused SBT invocation `620-20260820T185149Z` ran
  `testOnly org.smartdox.semanticweb.RdfTermProjectionSpec`, with 5 succeeded
  / 0 failed.
- `git diff --check` passed.
