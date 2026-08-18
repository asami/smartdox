# Hygiene Resolution Batch Handoff

Status: COMPLETE
Created: 2026-08-19
Source Repository: /Users/asami/src/dev2025/smartdox
Target Repositories: /Users/asami/src/dev2025/smartdox
Suggested Invocation: $cncf-goal-hygiene /Users/asami/src/dev2025/smartdox/docs/journal/2026/08/2026-08-19-hygiene-resolution-batch-handoff.md

## Purpose

Resolve the remaining Phase-2 non-behavioral source-size and executable-spec
hygiene in one SmartDox maintenance batch.  The parser-state extractions are
an intentional compatibility transition: consumers must recompile and adapt
to the released implementation rather than rely on the prior Scala trait ABI.

## Included Hygiene

| ID | Source | Evidence | Work Package | Required outcome |
| --- | --- | --- | --- | --- |
| HYG-001 | `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md` | `Dox.scala` combined the central AST with independent element definitions. | HP-001 | Split independent AST element definitions into same-package sources without changing public AST identities. |
| HYG-002 | `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md` | `DoxInlineParser.scala` combined public state identities with their default event handling. | HP-002 | Extract default state handling while preserving `DoxInlineParser` public nested type identities and inline grammar behavior. |
| HYG-003 | `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md` | Active legacy `Dox2ParserSpec.scala` examples lacked consistent Given/When/Then boundaries. | HP-003 | Express the existing parser behaviors with explicit executable-spec boundaries and existing matchers. |
| HYG-004 | `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md` | `DoxLinesParser.scala` coupled public parser state identities with default transition and NormalState text handling. | HP-004 | Extract default state handling while preserving `DoxLinesParser` state identities, logical-line behavior, and source locations. |

## Frozen Boundary

- Allowed repositories: `/Users/asami/src/dev2025/smartdox`.
- Preserve paths: all paths outside the source, test, and closure journals named in this handoff; in particular preserve Phase 7 plans and unrelated source, specs, and journals.
- Allowed behavior change: none; preserve public AST and nested parser type names, parser grammar, parser results, diagnostics, source locations, serialized representations, and existing asserted semantics.  The source and binary compatibility of externally compiled implementations of parser-state traits is intentionally not preserved; downstream consumers must recompile or adapt.
- Prohibited expansion: features, new intended public APIs, parser grammar changes, RDF terminology behavior, schema, persistence, transport, security, lifecycle behavior, Phase 7 generic boolean/self-closing-tag work, list-parser completion, rendering changes, and unrelated cleanup.
- Legacy individual task handoffs `...-01.md` through `...-04.md` are superseded by this batch and must not be executed separately.

## HP-001 — Extract independent Dox AST element definitions

- Hygiene IDs: HYG-001
- Repository: /Users/asami/src/dev2025/smartdox
- Targets: `src/main/scala/org/smartdox/Dox.scala`; extracted same-package Scala AST files under `src/main/scala/org/smartdox/`; only compilation references required by the move.
- Allowed repair: move complete top-level AST declarations at declaration boundaries into responsibility-preserving same-package sources without renaming public symbols or changing AST semantics.
- Prohibited expansion: changing the Dox model, parser grammar, RDF behavior, rendering, or unrelated naming.
- Focused validation: `testOnly org.smartdox.DoxSpec org.smartdox.DoxesSpec org.smartdox.parser.Dox2ParserSpec org.smartdox.semanticweb.RdfTermResolverSpec`; static source-size and public-identity inspection.
- Dependencies: None.

## HP-002 — Extract inline-parser default state handling

- Hygiene IDs: HYG-002
- Repository: /Users/asami/src/dev2025/smartdox
- Targets: `src/main/scala/org/smartdox/parser/DoxInlineParser.scala`; `src/main/scala/org/smartdox/parser/DoxInlineParseStateSupport.scala`.
- Allowed repair: move default event handling into a `private[parser]` support trait while retaining `DoxInlineParser.Config` and `DoxInlineParser.DoxInlineParseState` in their original public owner.  Do not preserve the former trait-default-method ABI for externally compiled state implementations.
- Prohibited expansion: generic boolean/self-closing-tag grammar, RDF resolution, broad line-parser refactoring, or any new inline-tag behavior.
- Focused validation: `testOnly org.smartdox.parser.DoxInlineParserSpec org.smartdox.parser.Dox2ParserSpec org.smartdox.semanticweb.RdfTermResolverSpec`; static source-size and public-path inspection.
- Dependencies: HP-001.

## HP-003 — Modernize Dox2 parser executable specifications

- Hygiene IDs: HYG-003
- Repository: /Users/asami/src/dev2025/smartdox
- Targets: `src/test/scala/org/smartdox/parser/Dox2ParserSpec.scala`.
- Allowed repair: add Given/When/Then boundaries to active legacy cases while retaining existing assertions, matcher vocabulary, helper behavior, and production code.
- Prohibited expansion: production parser changes, new grammar, unrelated test rewrites, fixture-coverage expansion, and RDF-term feature work.
- Focused validation: `testOnly org.smartdox.parser.Dox2ParserSpec`; static inspection of active test-case boundaries and private-helper naming.
- Dependencies: HP-002.

## HP-004 — Extract line-parser default state handling

- Hygiene IDs: HYG-004
- Repository: /Users/asami/src/dev2025/smartdox
- Targets: `src/main/scala/org/smartdox/parser/DoxLinesParser.scala`; `src/main/scala/org/smartdox/parser/DoxLinesParseStateSupport.scala`.
- Allowed repair: move default state and NormalState text handling into `private[parser]` support traits while retaining original nested public state identities and parser behavior.  Do not preserve the former trait-default-method ABI for externally compiled state implementations.
- Prohibited expansion: inline-tag grammar, source-location semantics, list-parser completion, RDF terminology behavior, or unrelated parser cleanup.
- Focused validation: `testOnly org.smartdox.parser.Dox2ParserSpec org.smartdox.parser.DoxParserSpec org.smartdox.semanticweb.RdfTermResolverSpec`; static source-size and public-path inspection.
- Dependencies: HP-003.

## Final Focused Review

- Exact target programs/files: every source and test file changed by HP-001 through HP-004; `docs/journal/2026/08/2026-08-18-phase-2-hygiene-ledger.md`; this batch handoff; and the superseded legacy handoff journals.
- Required checks: every included ID, source-size result, extraction boundaries, nested public parser type names, absence of newly public support traits, behavior and source-location preservation, executable-spec structure, package-focused evidence, and containment of the frozen boundary.
- Failure policy: stop without commit; no automatic review-fix/re-review loop.

## Final Full-Validation Gate

1. `/Users/asami/src/dev2025/smartdox`: `sbt --batch test`

Run the repository suite exactly once on the reviewed tree through the shared
serialized SBT runner. Stop on failure.

## Completion Contract

- Commit only after the final focused review and full-validation gate pass.
- Update each HYG-001 through HYG-004 source record to `RESOLVED` with this batch, validation, and acceptance-commit evidence.
- Mark this batch `COMPLETE` only in the accepted committed tree.
- Keep HYG-005's accepted resolution unchanged.
- Do not absorb Development Candidates or newly noticed Hygiene.
- Record the accepted parser-state ABI transition in the grouped acceptance commit; it is deliberate and consumers own any required adaptation.

## Non-goals

- Phase 3 through Phase 7 product work.
- New parser or AST behavior.
- Any cleanup outside the four listed HYG records.
