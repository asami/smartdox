# Phase 6 RDF-Term Acceptance Handoff

Status: accepted pending the `TERM6-ACCEPTANCE` Step commit; Phase 6 remains
in-progress and this record is not Phase closure.

## Source and mirrored fixture

The authoritative source is the SimpleModeling.org repository at
`/Users/asami/src/dev2025/simplemodeling-org`, revision
`efa979b5fb5ae04f28756eff18db5f3e8ebd3b48`, source fixture
`src/main/doxsite/overview/rdf-term-acceptance.dox`. SmartDox consumes the
mirrored fixture at
`src/test/resources/simplemodeling-org/phase-6-rdf-term-acceptance.dox`.
The source and mirrored fixture share SHA-256
`714c2db58f8b2107b2e29e29d8a1f336486c4e93f422e09ddf31bf7c8f5d8ff8`.

## RDF-node evidence

Consumers receive five explicit reference occurrences: Structural Ownership
twice, Composition, Realization, and Cozy Modeling Language. Their canonical
concept IRIs are:

- `https://www.simplemodeling.org/glossary/object-foundation/structural-ownership`
- `https://www.simplemodeling.org/glossary/object-foundation/composition`
- `https://www.simplemodeling.org/glossary/object-foundation/realization`
- `https://www.simplemodeling.org/glossary/literate-modeling/cml`

The source document IRI is
`https://www.simplemodeling.org/overview/rdf-term-acceptance`, and the public
page IRI is
`https://www.simplemodeling.org/overview/rdf-term-acceptance.html`. The source
path is `overview/rdf-term-acceptance.dox`. The downstream projection exposes
the relations `sourceDocument`, `publicGlossaryPage`, `inSourceDocument`, and
`denotesConcept`; its JSON-LD includes all four canonical concept IRIs.

## Display and speech evidence

The accepted display and speech behavior is:

- Japanese-first display: `構造上の所有（Structural Ownership）`.
- Short label: `所有`.
- CML visible expansion: `CML（Cozy Modeling Language）`.
- Speech labels: `構造上の所有` and `CML`.
- Ordinary `所有`, `合成`, and `実現` remain authored stable spans rather than
  term references.

## Validation evidence

- Focused SmartDox acceptance invocation
  `97834-20260820T215703Z` (`SimpleModelingRdfTermAcceptanceSpec`): 4 passed.
- Focused accumulator invocation `98554-20260820T215801Z`
  (`RdfTermDisplaySpec` and `RdfTermProjectionSpec`): 12 passed.
- `git diff --check` succeeded on the current intended paths.
- External Dox executable:
  `/Users/asami/Dropbox/share/bin.air-sonoma/dox`.
  Its SHA-256 is
  `bcbee451a9c925ecb116b312b1ff42706817492f71fe70b57fcb994f33757c95`.
  From `simplemodeling-org`, the exact invocation was:

  ```text
  dox site -strategy test -output.scope.policy all -target /overview/rdf-term-acceptance.dox src/main/doxsite
  ```

  It exited 0 after the generic `Term` visitor correction. No generated site
  was published.

## Consumer boundary

This is a consumer handoff only. No Textus BoK retrieval occurred, and no
BoK/MCP service or storage layer was implemented. The SimpleModeling glossary
entry-format policy is
`/Users/asami/src/dev2025/simplemodeling-org/docs/spec/glossary-entry-format.md`;
until retrieval or synchronization exists, consumers cite the exact
repository path and revision.
