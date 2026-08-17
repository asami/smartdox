---
manifest_kind: cncf.provisional.phase.v2
manifest_version: 2
provisional_id: SMARTDOX-P2-TERM2-01-PROVISIONAL-001
status: PARKED
authority_source: docs/phase/phase-2.md
phase: "2: RDF-Grounded Terminology Semantics"
primary_repository: /Users/asami/src/dev2025/smartdox
manifest_path: /Users/asami/src/dev2025/smartdox/docs/journal/2026/08/2026-08-18-phase-2-rdf-terminology-contract-provisional.md
created_at: 2026-08-18T05:47:40+09:00
owner: user
update_rule: cncf-goal-phase-stages-recovered-and-commit-makes-it-authoritative
budget:
  user_token_budget: not-supplied
  max_steps: 1
  max_worker_turns: 1
  max_command_turns: 1
  max_checkpoint_commit_turns: 1
repositories:
  mutation: [/Users/asami/src/dev2025/smartdox]
  validation_only: []
  read_only: [/Users/asami/src/dev2025/simplemodeling-org]
allowed_roots: [/Users/asami/src/dev2025/smartdox]
repository_state:
  - repository: /Users/asami/src/dev2025/smartdox
    base_head: 5008b2fd186ec1fa067f4894cbd81c5361460441
    index: f66fb5eda507d447bdb1dd96193cef399a5c7d2e
    implementation_diff_sha256: 1c46288b6bce53c70b3038665969779733233101b3eb17c9ba7641f54d5ae90a
step_slice_coverage:
  implemented: [TERM2-01-contract-documentation]
  partial: [TERM2-01]
  omitted: [TERM2-02, TERM2-03, TERM2-04, TERM2-05]
owned_paths:
  - /Users/asami/src/dev2025/smartdox/docs/design/rdf-grounded-terminology.md
  - /Users/asami/src/dev2025/smartdox/docs/spec/rdf-grounded-terminology.md
  - /Users/asami/src/dev2025/smartdox/docs/spec/smartdox-grammar.md
  - /Users/asami/src/dev2025/smartdox/src/main/dox/spec/03-inline-semantics.dox
  - /Users/asami/src/dev2025/smartdox/src/main/dox/spec/04-tag-system.dox
  - /Users/asami/src/dev2025/smartdox/src/main/dox/spec/05-extraction-model.dox
  - /Users/asami/src/dev2025/smartdox/docs/journal/2026/08/2026-08-18-phase-2-rdf-terminology-contract-provisional.md
preserve_paths: []
concurrent_dirty_paths: []
owned_path_states:
  - repository: /Users/asami/src/dev2025/smartdox
    paths:
      docs/design/rdf-grounded-terminology.md: 257a665d094668ef48d1bf38485e0f55f240ee6a87df4a47c43ae8897822935a
      docs/spec/rdf-grounded-terminology.md: b947c5a0adeae4d9eebca1c9cad02ed483f8422106ec08d35351f4c815cf4cf9
      docs/spec/smartdox-grammar.md: 41d45234dbf9f54984ee836829534ddd5652b69b52568bc1585c96eb8218ff8c
      src/main/dox/spec/03-inline-semantics.dox: 37b09235074a54fb16d715155bd9eb11a896a218a07d73c4050676eb990bc298
      src/main/dox/spec/04-tag-system.dox: a862af397d783f5a8fb4e8d797f3e50697bcfd884e789e82645f2d70cfc825ba
      src/main/dox/spec/05-extraction-model.dox: bc2b45f8b05ef2ca089335ab99e3e1e4c791973f810f679201a4f1975414b535
requirements:
  - id: TERM2-01-identity-contract
    status: implemented
    evidence: docs/design/rdf-grounded-terminology.md
  - id: TERM2-01-normative-contract
    status: implemented
    evidence: docs/spec/rdf-grounded-terminology.md
  - id: TERM2-01-existing-spec-cross-references
    status: implemented
    evidence: docs/spec/smartdox-grammar.md and src/main/dox/spec/03-inline-semantics.dox through 05-extraction-model.dox
  - id: TERM2-02-parser-ast-resolution
    status: omitted
    evidence: deferred to ordinary Phase 2 recovery
  - id: TERM2-03-projection
    status: omitted
    evidence: deferred to ordinary Phase 2 recovery
  - id: TERM2-04-compatibility-acceptance
    status: omitted
    evidence: deferred to ordinary Phase 2 recovery
affected_consumers:
  - SimpleModeling.org remains a read-only terminology authority; its existing public term URI and TBox conventions are the contract evidence, but no consumer artifact changed.
  - Textus BoK and MCP remain downstream consumers; no service or storage artifact changed.
target_program_compliance:
  - All modified Dox section and metadata headings pass the mandatory blank-line termination check.
  - No executable behavior changed; parser, resolver, renderer, RDF/JSON-LD, BoK, and runtime specifications remain unimplemented.
validation:
  - command: git diff --check
    status: passed
    tree_identity: 1c46288b6bce53c70b3038665969779733233101b3eb17c9ba7641f54d5ae90a
    evidence: no whitespace diagnostics
  - command: Dox one-line-heading termination static check
    status: passed
    tree_identity: 1c46288b6bce53c70b3038665969779733233101b3eb17c9ba7641f54d5ae90a
    evidence: no heading/body adjacency in the three modified Dox specification sources
  - command: whole-file trailing-whitespace static scan
    status: failed
    tree_identity: 1c46288b6bce53c70b3038665969779733233101b3eb17c9ba7641f54d5ae90a
    evidence: pre-existing trailing whitespace remains in the three modified Dox sources; `git diff --check` proves this Step introduced none
  - command: focused SBT/runtime validation
    status: not_run
    tree_identity: not-run
    evidence: Work Class D documentation-only provisional step; no executable behavior was admitted
known_failures:
  - Pre-existing trailing whitespace in src/main/dox/spec/03-inline-semantics.dox, 04-tag-system.dox, and 05-extraction-model.dox is outside this documentation-contract Step; no new trailing whitespace was introduced.
risks:
  - The contract is not parser-backed; later implementation must add executable acceptance before any stable-grammar claim.
  - SimpleModeling.org remains read-only here; its glossary sources must be updated only in a separately admitted compatibility step if explicit instance metadata is required.
protected_boundaries:
  - No parser, AST, resolver, renderer, glossary registry, RDF/JSON-LD, BoK, site, MCP, or dependency implementation changed.
  - No phase, checklist, strategy, SimpleModeling.org, or Textus BoK mutation occurred.
forbidden_actions:
  review_claimed: false
  step_or_phase_closed: false
  full_suite_run: false
  acceptance_or_release_commit_claimed: false
  published_or_deployed: false
checkpoint:
  policy: provisional-checkpoint-v2
  transaction_id: SMARTDOX-P2-TERM2-01-PROVISIONAL-001
  branches:
    /Users/asami/src/dev2025/smartdox:
      name: Scala12
      classification: unknown
      authorization: pending
  repository_order: [/Users/asami/src/dev2025/smartdox]
  manifest_repository_commit_order: last
  commit_messages:
    /Users/asami/src/dev2025/smartdox:
      subject: "WIP(provisional): SmartDox Phase 2 RDF terminology contract"
      trailers:
        Provisional-Transaction: SMARTDOX-P2-TERM2-01-PROVISIONAL-001
  commit_identities: derive-from-required-transaction-trailers-and-git-history
  manifest_sha256: reported-externally
  pushed: false
recovery:
  skill: cncf-goal-phase
  next_state: PROVISIONAL_ADOPTION
  accepted_checkpoint_steps: []
  resume_condition: adopt this PARKED contract through cncf-goal-phase, then implement TERM2-02 parser/AST/resolution before TERM2-03 projection and TERM2-04 acceptance
---

# PARKED — Phase 2 RDF terminology contract

This provisional transaction implements only the Stage 2.1 documentation
contract. It establishes one RDF IRI identity for a glossary concept, separates
the `smglo` vocabulary from `smterm` concept instances, and defines the future
authoring, diagnostic, display, speech, and projection contract.

No parser or runtime behavior has changed. In particular, the proposed
`term_namespaces`, `<term ref>`, `<dfn about>`, and `<noterm>` forms are not
stable parser grammar and must not be represented as currently supported
behavior. Existing `<dfn>` input remains described as compatibility behavior.

The Phase 2 checklist remains unchanged. The next ordinary `cncf-goal-phase`
run must adopt this manifest, preserve the recorded path identities, and begin
with TERM2-02 executable parser/AST/resolution work. It must then establish
TERM2-03 projection and TERM2-04 SimpleModeling.org acceptance in separately
frozen Steps before Phase closure is considered.

The branch is `Scala12`; this transaction has no evidence that the branch is
task-local. No staging or checkpoint commit has been attempted. An explicit
user authorization is required before the stated provisional checkpoint
transaction may be committed.
