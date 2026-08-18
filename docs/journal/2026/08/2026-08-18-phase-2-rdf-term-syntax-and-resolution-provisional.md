---
manifest_kind: cncf.provisional.phase.v2
manifest_version: 2
provisional_id: SMARTDOX-P2-TERM2-01-03-PROVISIONAL-002
status: PARKED
authority_source: docs/phase/phase-2.md
phase: "2: RDF Term Syntax and Resolution"
primary_repository: /Users/asami/src/dev2025/smartdox
manifest_path: /Users/asami/src/dev2025/smartdox/docs/journal/2026/08/2026-08-18-phase-2-rdf-term-syntax-and-resolution-provisional.md
created_at: 2026-08-18T17:49:23+09:00
owner: asami
update_rule: cncf-goal-phase-stages-recovered-and-commit-makes-it-authoritative
budget:
  user_token_budget: not-supplied
  target_elapsed_minutes: 120
  max_internal_steps: 3
  max_worker_turns: 2
  max_command_turns: 2
  max_checkpoint_commit_turns: 1
repositories:
  mutation: [/Users/asami/src/dev2025/smartdox]
  validation_only: []
  read_only: []
allowed_roots: [/Users/asami/src/dev2025/smartdox]
repository_state:
  - repository: /Users/asami/src/dev2025/smartdox
    base_head: eaecdb4b8c6e490a2789696a1383e9c668eeac75
    branch: Scala12
    index: empty
    implementation_diff_sha256: 1936e92d65b84617f38a98bfd58d280bdca48030e6d598a3c0507de9d66a2d2c
step_slice_coverage:
  implemented: [TERM2-01-source-located-ast, TERM2-02-explicit-resolution]
  partial: [TERM2-03-executable-acceptance]
  omitted:
    - Phase-3-visible-label-and-narration-projection
    - Phase-4-rdf-jsonld-and-bok-occurrence-projection
    - Phase-5-legacy-compatibility-proof
    - Phase-6-simplemodeling-org-acceptance
owned_paths:
  - /Users/asami/src/dev2025/smartdox/src/main/scala/org/smartdox/Dox.scala
  - /Users/asami/src/dev2025/smartdox/src/main/scala/org/smartdox/parser/DoxInlineParser.scala
  - /Users/asami/src/dev2025/smartdox/src/main/scala/org/smartdox/semanticweb/RdfTermResolver.scala
  - /Users/asami/src/dev2025/smartdox/src/test/scala/org/smartdox/semanticweb/RdfTermResolverSpec.scala
  - /Users/asami/src/dev2025/smartdox/docs/journal/2026/08/2026-08-18-phase-2-rdf-term-syntax-and-resolution-provisional.md
preserve_paths: []
concurrent_dirty_paths: []
owned_path_states:
  - repository: /Users/asami/src/dev2025/smartdox
    paths:
      src/main/scala/org/smartdox/Dox.scala: 493ecbf277be76642bd790ec5696385f9eb80513a5c8fc49f1e2dacb52bf484b
      src/main/scala/org/smartdox/parser/DoxInlineParser.scala: 11813051b058f369fd3cd714e7514a6e3624dc62208e55bfc8bc10ace150ffcc
      src/main/scala/org/smartdox/semanticweb/RdfTermResolver.scala: d7885cf2cab06a774fff4fbc69d4c41606a511f2551fa6a916ff80f717b669dc
      src/test/scala/org/smartdox/semanticweb/RdfTermResolverSpec.scala: 0d018516e3357d9b28f1a1789772b119b1db0346f8f816ed6e7ee26ee9ee12b7
requirements:
  - id: TERM2-01-parsed-rdf-term-forms
    status: partial
    evidence: Dox.Term, Dox.NoTerm, and attribute-preserving Dox.Dfn factory
  - id: TERM2-02-deterministic-explicit-resolution
    status: partial
    evidence: semanticweb.RdfTermResolver explicit CURIE/IRI normalization and diagnostics
  - id: TERM2-03-executable-acceptance
    status: blocked
    evidence: RdfTermResolverSpec added, but its parser-backed focused validation fails before resolver assertions run
affected_consumers:
  - Dox inline parsing feeds the new AST factories.
  - RdfTermResolver consumes only explicit term and definition AST values; it does not perform bare-label fallback.
  - No renderer, RDF/JSON-LD, BoK, SimpleModeling.org, or downstream artifact changed.
target_program_compliance:
  - New Scala declarations use the repository's standard source headers and private helper naming.
  - Parser-backed fixtures intentionally exercise inline RDF terminology tags, including multi-attribute definitions and references.
validation:
  - command: git diff --check
    status: passed
    tree_identity: 1936e92d65b84617f38a98bfd58d280bdca48030e6d598a3c0507de9d66a2d2c
    evidence: no introduced whitespace diagnostics before this PARKED manifest
  - command: ["--batch", "testOnly org.smartdox.semanticweb.RdfTermResolverSpec"]
    status: failed
    tree_identity: 1936e92d65b84617f38a98bfd58d280bdca48030e6d598a3c0507de9d66a2d2c
    evidence: invocation=19369-20260818T084314Z; sbt_exit=1; wrapper_exit=1; 1 suite; 5 tests; 1 succeeded; 4 failed; shared lock released
    log: /var/folders/vx/f3wcxbgx0hbgwfjw3ly2v7lm0000gn/T/cncf-sbt-logs/19369-20260818T084314Z.log
    summary: /var/folders/vx/f3wcxbgx0hbgwfjw3ly2v7lm0000gn/T/cncf-sbt-logs/19369-20260818T084314Z.summary.json
known_failures:
  - TagAttributeState#character_State remains unimplemented for the next attribute character after whitespace. FOCUSED-001 repaired the handoff into that state but cannot supply the missing state implementation under the one-repair provisional budget.
  - LogicalLines.XmlOpenState remains unimplemented for an absolute-IRI attribute containing https:. Inline fixture prefixing did not prevent that lower-level path for every form.
  - Consequently, four parser-backed acceptance cases fail before RdfTermResolver behavior is exercised; no resolver acceptance claim is made.
risks:
  - The staged AST and resolver API is not adoption-ready until the parser state-machine gaps are repaired and the same focused suite passes.
  - Do not expose the new source forms as stable grammar or infer fallback behavior while this manifest is PARKED.
protected_boundaries:
  - No automatic bare-label resolution, registry insertion-order selection, display/narration projection, RDF/JSON-LD output, BoK projection, compatibility migration, or SimpleModeling.org mutation was introduced.
  - No full suite, release, publication, deployment, or acceptance review was run or claimed.
forbidden_actions:
  review_claimed: false
  step_or_phase_closed: false
  full_suite_run: false
  acceptance_or_release_commit_claimed: false
  published_or_deployed: false
checkpoint:
  policy: provisional-checkpoint-v2
  transaction_id: SMARTDOX-P2-TERM2-01-03-PROVISIONAL-002
  branches:
    /Users/asami/src/dev2025/smartdox:
      name: Scala12
      classification: unknown
      authorization: pending
  repository_order: [/Users/asami/src/dev2025/smartdox]
  manifest_repository_commit_order: last
  commit_messages:
    /Users/asami/src/dev2025/smartdox:
      subject: "WIP(provisional): SmartDox Phase 2 RDF term syntax and resolution"
      trailers:
        Provisional-Transaction: SMARTDOX-P2-TERM2-01-03-PROVISIONAL-002
  commit_identities: derive-from-required-transaction-trailers-and-git-history
  manifest_sha256: reported-externally
  pushed: false
recovery:
  skill: cncf-goal-phase
  next_state: PROVISIONAL_ADOPTION
  accepted_checkpoint_steps: [P2-TERM2-001]
  adoption_decision: "A — user selected 002 as the active interrupted checkpoint"
  resume_condition: adopt this PARKED manifest, first implement the two recorded Dox parser state-machine gaps, then rerun the exact focused RdfTermResolverSpec command before any TERM2-03 acceptance claim
---

# PARKED — Phase 2 RDF term syntax and resolution vertical slice

## Recovery adoption decision

On 2026-08-18, the user selected option A for ordinary Phase recovery. This
manifest, `SMARTDOX-P2-TERM2-01-03-PROVISIONAL-002`, is the active
checkpoint-dirty provenance for Phase 2 because it matches the current Phase
title, implementation paths, and recorded source hashes. The earlier `001`
manifest remains historical documentation-contract provenance only.

This provisional vertical increment adds the proposed AST values and explicit
resolver implementation for `term`, `dfn`, and `noterm`. It preserves a
definition anchor separately from its `about` resource, accepts only explicit
CURIE or absolute-IRI references, and records deterministic diagnostics rather
than choosing a bare surface label.

The acceptance suite is intentionally parser-backed, and it exposes two
pre-existing Dox parser state-machine gaps. The permitted repair corrected the
attribute handoff but reached the still-unimplemented target state; absolute
IRI attributes also reach an unimplemented `LogicalLines.XmlOpenState` path.
The single permitted rerun therefore remains red. This is not an accepted
parser or resolver result.

No internal Step created a separate PARK boundary. The remainder of Phase 2
stays open: first repair the recorded parser infrastructure and complete
`TERM2-03`; then implement later Phase 3 through Phase 6 work in separately
admitted slices. The prior documentation contract journal remains historical
context and is not rewritten by this transaction.

The branch is `Scala12` and its task-local status is unknown. This transaction
has not staged or committed the implementation. A later checkpoint needs fresh
explicit user authorization and must preserve this manifest's recorded path
states; it is not an acceptance or release commit.
