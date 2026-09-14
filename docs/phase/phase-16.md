# Phase 16: Cozy BoK Consumer Acceptance

Status: PLANNED

Plan date: 2026-09-09

Predecessor: Phase 15 operational force-release baseline
`34d27df6b8516951d2f664bc60e5be1f556f2901`.

## Goal

Prove that an actual Cozy-built BoK fixture consumes SmartDox's unchanged
direct DoxSite article-header projection under the accepted virtual source-root
semantics.

## Origin

This Phase adopts `DEV-006` from
`docs/journal/2026/09/2026-09-09-phase-15-cozy-consumer-acceptance-deferral.md`.
The SmartDox direct-DoxSite physical and virtual proofs are not Cozy consumer
acceptance and cannot substitute for this Phase's fixture evidence.

## Scope

In scope:

- freeze exact SmartDox and Cozy baselines before consumer-fixture execution;
- exercise an actual Cozy-built BoK fixture that consumes the existing direct
  DoxSite article-header projection with a virtual source root;
- prove the established source-root containment and absence of a
  process-current-directory fallback through that consumer boundary; and
- record executable acceptance evidence for the existing metadata, media
  action, locale, accessibility, and infographic behavior required by the
  authoritative article-header specification.

Out of scope:

- a Cozy-only HTML rewrite, preprocessor, registry schema, or product mutation;
- changing SmartDox article-header semantics, article-media registry roles,
  locale resolution, or source-root semantics;
- modifying the Cozy checkout's pre-existing uncommitted work;
- publication, deployment, push, or downstream production acceptance; and
- interpreting Phase 15's ordinary in-progress ledger as normal closure.

## Stages

### CBA16-01: Cross-Repository Fixture Boundary

Stage Status:
- Current status: PLANNED
- Owner: SmartDox and Cozy consumer-acceptance boundary
- Update rule: Start only after exact clean/preserved baselines for both
  repositories are frozen and the fixture contract is mapped to the
  authoritative SmartDox specification.

- Freeze the two repository identities without absorbing unrelated Cozy work.
- Identify the existing Cozy BoK fixture/build path and the direct DoxSite
  projection it consumes.

### CBA16-02: Actual Cozy BoK Acceptance Evidence

Stage Status:
- Current status: PLANNED
- Owner: Cozy-built BoK fixture consuming the SmartDox projection
- Update rule: Accept only with executable evidence from the actual consumer,
  not from direct SmartDox DoxSite tests alone.

- Exercise the consumer fixture under the virtual source-root contract.
- Assert the required projection behavior and preserve the no-Cozy-only-rewrite
  boundary.

### CBA16-03: Cross-Repository Review and Release

Stage Status:
- Current status: PLANNED
- Owner: Phase 16 acceptance ledger
- Update rule: Close only after every checklist item, independent Phase review,
  final validation, and the distinct release boundary succeed.

- Record separate SmartDox and Cozy validation evidence for the frozen final
  tree and complete the Phase closure ledger.

## Completion Criteria

Phase 16 completes only when an actual Cozy-built BoK fixture proves use of the
unchanged direct DoxSite article-header projection with a virtual source root,
the required source-root and projection behaviors have executable evidence, no
Cozy-only transformation is introduced, the two-repository review and release
gates succeed, and every checklist item is closed. Registration alone neither
starts the fixture work nor accepts the Phase.

## References

- `docs/phase/phase-16-checklist.md`
- `docs/design/article-header-metadata-and-media-actions.md`
- `docs/spec/article-header-metadata-and-media-actions.md`
- `docs/journal/2026/09/2026-09-09-phase-16-cozy-bok-consumer-acceptance-registration.md`
