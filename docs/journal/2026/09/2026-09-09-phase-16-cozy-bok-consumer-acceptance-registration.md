# Phase 16 Cozy BoK Consumer Acceptance Registration

Status: planned

Date: 2026-09-09

## Decision

`DEV-006` is registered as SmartDox Phase 16: Cozy BoK Consumer Acceptance.
It is the separately invoked cross-repository acceptance Phase for the actual
Cozy-built BoK fixture required by the article-header design and specification.

## Preserved boundary

The Phase consumes SmartDox's existing direct DoxSite projection. It does not
authorize a Cozy-only HTML rewrite, preprocessor, registry schema, or product
mutation. Direct SmartDox physical or virtual fixture evidence remains
insufficient to claim this Cozy consumer acceptance.

## Initial scope condition

Before fixture execution, the Phase must freeze exact SmartDox and Cozy
baselines. The Cozy checkout currently contains unrelated uncommitted work;
this registration neither modifies nor adopts it. The Phase must preserve that
work and identify a reproducible consumer-fixture baseline before it starts
implementation or validation.

## Predecessor status

Phase 15's operational baseline is the exception-recorded force-release commit
`34d27df6b8516951d2f664bc60e5be1f556f2901`. That result permits this new
Phase registration but does not rewrite the historical Phase 15 ordinary
review ledger into a normal closure claim.

## Non-effects

This registration starts no fixture run and changes no Cozy source, test,
fixture, Git state, publication, deployment, or production acceptance.
