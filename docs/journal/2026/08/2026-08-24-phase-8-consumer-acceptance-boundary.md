# Phase 8 Consumer Acceptance Boundary

Status: accepted decision
Date: 2026-08-24

Decision ID: `SMARTDOX-P8-ACCEPTANCE-001`

## Decision

SmartDox Phase 8 owns the producer-side RDF graph literal-label correction and
the downstream evidence needed for Cozy to accept the newly generated site
artifact. Its required acceptance boundary is therefore:

- regenerate the SimpleModeling.org site with the accepted SmartDox version;
- prove every generated RDF graph node label is nonempty, including the known
  empty language-tagged literal cases;
- run Cozy metadata finalization and record the required inventory, hash, and
  metadata-allowlist evidence; and
- retain the WIP and production wrapper failure-path evidence for invalid BoK
  metadata.

Managed Textus BoK startup, reader acceptance, and the CNCF post-assembly
activation hook are not SmartDox Phase 8 completion conditions. They are
downstream consumer-integration work owned by Textus BoK Phase 8, including
its CNCF Phase 70 dependency.

## Rationale

Making SmartDox Phase 8 wait for the consumer bootstrap would reverse the
dependency needed to advance Cozy development: SmartDox must first produce an
artifact Cozy can finalize, while Textus BoK may later consume that finalized
artifact through its own managed-startup lifecycle.

## Consequences

- The Phase 8 authority and checklist must remove Textus BoK reader acceptance
  from their required closure items and retain it only as a nonblocking consumer
  handoff.
- No SmartDox-local reader, fixed fixture, or startup workaround is permitted
  to substitute for Textus BoK Phase 8 acceptance.
- Textus BoK Phase 8 `P8-C` remains responsible for the managed `website.d`
  reader acceptance once its own bootstrap and CNCF Phase 70 prerequisites are
  available.
