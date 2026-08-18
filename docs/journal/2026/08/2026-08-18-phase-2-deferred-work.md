# Phase 2 Deferred Work

Status: open
Date: 2026-08-18

| ID | Deferred behavior | Boundary and reason | Intended follow-up |
| --- | --- | --- | --- |
| P2-DFW-001 | Generic boolean and self-closing open-tag handling in `DoxInlineParser.OpenTagState.resultOpenEnd` and the `>` / `/` branches of `TagAttributeState`. | These pre-existing branches remain `???`.  They are not required for quoted-attribute `<term>`, `<dfn>`, or `<noterm>` Phase-2 syntax.  Repairing generic tag grammar would exceed this resolver/parser recovery slice. | Specify and implement generic boolean and self-closing inline-tag behavior in a separate parser task. |

Candidate Triage: COMPLETED
Canonical ID: DEV-001
Source ID: P2-DFW-001
Disposition: NEW_PHASE
Strategy Record: docs/strategy/smartdox-development-strategy.md#9-development-item-status
Target Phase: docs/phase/phase-7.md
Triaged On: 2026-08-19
