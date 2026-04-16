# Site Inline Macro Decision Note

Date: 2026-04-16

## Context

SmartDox has historically accepted single bracket links such as `[xxx]` as a
site-style link notation. This notation now conflicts with other bracket-heavy
inline text, especially escaped or raw strings used by CML metadata and regular
expression examples.

The direction is to migrate site links to an explicit inline macro:

```dox
site:[target.dox]
```

The old `[xxx]` notation remains compatible for now. When it is used, SmartDox
prints a console warning so existing documents can be migrated gradually.

## Current Decision

`site:[xxx]` is introduced as the migration target for site links.

For now, the HTML fallback behavior is intentionally conservative:

- `site:[target.dox]` emits a link to `target.html`.
- The visible label remains `target.dox`.
- Legacy `[target.dox]` remains link-compatible and prints a warning.

This avoids breaking existing documents while making the intended syntax clear.

## Deferred Title Resolution

When SmartDox runs through the `site` command, the target context is known from
the site tree. In that mode, a future implementation can resolve `site:[xxx]`
relative to the current page, load the target document, and use the document
title as the visible label.

Outside the site command, such as inside CML processing, there is currently no
general target context for resolving `site:[xxx]`. To support that use case,
the caller would need to provide a document context and resource resolution
base. CML does not need this immediately, so context-aware title resolution is
deferred.

## Open Items

- Resolve `site:[xxx]` labels from the target document title when a site context
  is available.
- Define how non-site callers, including CML tooling, can provide a site/resource
  context if they need title resolution.
- Decide whether explicit labels or attributes are needed, for example a future
  form such as `site:[target.dox label="Label"]`.
- Remove legacy `[xxx]` site link parsing after existing documents no longer
  emit migration warnings.

