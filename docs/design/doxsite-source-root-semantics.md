# DoxSite Source-Root Semantics Design

Status: proposed Phase 11 design
Date: 2026-08-30

The normative proposed contract is
[`docs/spec/doxsite-source-root-semantics.md`](../spec/doxsite-source-root-semantics.md).
It composes with the stable Phase 10 Markdown-image contract in
[`docs/spec/markdown-image-admission.md`](../spec/markdown-image-admission.md).

## Problem

Phase 10 made Markdown image admission require an explicit resource root.
`PdfOperationClass` supplies the physical parent directory of its input, but
the DoxSite page and bibliography-source parse paths call `Dox2Parser` without
that context. A Markdown image in those DoxSite inputs therefore rejects
deterministically. Restoring behavior by consulting the process current
directory would make source identity execution-dependent and would weaken the
Phase 10 containment rule.

## Responsibility split

| Boundary | Responsibility |
| --- | --- |
| DoxSite input selection | Determine whether each document has a physical or virtual origin and preserve that origin through cache lookup and parsing. |
| `Dox2Parser` / `DoxInlineParser` configuration | Carry a typed source-root context to Markdown image admission. Existing physical `withResourceRoot(Path)` behavior remains compatible. |
| Markdown image admission | Validate the local relative image URI within the supplied root and project the normalized root-relative `ReferenceImg`. It does not look up files. |
| DoxSite output | Resolve virtual Realm resources only within the virtual namespace needed by the selected page/output path. It does not fall back to the host filesystem. |
| PDF conversion | Continues to require a physical resource root. Virtual DoxSite content is not silently materialized as a PDF input. |

## Intended implementation direction

Phase 11 should introduce a typed parser resource-origin abstraction instead
of overloading a host `Path` for virtual data. The abstraction has physical,
virtual, and absent forms and provides only the operations needed for lexical
admission and normalized relative identity.

`DoxSite.DoxSiteBuilder` should construct the appropriate context at its three
known parser boundaries:

- origin-backed `.dox` and Markdown Realm pages;
- origin-less Realm pages with a stable `node.pathname`; and
- source documents scanned for bibliography references.

The DoxSite cache key must include the source-root identity whenever cached
documents can be reused under more than one origin. The Phase must verify this
explicitly rather than relying on the existing pathname-only cache key.

The output representation keeps `ReferenceImg.src` as the normalized relative
URI. The design does not add an absolute filesystem path to the public Dox
model and does not allow virtual path data to become a host filesystem path.

## Deliberate exclusions

- No process-current-directory fallback.
- No remote image retrieval, generic URL resolver, or resource-existence probe
  during parser admission.
- No Cozy preprocessor, source transform, PDF receipt format, publication,
  upload, or deployment work.
- No change to site-link resolution, established bracket images, or ordinary
  Markdown links.
- No virtual-to-PDF materialization; that would be a separate contract.
