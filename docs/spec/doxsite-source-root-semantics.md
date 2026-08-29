# DoxSite Source-Root Semantics

Status: proposed Phase 11 specification
Date: 2026-08-30

## Purpose

This specification defines how a DoxSite document establishes the resource
root used to admit local Markdown image paths. It extends the Phase 10
Markdown-image contract to DoxSite consumers without introducing a
current-directory fallback, a Cozy preprocessor, or an implicit remote
resource resolver.

The Phase 10 grammar, `ReferenceImg` model, image-file classification, and
diagnostic names remain authoritative. This document defines only the DoxSite
source-origin context that is supplied to that admission boundary.

## Source-origin kinds

Every DoxSite document that enables Markdown image syntax MUST have exactly one
of these source-origin kinds at parse time:

| Kind | Resource root | Intended input |
| --- | --- | --- |
| Physical | Canonical absolute parent directory of the resolved input document. | A DoxSite page whose `origin` and document pathname resolve to a filesystem document, and a source document discovered by the bibliography scan. |
| Virtual | The normalized virtual parent directory of the document's Realm pathname. | A DoxSite page held as Realm data without a filesystem origin. |
| Absent | No resource root exists. | Anonymous parser use or an input that has neither a resolved physical path nor a stable virtual pathname. |

The parse entrypoint, rather than an individual Markdown-image parser state,
selects the source-origin kind. A document does not infer its root from the
process current directory.

## Physical source roots

For a physical DoxSite source, the entrypoint MUST first resolve the document
pathname against the configured DoxSite origin, canonicalize the document, and
use its canonical parent directory as the physical resource root.

For `![alt](path)`, Phase 10 local-path admission then URI-decodes `path`,
normalizes dot segments, resolves it against that root, and retains the
root-relative normalized URI in `ReferenceImg.src`. A candidate that is
absolute, remote, non-image, empty, malformed, or escapes that document root
is rejected with the existing Phase 10 diagnostic contract.

Resource existence remains renderer or site-output ownership; Markdown image
admission does not perform a filesystem existence probe.

## Virtual source roots

For a virtual DoxSite page, the entrypoint MUST derive a normalized virtual
directory from the page's Realm pathname. The virtual root has no filesystem
meaning and MUST NOT be converted to a host path.

The virtual admission rule is lexical and deterministic:

1. the source path is a local relative image path admitted by the Phase 10
   grammar and image-file classification;
2. dot segments normalize within the virtual document directory;
3. traversal outside that virtual directory is rejected; and
4. the model retains the resulting document-root-relative URI.

The DoxSite/HTML or Antora output layer owns any later mapping of that URI to a
Realm resource. It may not treat a virtual root as permission to read the host
filesystem. Virtual-source images are not PDF inputs: a PDF conversion requires
the existing physical-document root contract unless a separately authorized
materialization contract is introduced.

## Root absence and compatibility

When no physical or virtual source root is available, a Markdown image MUST
fail with the existing `image.markdown.unsupported-resource` diagnostic. The
diagnostic retains the source fragment and raw path context defined by the
Phase 10 specification.

This work does not change:

- the Phase 10 Markdown grammar, `ReferenceImg` projection, Japanese alt-text
  retention, or PDF physical-root behavior;
- established `[[image-uri]]` behavior;
- ordinary Markdown hyperlinks;
- `site:[target.dox]` document-link resolution; or
- resource existence, publication, deployment, or remote URI behavior.

## Required executable evidence

Phase 11 must prove, using Given/When/Then executable specifications:

- physical origin-backed Markdown pages admit sibling image paths and retain
  the normalized relative URI;
- virtual Realm pages admit only paths within their virtual document root;
- physical and virtual traversal attempts reject deterministically;
- root-absent pages retain the stable unsupported-resource diagnostic rather
  than consulting the process current directory;
- bibliography source parsing applies the physical document parent root;
- the same document pathname parsed under distinct physical or virtual
  source-root identities does not reuse stale cache results across roots; and
- existing Markdown links, `site:[...]` links, bracket images, and Phase 10 PDF
  conversion remain compatible.
