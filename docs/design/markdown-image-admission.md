# Markdown Image Admission Design

Status: stable Phase 10 design
Date: 2026-08-29

The normative behavior contract for this design is
[`docs/spec/markdown-image-admission.md`](../spec/markdown-image-admission.md).
The surrounding parser grammar records the established SmartDox image form and
delegates ordinary Markdown image semantics to that specification in
[`docs/spec/smartdox-grammar.md`](../spec/smartdox-grammar.md).

## Purpose and boundary

SmartDox admits one ordinary Markdown inline image source form,
`![alt](path)`, and projects it to the same `ReferenceImg` model used by the
established `[[image-uri]]` form. The `!` is part of the image opener and
distinguishes this form from the existing bracket/link grammar. Markdown
punctuation is syntax only; it is never rendered as paragraph text.

This Slice establishes documentation authority only. Parser and renderer
implementation, executable specifications, and Phase/checklist status remain
owned by the later MDIMG10-02 and MDIMG10-03 work.

## Admitted source and model

The exact admitted source shape is:

```text
![alt](path)
```

`alt` is source-exact text, including Japanese text and the empty string.
`path` is a nonempty local relative image resource. After dot-segment
normalization, the resource identity is the normalized relative URI. A path
whose traversal would escape the document resource root is rejected.

For a document conversion, the document resource root is the canonical
absolute parent directory of the input document. The conversion entrypoint
establishes this root once and passes the same root to Markdown-image
admission and to PDF rendering. Admission URI-decodes the path, resolves it
against that root, normalizes the resulting path, and requires it to remain
contained by the root; its model source is the root-relative normalized URI.
The PDF renderer resolves that model URI against the same root and, when the
resource exists, verifies canonical containment before reading or copying it.
No admitted Markdown image may use the process current directory as an
implicit fallback. Missing-resource existence remains a renderer concern, not
an admission-time filesystem probe.

The parser projection is:

```text
ReferenceImg(
  src = normalized relative URI,
  alt = Some(source-exact alt text),
  attributes = empty,
  location = parser location
)
```

An empty alt therefore remains `Some("")`. The established
`[[image-uri]]` form remains compatible and continues to project
`ReferenceImg` with `alt = None`. Direct `ReferenceImg` construction remains
valid.

The path must pass SmartDox's existing image-file classification. Its
classification and supported image suffixes are not redefined here.

## Diagnostics and ownership

Admission reports deterministic diagnostics with source location, the exact
source fragment, and the raw path when a path can be identified (otherwise an
explicit absent-path context):

| Diagnostic | Meaning | Owner |
| --- | --- | --- |
| `image.markdown.malformed` | The source does not have the exact `![alt](path)` shape, including malformed delimiters or unsupported Markdown image-title/reference syntax. | Parser admission |
| `image.markdown.unsupported-resource` | The parsed path is empty, remote, absolute, outside the document resource root after normalization, or rejected by existing image-file classification. | Parser admission |
| `image.local.missing-resource` | The admitted local image resource is absent when the local PDF is rendered. | Local PDF rendering, before a misleading PDF artifact is emitted |

The diagnostic names and ownership above are stable; this design does not
prescribe an internal Scala error-class implementation.

## Compatibility and responsibility boundary

The established `[[image-uri]]` grammar, direct `ReferenceImg` values, and
the current bracket/link grammar remain valid. Existing image-file
classification in the bracket/link grammar is preserved. Non-image Markdown
links remain `Hyperlink` values. The new `![alt](path)` form is neither a text
`!` node nor a `Hyperlink`.

The intended consumer chain is authoritative grammar →
`DoxInlineParser`/`DoxLinesParser` → `ReferenceImg` → `Dox2LatexConverter` →
`PdfOperationClass`. This design changes none of those code paths. Cozy owns
no preprocessing or image-model workaround for this feature.

## Non-goals

- No Scala, parser, renderer, or public model/API edit in this documentation-only Slice.
- No Markdown image title, reference-style image, remote URI, absolute path, or rendered source punctuation.
- No change to established bracket/link semantics or non-image link projection.
- No Cozy preprocessing/workaround or receipt contract.
- No Phase/checklist status update, executable specification, publication, deployment, or upload.
