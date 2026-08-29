# Markdown Image Admission Specification

Status: normative Phase 10 specification
Date: 2026-08-29

The stable design boundary is
[`docs/design/markdown-image-admission.md`](../design/markdown-image-admission.md).
This specification is also the delegated ordinary Markdown image chapter of
[`docs/spec/smartdox-grammar.md`](smartdox-grammar.md); the latter continues to
define the established `[[image-uri]]` form.

## Exact source grammar

The only admitted ordinary Markdown inline image source form is:

```ebnf
markdown-image ::= "![" alt-text "](" path ")"
```

The literal `!` distinguishes `markdown-image` from the existing bracket/link
grammar. `alt-text` is source-exact text and may be empty. `path` is the
source path between the parentheses. There is no title suffix, reference-style
image form, or alternate delimiter form in this grammar. Image punctuation is
syntax and is not a rendered text node.

## Resource admission

The source path MUST be nonempty, local, and relative. It MUST pass SmartDox's
existing image-file classification. Dot segments are normalized before the
resource identity is retained. Any traversal that escapes the document
resource root is rejected; a normalized path that remains within that root is
the resource identity as a relative URI.

For a document conversion, the document resource root MUST be the canonical
absolute parent directory of the input document. The conversion entrypoint
MUST establish this root once and pass the identical root to Markdown-image
admission and to PDF rendering. Admission MUST URI-decode the path, resolve it
against that root, normalize the resolved path, and reject it unless it remains
contained by that root; the retained source identity is the root-relative
normalized URI. The renderer MUST resolve that model URI against the same
root and, for an existing resource, verify canonical containment before
reading or copying it. The process current directory MUST NOT be a fallback
for an admitted Markdown image. Resource existence is not an admission-time
probe: an admitted but absent resource remains the renderer-owned missing
resource case.

Remote URIs, absolute paths, empty paths, paths rejected by image-file
classification, and paths escaping the document resource root are not
admitted. This specification does not change the existing image-file
classification used by `[[image-uri]]` or by the established bracket/link
grammar.

## Model projection

Every admitted `![alt](path)` source MUST project to one shared image model:

```text
ReferenceImg(
  src = normalized relative URI,
  alt = Some(source-exact alt text),
  attributes = empty,
  location = parser location
)
```

Japanese alt text is preserved exactly as authored. An empty alt is retained as
`Some("")`, not converted to `None`. The established `[[image-uri]]` form
continues to project `ReferenceImg` with `alt = None`, and direct `ReferenceImg`
values remain compatible.

## Deterministic diagnostics

Each diagnostic includes deterministic source/path context: parser location,
the exact source fragment, and the raw path when identifiable. A diagnostic
with no identifiable path carries an explicit absent-path context.

| Diagnostic | Required condition | Ownership and timing |
| --- | --- | --- |
| `image.markdown.malformed` | The candidate does not match the exact `![alt](path)` source shape, including malformed delimiters or Markdown image title/reference syntax. | Parser admission |
| `image.markdown.unsupported-resource` | The source shape parses, but its path is empty, remote, absolute, outside the document resource root after normalization, or rejected by existing image-file classification. | Parser admission |
| `image.local.missing-resource` | An admitted local resource is missing. | Local PDF rendering, before emitting a misleading PDF artifact |

The diagnostic names are the stable contract. Their internal Scala error-class
representation is intentionally unspecified.

## Compatibility requirements

- Established `[[image-uri]]` remains admitted with its current image-file classification and `alt = None` projection.
- Direct `ReferenceImg` remains admitted.
- Existing bracket/link grammar remains unchanged.
- Non-image Markdown links remain `Hyperlink`.
- `![alt](path)` becomes a `ReferenceImg`, never a text `!` node and never a `Hyperlink`.
- Source-exact Japanese and empty alt text remain observable through the model.

## Implementation and evidence boundary

The authoritative consumer chain is grammar → `DoxInlineParser`/`DoxLinesParser`
→ `ReferenceImg` → `Dox2LatexConverter` → `PdfOperationClass`. This static
authority Slice changes no implementation path. MDIMG10-02 owns parser and
renderer admission; MDIMG10-03 owns the Given/When/Then executable behavior
evidence. No executable specification is introduced here.

## Non-goals

This specification does not define a Scala/API/model edit, Cozy preprocessing
or workaround, a Cozy receipt contract, Phase/checklist status, publication,
deployment, upload, or executable specifications in this documentation-only
Slice.
