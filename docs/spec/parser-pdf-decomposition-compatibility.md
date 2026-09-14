# Parser and PDF Decomposition Compatibility

Status: proposed Phase 13 specification

## Scope

This specification fixes the observable compatibility boundary for the
Phase 13 source-responsibility decomposition.  It adds no new accepted input
or rendering behavior.

The separate PublishMetadata responsibility decomposition belongs to Phase 14
only and is excluded from this Phase 13 compatibility boundary.

The Phase also admits a narrow DoxSite compatibility repair: the established
public URL flattening for a physical `xxx.dox/index.dox` source must use logical
`xxx.dox` content consistently in LinkCollection, related-link projection, and
Antora. The physical source page and package metadata remain addressable; no
new public URL, Dox authoring form, or DoxSite feature is introduced.

## Parser compatibility

- `DoxInlineParser` continues to parse the established SmartDox, Markdown,
  Org-mode, inline-macro, generic-tag, link, image, and formatting forms.
- Existing successful parse results retain their Dox node kinds, contents,
  attributes, ordering, location, resource-origin, and source-identity
  facets.
- Existing rejected forms retain their structured diagnostic identity and
  source-location/token-context evidence.
- `DoxInlineParserInlineMacro` may perform complete-input macro recognition,
  embedded macro-name lexical splitting/validation, and established Site-link
  or generic `InlineMacro` construction only as a package-internal delegate.
  `DoxInlineParser` remains the source owner of all parser states; in
  particular, `LinkState`, `OrgModeLinkUrnState`, `OrgModeLinkLabelState`,
  `MarkdownLinkUrnState`, `MarkdownLinkUrnContState`, and
  `MarkdownLinkLabelState` retain their nested identities and companions.
- `Dox2Parser` continues to preserve filename-driven style selection, logical
  block/section assembly, HEAD metadata handling, front matter, include, and
  resolver behavior.

The primary executable specifications are
`DoxInlineParserSpec` and `Dox2ParserSpec`; they must pass unchanged or gain
only regression coverage that observes an already established behavior.

## PDF compatibility

- `PdfOperationClass` continues to expose the existing command, renderer,
  result, and package-visible test seams.
- Canonical input roots, contained image staging, locale selection, Site-link
  projection, renderer dispatch, local/Docker choice, timeout handling, and
  structured renderer diagnostics are preserved.
- Invalid input must still fail before an external renderer starts whenever
  that is the established behavior.

`PdfOperationClassSpec` is the primary executable specification.  Its focused
execution must cover each extracted PDF responsibility and preserve the
operation's existing behavior.

## Document Project compatibility

- A Document Project is a physical `xxx.dox` directory with an immediate
  `index.dox` child. Its public parser input is that direct index alone;
  nested `.dox`, `.org`, `.md`, and `.markdown` sources are private package
  content and do not become pages, diagnostics, or generated site metadata.
  Direct package metadata and non-document resources retain their established
  handling.
- LinkCollection records outgoing and incoming relations under logical
  `xxx.dox` identity, including Site-link references authored as
  `xxx.dox/index.html`.
- Related links and Antora use that same effective content and never emit an
  `xxx.dox/index` page for a Document Project.
- A nested Document Project must not alter the Antora module path of a following
  sibling page.

`DoxSiteSpec` is the primary executable specification for this compatibility
repair.

## Acceptance

Phase 13 accepts an extraction only when the focused parser or PDF executable
specification passes on the exact tree, the facade compatibility constraints
above are reviewed, and no new behavior is claimed.
