# Parser and PDF Responsibility Decomposition

Status: proposed Phase 13 design

## Purpose

Phase 13 separates cohesive implementation responsibilities from the
large SmartDox inline/document-parser and PDF-operation sources.  This is a
behavior-preserving design: it creates no grammar, AST, rendering, diagnostic,
or command-line contract.

Phase 13 also carries the narrow DoxSite compatibility repair required to make
the established Document Project public-URL flattening observable consistently:
consumers use logical `xxx.dox` content for physical
`xxx.dox/index.dox`, while the physical source page and package metadata remain
available.

## Stable Facades

The following types remain the owner-facing compatibility facades.

- `DoxInlineParser` retains its parse/apply entry points, `Config`, resource
  origin propagation, and existing nested state identities that callers and
  executable specifications access.
- `Dox2Parser` retains its public parser entry points, `Config`,
  `ParseContext`, style selection, and resolver integration.
- `PdfOperationClass` retains the `pdf` operation, `PdfCommand`, `PdfResult`,
  `PdfRenderer`, `PdfDependencyMode`, and the existing package-visible
  executable-specification seams.

An extracted collaborator may be package-visible only where the owning facade
must delegate across a source file.  It must not become a new user-facing
operation or a replacement public parser API.

## Responsibility Boundaries

### Inline and document parsing

- `DoxInlineParser` owns public configuration, entry points, and every parser
  state protocol and implementation, including its link, text, and formatting
  states.  Its public nested state identities remain source-owned by
  `DoxInlineParser`.
- `DoxInlineParserInlineMacro` is package-internal and owns complete-input
  inline-macro recognition, embedded macro-name lexical splitting and
  validation, and construction of the established Site-link or generic
  `InlineMacro` node with its existing location behavior.  It owns no parser
  state-transition identity;
- `Dox2Parser` owns the document-parser facade and public configuration;
- a document-assembly collaborator owns logical-block assembly and HEAD/
  metadata distillation that otherwise obscures the facade; and
- a front-matter collaborator owns only filename-style front-matter extraction
  and merge mechanics, without changing metadata meaning.

### PDF operation

- `PdfOperationClass` owns CLI entry, renderer selection, and the established
  command/result identities;
- a PDF-input/workspace collaborator owns canonical input parsing, contained
  image staging, and workspace disposal;
- a locale/site-projection collaborator owns locale selection and Site-link
  resolution; and
- a renderer-invocation collaborator owns generated-input naming, local-tool
  discovery, and local/Docker renderer invocation.  Process lifecycle and
  typesetting diagnostic translation remain owned by
  `PdfRendererExecution`.

### Document Project effective content

- `DoxSiteBuilder.Rule` owns source-input admission for a physical
  `xxx.dox/index.dox` Document Project: only the direct index is public parser
  input, while private nested document-shaped files are retained outside parser
  and generated-metadata surfaces. Direct package metadata and non-document
  resource handling remain unchanged.
- `DoxSiteEffectiveContent` owns the shared logical-content view of a Document
  Project package; and
- LinkCollection, related-link projection, and Antora consume that view rather
  than independently recognizing `xxx.dox/index.dox`.

## Invariants

- SmartDox, Markdown, and Org-mode accepted/rejected grammar is unchanged.
- Parsed AST shape, source locations, resource-origin containment, HEAD and
  front-matter metadata, and structured syntax diagnostics are unchanged.
- Existing parser and PDF facade names, signatures, and package-visible test
  seams continue to compile and delegate to the same behavior.
- PDF locale, Site-link, image-containment, renderer selection, process
  ordering, and structured typesetting diagnostics are unchanged.
- No current-directory fallback, new external process, new renderer, or
  publication/deployment behavior is introduced.

## Non-goals

- changing SmartDox grammar or metadata semantics;
- redesigning DoxLinesParser, DoxSite beyond the effective-content compatibility
  repair, PublishMetadata, or Cozy consumers; and
- the separate PublishMetadata responsibility decomposition, which is Phase 14
  only and not part of Phase 13.
