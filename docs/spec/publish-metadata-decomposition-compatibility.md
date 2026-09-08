# PublishMetadata Decomposition Compatibility Specification

Status: proposed Phase 14 specification

This specification fixes the behavior boundary for the Phase 14 internal
responsibility decomposition. It adds regression evidence only; it does not
authorize new behavior.

## Loading Selection

- `PublishMetadata.load(directory)` continues to return `None` for an empty
  metadata directory.
- A publication-bundle entry set remains the selected registry when a
  directory contains both a valid bundle and standalone metadata files.
- When no bundle exists, recognized standalone JSON/YAML metadata remains
  loadable under its existing catalog, publication, repository, and related
  metadata keys.
- Parsed entry paths, identities, ordering, validation, and nested public model
  identities remain unchanged.

## Catalog and Public Realm

- `publicRealm` continues to expose the bundle's validated metadata paths and
  JSON payloads through the existing realm representation.
- `generatedPages` continues to include `catalog/index.dox` and the existing
  publication group catalog/public page paths, titles, links, and diagnostics.
- No new public page, URL, schema, or rendering behavior is introduced.

## Article-Media and Video Compatibility

- `articleMediaProjection` and exact-locale `resolveArticleMedia` preserve
  native infographic, video, PDF-role, and legacy `VideoPublication` behavior.
- `sourcePathToArticleIdentity` preserves source and generated path
  normalization and all public model qualified identities.

## RDF Artifact Merge

- With `RdfMergeConfig.mergePublicationArtifacts = true`, a matching
  repository artifact is parsed into the same RDF triple values.
- With merging disabled, no publication artifact triples are returned.
- A missing or unresolvable artifact remains reference-only under the default
  warn policy and raises the established structured exception under `fail` or
  `error` policy.
- Repository-root containment and artifact path resolution remain unchanged.

## Acceptance

`PublishMetadataSpec` is the primary executable specification for this
boundary. Its deterministic temporary-directory scenarios cover loading
selection, catalog page projection, and RDF merge/missing-policy behavior.
The focused parent validation also runs `ArticleMediaProjectionSpec`; no
production source or consumer changes are admitted in PMD14-01.

## Exclusions

Behavior changes, public/protected API changes, article-media schema changes,
locale or media projection changes, Cozy, publication, deployment, and Phase
15 work are outside this specification.
