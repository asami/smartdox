# PublishMetadata Responsibility Decomposition

Status: proposed Phase 14 design

## Purpose

Phase 14 separates the cohesive responsibilities in the 1,477-line
`PublishMetadata` implementation without changing its observable behavior.
The existing facade remains the compatibility owner; collaborators are
internal implementation details.

## Stable Facade

`PublishMetadata` retains `load`, `publicRealm`, `generatedPages`,
`videoPublications`, `articleMediaProjection`, `videoRdfArtifactTriples`,
`sourcePathToArticleIdentity`, and every existing nested public model and
qualified identity. Direct consumers retain their current imports and calls,
including AntoraGenerator, DoxSiteGenerator, DoxSite, DoxSiteArticleMedia,
DoxSiteBuilder, semanticweb Site, PublishMetadataSpec, and
ArticleMediaProjectionSpec.

## Responsibility Boundaries

- PMD14-01 owns the compatibility ledger and executable regression evidence.
- PMD14-02 may extract article-media parsing, normalization, exact-locale
  resolution, legacy compatibility adaptation, and article-media projection
  behind the facade.
- PMD14-03 may extract publication registry selection, public-realm and
  catalog-page projection, video publication association, and configured RDF
  artifact merging behind the facade.
- PMD14-04 owns acceptance and closure evidence; it introduces no new runtime
  responsibility.

Any collaborator is internal and must not become a replacement public
operation, schema owner, or consumer-facing model.

## Invariants

- `PublishMetadata.load` preserves bundle/standalone selection and empty-input
  behavior.
- `publicRealm` preserves bundle realm entries and paths.
- `generatedPages` preserves catalog and publication page paths and content.
- `videoPublications`, article-media resolution/projection, and
  `sourcePathToArticleIdentity` preserve current values and identities.
- `videoRdfArtifactTriples` preserves configured repository resolution, merge
  enablement, and warn/fail missing-artifact policy.
- Public/protected APIs, nested public model identities, article-media schema,
  locale semantics, and direct-consumer contracts remain unchanged.

## Non-goals

This design does not change behavior, public API, article-media schema, locale
semantics, media projection, Cozy, publication, deployment, or Phase 15. It
does not redesign generators, DoxSite, semanticweb, fixtures, configuration,
or repository scope.
