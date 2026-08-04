# Phase 1 Checklist

This checklist is the authoritative progress ledger for Phase 1: Article Media
Publication and Site Projection.

## AMP1-01: Publication Contract and Compatibility

Status: DONE

- [x] Promote the publication contract to reviewed
      `docs/design/article-media-publication.md` and
      `docs/spec/article-media-publication.md` before implementation.
- [x] Define the normalized article, locale, infographic, video-presentation,
      content-URL, watch-URL, provider, and publication-status semantics.
- [x] Define canonical publication-registry input and validation outcomes.
- [x] Resolve one article-media record by stable article identity and locale.
- [x] Admit article-media association only through `PublishMetadata`; keep
      `DocumentPropertiesParser` and Dox HEAD media URLs out of scope.
- [x] Keep the media block absent when no matching record exists.
- [x] Preserve `VideoPublication` as a compatible input by projecting it into
      the normalized internal model without changing source-package matching,
      `.video` slug rewriting, player/caption/link rendering, or its
      missing-metadata diagnostic; define its articlePath identity,
      locale-neutral availability, `publicPath -> contentUrl` mapping,
      explicit-record precedence, and duplicate-conflict diagnostic.
- [x] Keep a legacy `VideoPublication` with an invalid adapter content URL
      registry-valid and preserve its source-page behavior; emit a deterministic
      adapter diagnostic and omit only the ordinary-article compatibility
      candidate.
- [x] Reject or diagnose invalid URLs, conflicting locale variants, and an
      external video marked published without a watch URL.

## AMP1-02: Article and Notice Projection

Status: DONE

- [x] Project an optional infographic reference into the Notice data model and
      locale YAML encoder.
- [x] Project a published video watch reference into the Notice data model and
      locale YAML encoder.
- [x] Emit equal resolved media for an article's global and category-local
      Notice projections.
- [x] Project an article-top external-video callout without duplicating the
      URL in article source.
- [x] Project an embedded player only when the normalized presentation is
      site-hosted and its content reference is available.
- [x] Preserve article, Notice, dashboard, feed, and site behavior when media
      metadata is absent.

## AMP1-03: Specifications and Downstream Acceptance

Status: DONE

- [x] Add executable specifications for parser/registry normalization and
      locale association.
- [x] Add executable specifications for global/category Notice equality and
      absent-media behavior.
- [x] Add executable specifications for published versus unpublished external
      video and site-hosted presentation behavior.
- [x] Add executable specifications proving `VideoPublication` compatibility.
- [x] Provide a SimpleModeling.org fixture or documented metadata example for
      Japanese and English infographic and YouTube references.
- [x] Verify the fixture can be consumed by the existing Notice/widget path
      without changing article ordering or card identity.

## AMP1-04: Closure

Status: OPEN

- [ ] Run focused and full SmartDox tests.
- [ ] Run `git diff --check`.
- [ ] Complete a read-only post-implementation review.
- [ ] Fix all actionable findings, including executable-specification debt.
- [ ] Complete a clean read-only re-review when fixes are made.
- [ ] Record downstream handoff evidence for SimpleModeling.org and Cozy.
- [ ] Commit validated changes with required version updates.
- [ ] Close Phase 1 from checklist results.
