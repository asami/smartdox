# Article Media Publication and Site Projection

status=design
published_at=2026-08-04

## Purpose

SmartDox associates optional introduction media with an article through
publication metadata, then resolves that association once for both the article
page and its Notice projections. This design makes the same contract usable by
the direct SimpleModeling.org site and by a Cozy-built BoK without making
SmartDox responsible for YouTube, artifact storage, or card-widget rendering.

## Ownership and Input Boundary

`PublishMetadata` is the only SmartDox input boundary for article-media
association. `DocumentPropertiesParser` continues to parse properties owned by
an individual Dox document; it neither accepts article-media association nor
duplicates publication-registry URLs in a document HEAD.

SmartDox resolves registry records against a stable, normalized article
identity. The identity is the site-relative logical article path, without a
locale prefix, leading slash, generated suffix, display title, or Notice
number. Path separators are normalized to `/`. Locale resolution uses an exact
canonical locale tag; a missing locale variant means that no media is
projected. SmartDox does not fall back from one locale to another.

## Normalized Model

```text
ArticleMediaPublication
  articleIdentity
  variants: Locale -> ArticleMediaVariant

ArticleMediaVariant
  infographic: Optional[ImageReference]
  video: Optional[VideoReference]
  articlePdf: Optional[PdfDocumentReference]
  summarySlidesPdf: Optional[PdfDocumentReference]

PdfDocumentReference
  publicPath
  mediaType
  label

VideoReference
  presentation: external-link | site-hosted
  status: draft | published | withdrawn
  provider: Optional[String]
  watchUrl: Optional[URI]
  contentUrl: Optional[URI]
```

The normalized `ArticleMediaVariant` has two independent optional PDF roles:
`articlePdf` and `summarySlidesPdf`. Each present role is represented by a
`PdfDocumentReference(publicPath, mediaType, label)`. The registry maps these
roles from the exact variant fields `article_pdf` and `summary_slides_pdf`;
those fields are the role discriminators. No separate role field and no
filename inference is permitted.

For each present role, `publicPath` is a non-empty, site-visible public path
and `mediaType` is present and exactly `application/pdf`. `label` is optional,
but is nonblank when supplied. Either role may be absent. The future registry
consumer rejects duplicate role entries, invalid public paths or media types,
and invalid locale association. PDF `publicPath` values are locale-agnostic
site-visible references; only the enclosing canonical variant key associates a
reference with a locale. SmartDox does not infer locale from a path or filename
and does not require a locale path prefix. There is no role fallback or merging
and no locale fallback or merging.

Phase 9 defines this PDF-role contract only. It does not implement
`PublishMetadata` parsing or article/Notice projection; those responsibilities
belong to Phase 9.1. Existing infographic, external-video, site-hosted-video,
legacy `VideoPublication`, and no-media behavior remains compatible.

## Locale-Selected Single-Document PDF

For single-document PDF generation, `pdf --locale` accepts only the canonical
locale tags `ja` and `en` for this delivery. Locale-neutral content remains in
the output. Language-tagged Dox content is included only when its language tag
matches the selected canonical tag exactly; a different locale is never used
as fallback. Omitting `--locale` preserves the legacy unfiltered
single-document output.

The locale-selection boundary has stable diagnostics:

- `pdf.locale.invalid` identifies a malformed or noncanonical locale value;
- `pdf.locale.unsupported` identifies a valid locale that this delivery does
  not support; and
- `pdf.locale.unavailable` identifies absent source content for the selected
  locale.

Phase 9 implements deterministic explicit locale selection for single-document
PDF source input. Registry parsing and article/Notice PDF projection remain
Phase 9.1 work.

`watchUrl` is the user-facing destination. `contentUrl` identifies a playable
site-hosted asset. An infographic is independent of video availability.

An external video is projectable only when its status is `published` and it has
a `watchUrl`. A site-hosted video is projectable only when its status is
`published` and it has a `contentUrl`; a `watchUrl` may additionally name a
canonical player page. `draft` and `withdrawn` are valid records that project
no video. A published record lacking its required URL, malformed URLs, a
conflicting duplicate `(articleIdentity, locale)`, or an unsupported
presentation/status is invalid registry metadata and fails metadata loading.

## Existing VideoPublication Compatibility

`PublishMetadata.VideoPublication` remains a first-class supported input. Its
existing observable behavior is preserved:

- `sourcePackage` matching continues to identify `.video` source packages;
- `.video/index.dox` continues to be rewritten to its slug article path;
- a matched publication continues to render the existing self-hosted player,
  captions, and publication links; and
- absent publication metadata for a `.video` source continues to render the
  current diagnostic rather than silently discovering files.

The article-media resolver **must** adapt a compatible `VideoPublication` with
an `articlePath` into one locale-neutral, site-hosted published video variant
for its ordinary article. The adapter uses `publicPath` as `contentUrl`, retains
the publication's caption/link behavior on the existing source-page path, and
does not invent a `watchUrl`.

Native `ArticleMediaPublication` URL rules do not retroactively invalidate a
legacy `VideoPublication`. SmartDox validates `publicPath` only while creating
the normalized adapter. If it is not a valid site-visible `contentUrl`, the
adapter is omitted and SmartDox emits
`article-media.video-publication-invalid-content-url`. The registry still
loads, and the legacy `.video` source-page player continues to receive the
original `publicPath` exactly as it did before Phase 1.

For `articlePath = index.dox`, the stable article identity is derived by
removing the `.video` suffix from the normalized `sourcePackage`; for example,
`concepts/tutorial.video` plus `index.dox` becomes `concepts/tutorial`. For a
non-index article path, the identity is its normalized site-relative path with
the final `.dox` suffix removed. An absent, ambiguous, or invalid article path
does not create an adapter and leaves the legacy video-package behavior
unchanged.

`VideoPublication` has no locale field. Its adapter is therefore explicitly
locale-neutral and is available in every site locale; it is not a fallback from
one localized `ArticleMediaPublication` variant to another. An exact-locale
`ArticleMediaPublication` always wins for the same article and is not merged
with the compatibility variant. Multiple compatible `VideoPublication` records
for one derived article identity produce a deterministic article-media
diagnostic and no compatibility adapter, while their independent legacy source
package behavior remains unchanged.

## Site Projections

SmartDox resolves one variant before either projection is encoded.

- A Notice receives an optional media block containing its infographic and
  only a projectable video reference. Global and category-local Notice YAML
  for the same article and locale receive the same resolved block.
- An ordinary article receives an article-top callout after its effective lead
  and before its first body section. External video renders as a watch link.
  A site-hosted video renders an embedded player only with a registered
  `contentUrl`.
- When no record, locale variant, infographic, or projectable video exists,
  SmartDox keeps the existing article, Notice, dashboard, and feed behavior.

## Non-goals

SmartDox does not upload or inspect YouTube content, scan `target` or artifact
directories, implement Arcadia widget markup, generate video files, or add RDF
media projection in this phase. RDF enrichment remains future work unless a
later phase admits it explicitly.
