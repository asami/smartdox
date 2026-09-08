# Article Media Publication Specification

Status: draft specification
Date: 2026-08-29

The authoritative design is
`docs/design/article-media-publication.md`. This specification fixes the
article-media/video behavior implemented by SmartDox Phase 1. Its PDF-role
sections define the Phase 9 contract for the Phase 9.1 registry and projection
consumer. Its locale-selection section specifies Phase 9's deterministic
explicit locale selection for single-document PDF source input.

## Registry Entry

An article-media record is one `PublishMetadata` registry entry whose `type` is
`article-media-publication`.

```yaml
type: article-media-publication
article:
  identity: development-process/example
variants:
  ja:
    infographic:
      public_path: /ja/development-process/images/example/video-summary-ja.png
      media_type: image/png
      alt: 動画の詳細インフォグラフィック
    article_pdf:
      public_path: /ja/development-process/pdf/example-article-ja.pdf
      media_type: application/pdf
      label: 記事 PDF
    summary_slides_pdf:
      public_path: /ja/development-process/pdf/example-summary-ja.pdf
      media_type: application/pdf
      label: 要約スライド PDF
    video:
      presentation: external-link
      status: published
      provider: youtube
      watch_url: https://youtu.be/example-ja
```

`article.identity` is required. It is normalized as a non-empty, site-relative
logical path. It must not contain a leading slash, `.` or `..` path segment, a
locale prefix, or a generated `.html` / `.dox` suffix.

`variants` is required and maps a canonical locale tag to one variant. A
duplicate normalized `(article.identity, locale)` is invalid even when its
metadata is otherwise equal.

An infographic has a required non-empty `public_path`, and optional
`media_type` and `alt`. Its path is a registered site-visible reference;
SmartDox does not verify an artifact by scanning a filesystem.

## Localized PDF Roles

Each variant may independently contain the direct role fields
`article_pdf` and `summary_slides_pdf`. Each present field is a single
document reference with this shape:

```yaml
article_pdf:
  public_path: /ja/development-process/pdf/example-article-ja.pdf
  media_type: application/pdf
  label: 記事 PDF
summary_slides_pdf:
  public_path: /ja/development-process/pdf/example-summary-ja.pdf
  media_type: application/pdf
  label: 要約スライド PDF
```

`article_pdf` and `summary_slides_pdf` are the only role discriminators. A
separate role field and filename inference are forbidden. For each present
role, `public_path` is required, non-empty, and site-visible; `media_type` is
required and exactly `application/pdf`; `label` is optional and must be
nonblank when supplied. Either role may be absent.

Registry parsing and projection reject duplicate role entries, invalid paths or
media types, and invalid locale association. Role entries are not merged, and
one role is never a fallback for the other. PDF `public_path` values are
locale-agnostic site-visible references; only the enclosing canonical variant
key associates a reference with a locale. SmartDox does not infer locale from
a path or filename and does not require a locale path prefix. Locale variants
are matched exactly; no locale fallback or merging is permitted.
These PDF registry rules are the accepted Phase 9 contract consumed by Phase
9.1; this specification does not redefine them.

Existing infographic, external-video, site-hosted-video, legacy
`VideoPublication`, and no-media behavior remains compatible under the Phase
9.1 PDF projection contract.

## Single-Document PDF Locale Selection

The single-document PDF operation accepts an optional `--locale` selector.
For this delivery, the selector accepts only canonical `ja` or `en`.
Locale-neutral content remains in the output. Language-tagged Dox content is
selected only when its language tag exactly matches the selected canonical
tag; content in a different locale is never a fallback. When `--locale` is
omitted, the legacy unfiltered single-document output is preserved.

The selector reports stable diagnostics with these meanings:

- `pdf.locale.invalid`: the value is malformed or noncanonical;
- `pdf.locale.unsupported`: the value is valid but unsupported by this
  delivery; and
- `pdf.locale.unavailable`: source content for the selected locale is absent.

Phase 9 implements deterministic explicit locale selection for single-document
PDF source input. Registry parsing and article/Notice PDF projection remain
Phase 9.1 work.

## Video Rules

`presentation` is either `external-link` or `site-hosted` and `status` is one
of `draft`, `published`, or `withdrawn`.

| Presentation | Published requirement | Projection |
| --- | --- | --- |
| `external-link` | valid absolute `watch_url` | external article/Notice link |
| `site-hosted` | valid site-visible `content_url` | article player and Notice content reference |
| either, `draft` or `withdrawn` | no URL is required | no video projection |

A published external video without `watch_url`, a published site-hosted video
without `content_url`, an invalid URI, or an unknown enum value is a registry
load failure. A valid but unavailable video state does not suppress its
infographic.

## Resolution

The resolver receives `(articleIdentity, locale)` and returns at most one
variant. Both values are normalized before comparison. Locale matching is
exact; no default-locale or cross-locale fallback is permitted. No matching
record produces no media block and is not an error.

The resolver is invoked before Notice encoding. Every global and category-local
Notice projection for one `(articleIdentity, locale)` uses the same resolved
result. Article pages use that same result; they do not read an alternative
Dox HEAD property.

## PDF Projection

For the one exact-locale resolved variant, Notice `media` may contain direct
`article_pdf` and `summary_slides_pdf` maps. Each present map preserves the
resolved `PdfDocumentReference` exactly as `public_path`, `media_type`, and
optional `label`. An absent role is omitted; SmartDox emits neither an empty
role map nor an empty `media` block.

Direct DoxSite article-page anatomy, metadata, action order and labels,
semantic markup, and inline infographic behavior are specified by
`docs/spec/article-header-metadata-and-media-actions.md`. That specification
consumes the resolved roles defined here. In particular, it does not alter the
role values, projection eligibility, or exact-locale result defined by this
specification.

## Antora Compatibility Projection

The existing Antora article-top media callout MUST remain a distinct
compatibility consumer from the direct DoxSite article-header contract. For an
ordinary article it MUST be placed after the effective lead and before the
first body section. Its outer container MUST be `smartdox-article-media`;
present PDF roles MUST be grouped in `smartdox-article-media-pdf` and MUST
retain the fixed Antora role order article PDF, then summary-slides PDF.

A supplied PDF label MUST be rendered verbatim. An absent label MUST use the
exact locale default `Article PDF` or `Summary slides PDF` in `en`, and
`記事 PDF` or `要約スライド PDF` in `ja`. The optional video MUST follow the
PDF group in an exclusive `smartdox-article-media-video` sub-block. A
PDF-only callout MUST retain the outer/PDF markup and MUST omit the video
sub-block. An external video MUST render as an Antora watch-link using
`watchUrl`; a site-hosted video MUST render as an Antora player only when its
registered `contentUrl` is present. This historical callout/link/player
contract is compatibility behavior and does not redefine the separate direct
DoxSite header projection.

On an established legacy `VideoPublication` source page, the Antora
compatibility callout MUST retain its existing player, captions, and
publication links and MUST suppress only the duplicate
`smartdox-article-media-video` sub-block. Available Antora PDF role links MUST
remain projectable. This Antora sub-block suppression is separate from the
direct DoxSite header rule, which MUST suppress only its own duplicate
projected video action on that source page; neither rule suppresses independent
PDF or infographic actions.

The ordinary article and every global/category Notice projection for one
`(articleIdentity, locale)` consume the same exact resolved result. There is
no role merge, filename inference, or locale fallback at projection time.

For a direct article, SmartDox emits no media actions or inline figure only
when neither (a) a native exact-locale `ArticleMediaPublication` variant nor
(b) a valid locale-neutral `VideoPublication` compatibility candidate resolves.
Within an otherwise resolved variant, an absent infographic, PDF role, or
projectable video emits no corresponding direct-page media action or inline
figure while leaving the other available roles independent.

## VideoPublication Compatibility

Existing `VideoPublication` input stays valid. `sourcePackage` matching,
`.video/index.dox` slug rewriting, matched self-hosted player/caption/link
rendering, and missing-metadata diagnostics retain their current behavior.

When `VideoPublication.articlePath` is valid, SmartDox must add one
locale-neutral compatibility candidate:

| Existing input | Derived article identity | Normalized video |
| --- | --- | --- |
| `sourcePackage = concepts/tutorial.video`, `articlePath = index.dox` | `concepts/tutorial` | `site-hosted`, `published`, `contentUrl = publicPath` |
| a non-index `articlePath = concepts/tutorial.dox` | `concepts/tutorial` | `site-hosted`, `published`, `contentUrl = publicPath` |

The source-package form requires a normalized `.video` package. The non-index
form strips exactly one final `.dox` suffix. An absent, invalid, or ambiguous
path creates no compatibility candidate; it is not a registry-load error and
does not affect legacy `.video` source-page behavior.

Legacy `publicPath` is grandfathered for the existing video-source-page path.
SmartDox validates it only when constructing the normalized compatibility
candidate. A value that is not a valid site-visible `contentUrl` leaves the
registry and legacy player behavior unchanged, emits
`article-media.video-publication-invalid-content-url`, and suppresses only that
ordinary-article candidate. It must not become a registry-load failure.

The candidate is available in every locale because `VideoPublication` carries
no locale. This is an explicit locale-neutral legacy source, not locale
fallback. Resolution precedence is: an exact-locale
`ArticleMediaPublication` variant wins as a complete variant; SmartDox does
not merge its infographic or unavailable video with a compatibility candidate.
Two compatible `VideoPublication` values deriving the same article identity
produce the deterministic diagnostic
`article-media.video-publication-conflict` and no compatibility candidate.
The adapter never changes the existing video-source-page rewrite/player path.

## Required Executable Evidence

Phase 9.1 executable specifications must prove:

- registry parsing, path/URL/enum failure, conflict rejection, and exact locale
  association;
- absent metadata and absent locale behavior;
- external published/draft/withdrawn and site-hosted presentation behavior;
- identical global/category Notice media data;
- Antora compatibility article-top callout link/player selection, including
  its outer/PDF/video containers, fixed role order, exact supplied/default
  labels, PDF-only video omission, and `watchUrl`/`contentUrl` requirements;
- exact-locale PDF role links, verbatim/default labels, deterministic PDF/video
  ordering, absent-role omission, absent-media omission, and legacy-video
  duplicate suppression without suppressing available PDFs;
- all preserved `VideoPublication` source-package, slug, player, caption/link,
  and diagnostic behaviors; and
- `index.dox` and non-index derived identity, `publicPath -> contentUrl`,
  locale-neutral availability, exact-record precedence, duplicate-conflict
  suppression, and invalid-legacy-publicPath adapter suppression.
