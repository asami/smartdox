# Article Media Publication Specification

Status: draft specification
Date: 2026-08-04

The authoritative design is
`docs/design/article-media-publication.md`. This specification fixes the
registry input and resolution behavior implemented by SmartDox Phase 1.

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

Phase 1 executable specifications must prove:

- registry parsing, path/URL/enum failure, conflict rejection, and exact locale
  association;
- absent metadata and absent locale behavior;
- external published/draft/withdrawn and site-hosted presentation behavior;
- identical global/category Notice media data;
- article-top link/player selection; and
- all preserved `VideoPublication` source-package, slug, player, caption/link,
  and diagnostic behaviors; and
- `index.dox` and non-index derived identity, `publicPath -> contentUrl`,
  locale-neutral availability, exact-record precedence, duplicate-conflict
  suppression, and invalid-legacy-publicPath adapter suppression.
