# Article Media Publication and Site Projection

Status: promoted proposal

Date: 2026-08-03

This note is exploratory and non-normative. Its Phase 1 contract has been
promoted to `docs/design/article-media-publication.md` and
`docs/spec/article-media-publication.md`; those documents govern
implementation. The alternatives and history below remain non-normative.

## Problem

An article may have a detailed infographic produced for its video and a
locale-specific video presentation. The same information must appear in two
different site surfaces without copying URLs into article text:

- an article card needs an infographic action and a video action; and
- the article itself needs a video callout near its beginning.

The card is built from SmartDox Notice metadata. Global and category-top pages
are separate Notice projections, so media must join an article before either
projection is encoded.

## Proposed Normalized Model

The proposed internal model is intentionally provider-neutral:

```text
ArticleMediaPublication
  articleIdentity
  variants: Locale -> ArticleMediaVariant

ArticleMediaVariant
  infographic: Optional[ImageReference]
  video: Optional[VideoReference]

VideoReference
  presentation: external-link | site-hosted
  watchUrl: Optional[URI]
  contentUrl: Optional[URI]
  provider: Optional[String]
  status: draft | published | withdrawn
```

`watchUrl` is the user-facing destination. `contentUrl` identifies a playable
site-hosted artifact. An external presentation normally needs a published
`watchUrl`; a site-hosted presentation normally needs a playable `contentUrl`
and may also have a `watchUrl` for a canonical player location.

An infographic is an optional detailed image reference, not proof that a video
is published. The model must therefore not infer video availability from the
image's presence.

## Illustrative Publication Input

The final wire schema is deliberately undecided. A registry record could have
the following shape:

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
      provider: youtube
      watch_url: https://youtu.be/example-ja
      status: published
  en:
    infographic:
      public_path: /en/development-process/images/example/video-summary-en.png
      media_type: image/png
      alt: Detailed video infographic
    video:
      presentation: external-link
      provider: youtube
      watch_url: https://youtu.be/example-en
      status: published
```

Stable article identity must be distinct from display title, ordering, and
generated Notice number. Relative and public URL resolution needs an explicit
specification rather than a widget-specific convention.

## Proposed SmartDox Projections

SmartDox resolves article media once by article identity and locale.

For Notice data, the proposed optional shape is:

```yaml
notice:
  media:
    infographic:
      src: /ja/development-process/images/example/video-summary-ja.png
      alt: 動画の詳細インフォグラフィック
    video:
      presentation: external-link
      watch_url: https://youtu.be/example-ja
      provider: youtube
      status: published
```

The global Notice and every category-local Notice for the same article must
contain the identical resolved media block. Articles without a matching record
continue to emit ordinary Notice data.

For the article page, the proposed placement is after the title and effective
lead and before the first body section. An external presentation becomes a
callout link. A site-hosted presentation may become an HTML5 player only when
its registered playable content reference is available.

## Compatibility and Boundaries

`VideoPublication` remains supported. SmartDox should translate it into the
normalized internal representation rather than maintain independent page
enhancement paths.

SmartDox reads publication metadata. It must not discover media by scanning
`target`, generated site directories, arbitrary repository directories, or
external providers. It does not upload to YouTube and does not decide a
provider's publishing state.

SimpleModeling.org owns widget presentation: a shared, progressively enhanced
infographic dialog and external video link. Cozy owns generation, artifact
repository placement, and publication registration for BoK media. These are
separate consumers of the same normalized SmartDox contract.

## Questions To Promote Before Implementation

- What exact registry key and URI normalization rule identify an article?
- Should a locale fall back to a default media variant, or must it be absent?
- Which malformed-media cases fail a production build versus emit a diagnostic
  in preview?
- Which existing `VideoPublication` fields map to `watchUrl` and `contentUrl`?
- Does a site-hosted video callout point directly to the content URL or to a
  stable article-player anchor?
