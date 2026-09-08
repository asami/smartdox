# Article Header Metadata and Media Actions

Status: design
Date: 2026-09-09

## Purpose

This design assigns the direct DoxSite article-page presentation boundary to
SmartDox. It promotes the settled page anatomy from the exploratory note at
`docs/notes/article-header-metadata-and-media-actions.md` without changing that
note's non-normative status. It consumes the existing article-media publication
resolver; it does not change the registry or introduce an additional media
source.

## Page Anatomy

A direct DoxSite article page is ordered as follows:

```text
Article title, when title data exists
Article-header companion region
  Metadata strip, when it has items
  Media actions, when they have actions
Effective LEAD, when one exists
Inline infographic figure, when one is registered for the resolved variant
Article body
```

When title data exists, SmartDox emits the existing document title as exactly
one HTML `<h1>`, and that title is the page's sole HTML `<h1>`. The
article-header companion region is title-adjacent, distinct from both LEAD and
body content, and is a non-heading region: it emits neither an `<h1>` nor any
other heading element or heading role such as `role="heading"`. It introduces
no synthesized title or additional primary heading. When title data is absent,
SmartDox does not manufacture one: a nonempty header is simply the leading
article region.

The optional infographic is placed immediately after SmartDox's existing
effective-LEAD resolution. Therefore, with no effective LEAD, it follows the
header and precedes the first body section. An absent infographic adds neither
a figure nor an infographic action.

## Metadata Ownership

The initial metadata strip has exactly two reliable input sources, both already
owned by the document:

- the HOCON `tags` property used by DoxSite document fragments, with the
  existing `tag` property as its fallback; and
- `published_at` / `publishedAt`.

Tags preserve their supplied order after each value is trimmed and blank values
are discarded. SmartDox neither expands a hierarchical tag path nor infers a
tag link. It does not manufacture author, language, kind, status, or modified
date data. Every unavailable item is omitted, and a metadata strip with no
items is omitted. In particular, modified history is not surfaced in this
strip.

Metadata must remain meaningful without icons or CSS. SmartDox owns textual,
localized labels and semantic markup: tags are a list, and a published value is
expressed with `time` and a machine-readable `datetime`. The stable labels are:

| Item | `en` | `ja` |
| --- | --- | --- |
| tags | `Tags` | `タグ` |
| publication date | `Published` | `公開日` |

The companion region and its metadata/action portions have stable semantic
classes: `smartdox-article-header`, `smartdox-article-header-metadata`, and
`smartdox-article-header-actions`. The metadata markup additionally exposes
`smartdox-article-header-tags` and
`smartdox-article-header-published-at` for its respective items. These classes
identify semantic regions, not a theme styling API.

## Media Actions and Figure

SmartDox projects only available actions as native, usable anchors, in this
fixed source order:

1. projectable video;
2. summary-slides PDF;
3. article PDF; and
4. infographic.

The localized, stable action labels are:

| Action | `en` | `ja` |
| --- | --- | --- |
| projectable video | `Watch video` | `動画を見る` |
| summary-slides PDF | `Summary slides PDF` | `要約スライド PDF` |
| article PDF | `Article PDF` | `記事 PDF` |
| infographic | `View infographic` | `インフォグラフィックを見る` |

Every action anchor carries the stable semantic class
`smartdox-article-header-action`. The infographic figure carries the stable
semantic class `smartdox-article-infographic`.

The action targets are determined only by the existing resolved media roles:

- an external published video uses `watchUrl`;
- a site-hosted published video uses `contentUrl`;
- each PDF action uses its registered public path; and
- the infographic action targets
  `#smartdox-article-infographic`, the inline figure fragment.

An unavailable role produces no disabled or empty control. A legacy
`VideoPublication` source page retains its established player, captions, and
publication links; only the otherwise duplicate projected video action is
suppressed on that source page. Other available header actions remain
independent.

The inline infographic figure has the stable fragment id
`smartdox-article-infographic`. Its image always has an `alt` attribute. When
`PublishMetadata.ImageReference.alt` is `Some(value)`, including
`Some("")`, the value is preserved exactly. When it is `None`, SmartDox emits
`alt=""` and must not derive alternative text from the path or file name. The
full-size image anchor links to the registered full-size asset and carries the
exact localized accessible name already fixed for the infographic action via
`aria-label`: `View infographic` in `en` and `インフォグラフィックを見る` in
`ja`. This anchor therefore remains named when the registered alt is absent or
empty. The action and figure are independent of file discovery: SmartDox
consumes the existing normalized, exact-locale resolver result.

## Responsibilities and Compatibility

SmartDox owns semantic HTML and semantic classes, localized text labels,
accessibility hooks, deterministic source order, and meaningful no-CSS and
print-safe content. A consuming theme may add button styling or responsive
refinement, but this design adds no theme-specific CSS or JavaScript.

This presentation applies to direct DoxSite article projection, including
Cozy-built BoKs that consume that projection. Cozy receives no new
preprocessor, HTML rewrite, registry schema, or mutation responsibility.
Antora's existing media-callout projection remains a compatibility consumer;
it is not the Phase 15 article-header presentation target.

The article-media design continues to own the normalized registry, direct role
discriminators, filename-inference prohibition, exact-locale resolver, Notice
projection, and `VideoPublication` compatibility behavior. This design owns
only the direct-page header presentation of those already resolved values.

## Required Executable Evidence

A later implementation delivery MUST assert every one of the sixteen
availability combinations of projectable video, summary-slides PDF, article
PDF, and infographic for each exact `en` and `ja` locale through both:

- a direct DoxSite fixture with a physical source root; and
- a Cozy-built BoK fixture whose direct DoxSite projection uses a virtual
  source root.

Those fixtures MUST exercise header projection from the accepted Phase 11
source-root semantics: the physical fixture uses the canonical document
parent, the virtual fixture uses the normalized Realm parent, and both prove
that no process-current-directory fallback or root escape is consulted. A
pre-parsed document alone is not evidence of this propagation. The same
delivery MUST cover the exact localized metadata and action labels, metadata
source and omission cases, placement with and without an effective LEAD,
native-anchor/no-CSS/print behavior, and preserved Notice behavior. It MUST
also cover infographic `Some(nonempty)`, `Some("")`, and `None` alt values,
the exact full-size-anchor `aria-label`, stable fragment and asset link,
Antora compatibility callout behavior, and legacy `VideoPublication` player
behavior with duplicate-video suppression only.

## Explicit Non-goals

This design does not change registry roles or schema, filename inference,
locale fallback, asset discovery, asset generation, upload, publication,
Notice/card/dashboard/feed behavior, Antora design, Cozy code, or theme
styling.
