# Article Header Metadata and Media Actions Specification

Status: normative specification
Date: 2026-09-09

The authoritative design is
`docs/design/article-header-metadata-and-media-actions.md`. This specification
defines the direct DoxSite article-header projection contract. It consumes the
normalized article-media result specified by
`docs/spec/article-media-publication.md`; it does not alter media registration,
resolution, or publication behavior.

## Direct Article Structure

A direct DoxSite article projection MUST emit, in source order:

1. the existing document title as exactly one HTML `<h1>`, and the page's sole
   HTML `<h1>`, when title data is present;
2. a distinct title-adjacent article-header companion region when it has
   metadata or actions;
3. the existing effective LEAD when one is present;
4. the registered infographic figure when one is present for the resolved
   variant; and
5. the article body.

The companion region is neither LEAD nor body content and MUST be a
non-heading region. It MUST NOT emit an `<h1>`, any other heading element, or a
heading role such as `role="heading"`; it MUST NOT synthesize a title or
another primary heading. When title data is absent, an otherwise nonempty
companion region is the leading article region. A header without both metadata
and actions MUST be omitted.

The header region MUST use the class `smartdox-article-header`. Its metadata
and action portions, when present, MUST use
`smartdox-article-header-metadata` and `smartdox-article-header-actions`
respectively.

## Metadata Strip

The metadata strip MUST read only these initial reliable sources:

- the document HOCON `tags` property used by DoxSite document fragments, with
  the existing `tag` property as fallback; and
- `published_at` or `publishedAt`.

Tag values MUST be trimmed, blank values MUST be omitted, and remaining values
MUST retain supplied order. SmartDox MUST NOT expand hierarchical tag paths,
infer tag links, or manufacture author, language, kind, status, or
modified-date metadata. Every absent item MUST be omitted. The strip itself
MUST be omitted when it would contain no item, and it MUST NOT surface modified
history.

Metadata MUST carry localized textual labels and semantic markup independently
of icons and CSS. The labels are stable for these exact locales:

| Item | `en` | `ja` |
| --- | --- | --- |
| tags | `Tags` | `タグ` |
| publication date | `Published` | `公開日` |

Tags MUST be represented as a list in a
`smartdox-article-header-tags` metadata item. A publication date MUST be
represented with `time`, including a machine-readable `datetime`, in a
`smartdox-article-header-published-at` metadata item.

## Header Actions

For the one existing exact-locale resolved variant, the header MUST emit an
action only for each available role, in this fixed order:

1. a projectable video;
2. a summary-slides PDF;
3. an article PDF; and
4. an infographic.

Each action MUST be a native usable anchor with the following stable localized
text:

| Role | `en` | `ja` |
| --- | --- | --- |
| projectable video | `Watch video` | `動画を見る` |
| summary-slides PDF | `Summary slides PDF` | `要約スライド PDF` |
| article PDF | `Article PDF` | `記事 PDF` |
| infographic | `View infographic` | `インフォグラフィックを見る` |

Every action anchor MUST carry the stable semantic class
`smartdox-article-header-action`.

An external projectable video action MUST target `watchUrl`; a site-hosted
projectable video action MUST target `contentUrl`. PDF actions MUST target the
registered public path for their respective roles. The infographic action MUST
target `#smartdox-article-infographic`. An unavailable role MUST emit neither a
disabled nor an empty control.

On a legacy `VideoPublication` source page, SmartDox MUST preserve the
established player, captions, and publication links, and MUST suppress only the
duplicate projected video action. This suppression MUST NOT suppress independent
available PDF or infographic actions.

## Infographic Figure

When the resolved variant has an infographic, SmartDox MUST emit one figure
with class `smartdox-article-infographic` and id
`smartdox-article-infographic` immediately after its existing effective-LEAD
resolution. The image MUST always have an `alt` attribute. When
`PublishMetadata.ImageReference.alt` is `Some(value)`, including `Some("")`,
the emitted alt value MUST be preserved exactly. When it is `None`, the image
MUST emit `alt=""`; SmartDox MUST NOT derive alternative text from its path or
file name. The image MUST be linked to the registered full-size asset, and
that full-size image anchor MUST carry the exact localized accessible name
already fixed for the infographic action via `aria-label`: `View infographic`
for `en` and `インフォグラフィックを見る` for `ja`. This anchor name MUST
remain present when the registered alt is absent or empty. With no effective
LEAD, the figure MUST follow the header and precede the first body section.
With no infographic, SmartDox MUST emit neither the figure nor its header
action.

## Ownership and Compatibility

SmartDox MUST own the semantic HTML and classes, localized text labels,
accessibility hooks, deterministic source order, and meaningful content without
CSS and when printed. A consuming theme MAY provide button styling or
responsive refinement, but this contract introduces no theme-specific CSS or
JavaScript.

This contract applies to direct DoxSite article projection, including a
Cozy-built BoK that uses that projection. Cozy MUST receive no new
preprocessor, HTML rewrite, registry schema, or mutation for this behavior.
The existing Antora media-callout projection MUST remain a compatibility
consumer and is not the header presentation target.

This specification MUST NOT change the article-media registry roles or schema,
filename-inference prohibition, exact-locale behavior, asset discovery,
generation, upload, publication, Notice/card/dashboard/feed behavior, Antora
design, Cozy code, or theme styling. It does not redefine the existing
`VideoPublication` semantics.

## Required Executable Evidence

A later implementation delivery MUST add executable specifications covering:

- every one of the sixteen availability combinations of projectable video,
  summary-slides PDF, article PDF, and infographic, including complete,
  partial, and absent action groups, for each exact `en` and `ja` locale through
  both (a) a direct DoxSite fixture with a physical source root and (b) a
  Cozy-built BoK fixture whose direct DoxSite projection uses a virtual source
  root;
- source-root propagation in both fixtures using the accepted Phase 11
  semantics (canonical physical document parent or normalized virtual Realm
  parent), proving no process-current-directory fallback or root escape and
  not merely inspecting a pre-parsed document;
- the exact `en` and `ja` locales and every stable metadata/action label;
- metadata present and absent cases, including `tags` / `tag` fallback,
  trimming, blank omission, supplied order, publication display, and the
  absence of modified-history display;
- placement with an effective LEAD and with no effective LEAD;
- infographic `Some(nonempty)`, `Some("")`, and `None` alternative text,
  deterministic image `alt`, full-size-asset link, stable fragment, exact
  full-size-anchor `aria-label`, and action target;
- keyboard operation through native anchors, plus meaningful text when CSS is
  unavailable and in print output;
- one direct SmartDox fixture and one Cozy BoK fixture that uses direct DoxSite
  projection; and
- preserved Notice behavior, the Antora compatibility callout behavior
  (including its historical link/player contract), and legacy
  `VideoPublication` source-page player behavior, including duplicate-video
  action suppression only.
