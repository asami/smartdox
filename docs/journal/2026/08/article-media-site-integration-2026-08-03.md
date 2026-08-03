# Article Media And Site Integration Discussion

Date: 2026-08-03

## Purpose

This journal records the discussion about associating article introduction
media with SmartDox site articles and projecting that media into Cozy BoK and
SimpleModeling.org sites.

This is a chronological, non-normative record. The design and behavior
described here must be promoted into `docs/design` and `docs/spec`, with
Executable Specifications, before they become an implementation contract.

## Promotion record — 2026-08-04

The Phase 1 contract is promoted to
`docs/design/article-media-publication.md` and
`docs/spec/article-media-publication.md`. The promoted boundary fixes
`PublishMetadata` as the only association input, exact locale matching without
fallback, validation outcomes, and preservation of existing
`VideoPublication` source-package/player behavior. RDF media projection
remains future scope.

## Step 1.1 completion record — 2026-08-04

`PublishMetadata` now loads and validates provider-neutral
`article-media-publication` records, resolves exact locale variants, and
exposes deterministic compatibility diagnostics. Existing `VideoPublication`
source-page behavior remains independent: valid article paths also produce a
locale-neutral ordinary-article candidate, while invalid compatibility content
paths suppress only that candidate. Focused native metadata, legacy
source-page, and existing generator regression specifications passed before
the Step commit. Notice and article-top projection remain Phase 1 Step 1.2.

## Background

SimpleModeling.org currently has knowledge-centered media packages only for
articles for which introduction videos have been produced. Such packages can
contain:

- a compact Web infographic for the article body;
- a detailed 1920 by 1080 infographic used by the video;
- Japanese and English introduction videos;
- publication records containing language-specific YouTube state and URLs.

The requested site behavior is to use the detailed video infographic and the
published video as additional article introduction actions. These actions are
optional and must not affect articles without media packages.

The same conceptual feature is also required for Cozy-created BoK sites. The
delivery differs:

- a Cozy BoK stores the video artifact in its own artifact repository;
- SimpleModeling.org publishes videos on YouTube;
- Cozy orchestrates a normal BoK build;
- SimpleModeling.org is a special operation that invokes SmartDox site
  generation directly instead of running through `cozy bok`.

SmartDox site generation is therefore the intended common semantic base, while
Cozy and SimpleModeling.org own their respective publication and presentation
integration.

## Confirmed Current Behavior

### SmartDox Notice Generation

SmartDox site generation already collects article metadata into `Notice`
values and emits locale-specific YAML data.

Global notices are written for site top pages, for example:

```text
doxsite.d/WEB-INF/data/ja/notice01.yaml
doxsite.d/WEB-INF/data/en/notice01.yaml
```

Category-local notices are written from the same collected articles, for
example:

```text
doxsite.d/WEB-INF/data/ja/development-process/notice01.yaml
doxsite.d/WEB-INF/data/en/development-process/notice01.yaml
```

The existing Notice contract includes values such as title, title image,
category, article URI, brief, summary, description, publication date, and
status. It does not currently include article infographic or video promotion
data.

Because the global and category-local YAML files are projections of the same
Notice, adding media to the Notice centrally can preserve the article/media
association as article ordering changes.

### SimpleModeling.org Card Widgets

SimpleModeling.org top and category-top pages refer to numbered SmartDox
notices through Arcadia widgets. The main card variants are:

- `headline`;
- `headline-wide`;
- `headline-wide-reverse`.

For example, a top page selects `ja/notice01`, while the Development Process
top page selects `ja/development-process/notice01`. The widgets currently read
values such as `${notice.title}`, `${notice.summary}`,
`${notice.title_image}`, and `${notice.uri}`.

The existing title, summary, image, and article-link integration is working.
The proposed infographic and video actions are not currently present in the
Notice encoder or widgets.

### SimpleModeling.org Build Flow

The current production flow is conceptually:

```text
dox antora
  -> Antora article pages in website.d
dox site
  -> SmartDox site metadata and HTML in doxsite.d
arcadia site
  -> top and category-top pages in arcadiasite.d
copy arcadiasite.d into website.d
```

Consequently, card actions belong to the Notice and Arcadia widget path, while
an action inserted near the beginning of an article must be introduced in the
SmartDox document/site transformation used by Antora generation. Both surfaces
should consume the same publication metadata rather than repeat URLs in Dox
source documents.

### Existing Cozy Video Boundary

Cozy already publishes `.video` package metadata into `src/main/publication`
and stores generated video artifacts under an artifact repository. SmartDox
consumes registered publication metadata and must not scan `target`, `build`,
arbitrary generated directories, or repository directories to discover media.

The existing SmartDox `VideoPublication` path can embed a self-hosted video
player for a `.video/index.dox` source package. The article-media requirement
also covers ordinary articles with optional introduction media, so it needs a
more general article/media association while preserving this compatibility
path.

## Agreed Direction

### Common Article Media Model

Introduce a provider-neutral article media publication concept. A provisional
shape discussed for that concept is:

```text
ArticleMediaPublication
  articlePath
  variants: Locale -> ArticleMediaVariant

ArticleMediaVariant
  promotionImage
  video

VideoReference
  presentation
  watchUrl
  contentUrl
  provider
  status
```

The key distinction is:

- `contentUrl` identifies the video artifact itself when the site hosts it;
- `watchUrl` identifies the user-facing place at which the video is watched;
- `presentation` distinguishes an embedded/site-hosted presentation from an
  external link;
- `provider` may identify YouTube or another provider without making provider
  behavior part of the SmartDox core;
- `status` prevents a generated infographic from being mistaken for proof that
  its corresponding external video is published.

The exact schema and naming remain to be designed. A provisional publication
example is:

```yaml
type: article-media-publication
article:
  public_path: development-process/example.html
variants:
  ja:
    promotion:
      image:
        public_path: /ja/development-process/images/example/video-summary-ja.png
        media_type: image/png
      video:
        presentation: external-link
        watch_url: https://youtu.be/example
        provider: youtube
        status: published
```

The existing `VideoPublication` should be retained as a compatible input and
projected into the common internal model rather than creating two unrelated
video systems.

### Notice Projection

SmartDox should resolve the publication entry by article and locale, then add
optional media data to the corresponding Notice. A provisional Notice YAML
shape is:

```yaml
notice.media:
  infographic:
    src: /ja/development-process/images/example/video-summary-ja.png
    alt: Detailed article infographic
  video:
    watch_url: https://youtu.be/example
    provider: youtube
    status: published
```

The same resolved media must appear in both the global Notice and the
category-local Notice for that article. No media block should be required for
articles without a corresponding publication entry.

### SimpleModeling.org Card Presentation

SimpleModeling.org article cards should keep their existing title image and
article link. For a media-bearing article, the card should additionally show:

- an infographic action that opens the detailed video infographic in a dialog;
- a video action that links to the language-specific published YouTube video.

The three card widgets must use the same shared media-action fragment so that
the actions remain available when an article moves between wide and compact
card positions.

The infographic dialog should be shared by the page instead of emitting one
dialog per card. The intended progressive enhancement behavior is:

- the infographic action is an ordinary link to the image;
- JavaScript intercepts the action when native dialog support is available;
- the detailed image is loaded only when the dialog opens;
- the image source is cleared when the dialog closes;
- without JavaScript, the image link remains usable;
- the dialog has a close action, usable alternative text, and native Escape
  behavior.

Optional actions must not leave empty controls on cards without infographic or
video metadata. Only an external video whose publication status is
`published` and whose `watch_url` is present should be linked.

### Article-Top Presentation

The same resolved video reference should also produce a video callout near the
beginning of the article. The discussed placement is:

```text
article title
effective LEAD
video callout or embedded player
first body section
```

For SimpleModeling.org, the callout links to the language-specific YouTube
video. It should be absent when the video is not published.

For a Cozy BoK, an embedded presentation may render an HTML5 video player near
the same location, or the callout may link to that player through an article
anchor. The publication model should expose a user-facing `watchUrl` so card
rendering does not need to know how the video is stored.

Card actions and article-top presentation must be two projections of the same
article media publication entry. YouTube URLs must not be duplicated manually
in the SmartDox article HEAD or body.

## Cozy BoK Integration

### Artifact Placement

Video artifacts generated for a BoK are stored outside Git, under the BoK
artifact repository, for example:

```text
repository/
  video/
    <module>/
      <version>/
        <video>.mp4
        <video>.srt
        <video>.transcript.json
```

Detailed infographics may likewise be published as repository artifacts when
they are not curated site-source images:

```text
repository/
  media/
    <knowledge-id>/
      video-summary-ja.png
      video-summary-en.png
```

The intended Git and generated-artifact boundaries are:

- `src/main/doxsite`: authored article source, tracked by Git;
- `src/main/publication`: publication paths, hashes, versions, provenance, and
  article/media associations, tracked by Git according to the BoK operation;
- `repository/video` and other repository artifacts: not tracked by Git;
- `doxsite.d`, `antora.d`, and `website.d`: generated outputs, not tracked by
  Git.

"Stored inside the BoK" means that the site and artifact repository are
published as one logical BoK URL space. It does not mean copying MP4 files into
the SmartDox source tree, publication metadata directory, or generated
`doxsite.d` tree.

The final public URL space may therefore look like:

```text
/
  index.html
  articles/...
  repository/
    video/...
```

The physical site and artifact trees may remain separate before deployment.
Cozy staging and upload are responsible for publishing them consistently under
the same origin.

### Cozy Responsibilities

Cozy should:

- generate or publish the video and its sidecars explicitly;
- store those artifacts in the configured repository outside Git;
- write the article/media association into the publication registry;
- pass the publication registry and repository context to SmartDox;
- keep ordinary `bok build` free of heavy media generation or transcoding;
- stage and upload the generated site and repository as one consistent BoK
  publication.

SmartDox should continue to read registered metadata rather than discover
artifacts by scanning the repository.

## SimpleModeling.org Integration

SimpleModeling.org remains a direct SmartDox site user because its site build
and Arcadia top-page operation are special. It should not be required to run
through `cozy bok`.

The direct flow should nevertheless use the same article media publication
contract:

1. The media package produces Japanese and English detailed infographics.
2. The detailed infographics are published to site-visible asset paths.
3. The language-specific production records supply YouTube publication state
   and URL.
4. A SimpleModeling.org integration step projects those records into the
   common SmartDox publication schema.
5. Direct `dox antora` and `dox site` executions consume the same publication
   registry.
6. SmartDox projects the media into article content and global/category Notice
   YAML.
7. Arcadia widgets render the infographic dialog action and YouTube link.

The resulting information flow is:

```text
article media publication
  -> SmartDox site model
     -> global Notice
        -> top-page card widget
     -> category Notice
        -> category-top card widget
     -> transformed article document
        -> Antora article page and article-top video callout
```

SimpleModeling.org currently publishes the compact Web infographic into its
article assets, while the detailed video infographic does not have an
equivalent site publication target in the inspected media packages. That
publication target must be added before the card dialog can use the detailed
image.

## Fallback And Failure Behavior Discussed

The intended presentation matrix is:

| Infographic | Video | Card | Article top |
|---|---|---|---|
| absent | absent | existing card only | no media callout |
| present | absent or unpublished | infographic action only | no video callout |
| present | published external video | infographic and video actions | external video callout |
| present | site-hosted video | infographic and video actions | embedded player or player link |

The existence of an infographic must not imply that a video is uploaded or
published. Preview or draft builds may preserve media references with clear
diagnostics when configured artifacts are unavailable. Production behavior for
missing required local artifacts must align with the existing publication
missing-artifact policy.

## Responsibility Boundary

The discussed boundary is:

- SmartDox owns the provider-neutral article/media model, locale resolution,
  publication consumption, Notice projection, article transformation, and
  semantic/RDF projection;
- Cozy owns BoK media production, artifact-repository storage, publication
  registry updates, BoK orchestration, and deployment of local artifacts;
- SimpleModeling.org owns projection of its media/YouTube production records,
  Arcadia widget presentation, dialog behavior, and its direct site build;
- provider-specific concerns such as YouTube upload remain outside SmartDox.

## Open Design Items

- Define the authoritative schema name, version, and field names for article
  media publication metadata.
- Decide whether locale variants are a map or an ordered collection in the
  persisted schema.
- Define canonical matching between an article source path, public HTML path,
  and media package knowledge ID.
- Define how the existing `VideoPublication` maps into the new common model.
- Decide whether the article-top presentation is introduced as a semantic Dox
  node, a site transformer attachment, or a converter-level fragment.
- Decide the exact internal BoK `watchUrl`: an article anchor, a generated
  media page, or a direct artifact link.
- Define where SimpleModeling.org publishes detailed video infographics and
  which step writes its article-media publication records.
- Confirm optional nested Notice-value behavior in Arcadia and choose a clean
  conditional-rendering mechanism for missing actions.
- Select the shared dialog placement and JavaScript/CSS ownership in the
  SimpleModeling.org Arcadia library.
- Define whether UI action labels come from SmartDox locale data or the
  consuming site's UI localization resources.
- Promote stable decisions into SmartDox design and specification documents.

## Proposed Implementation Sequence

1. Specify the provider-neutral article media publication model.
2. Add SmartDox publication parsing and compatibility projection from existing
   video metadata.
3. Resolve article media by article identity and locale.
4. Extend global and category-local Notice YAML from the same resolved media.
5. Add article-top external-link and embedded presentation behavior.
6. Add SmartDox Executable Specifications for published, unpublished,
   site-hosted, media-free, Japanese, and English cases.
7. Extend Cozy publication generation for BoK-hosted article media while
   preserving Git and artifact-repository boundaries.
8. Add Cozy BoK integration specifications for repository-hosted media.
9. Add the SimpleModeling.org publication projection and detailed infographic
   publication paths.
10. Extend all three SimpleModeling.org card widgets with shared optional media
    actions and the shared infographic dialog.
11. Verify the final SimpleModeling.org top page, category-top page, and article
    page after the existing Antora/Arcadia merge flow.

No source implementation, publication, or site build was performed as part of
this discussion record.
