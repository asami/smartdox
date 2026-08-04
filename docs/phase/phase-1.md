# Phase 1: Article Media Publication and Site Projection

Status: closed

Start date: 2026-08-03

## Goal

Make an optional article-media publication record the single SmartDox input for
an article's detailed infographic and video presentation. SmartDox must project
the resolved, locale-specific media consistently into the article page and the
Notice data consumed by global and category-top article cards.

## Scope

In scope:

- a provider-neutral article/media publication metadata model with article and
  locale identity;
- optional detailed infographic and video presentation references;
- distinction between a site-hosted content URL and a user-facing watch URL;
- publication status that prevents unpublished external video links;
- parser and publication-registry integration for the new metadata;
- promotion of the publication contract to reviewed design and specification
  documents before implementation;
- compatible projection of existing `VideoPublication` input;
- Notice media projection for both global and category-local Notice YAML; and
- article-top media callout or embedded-player projection according to the
  selected presentation type.

Out of scope:

- YouTube upload, verification, or provider API integration;
- Arcadia widget HTML/CSS/JavaScript implementation in SimpleModeling.org;
- Cozy media generation, transcoding, or BoK artifact staging;
- discovery by scanning `target`, generated site output, or an artifact
  repository; and
- a requirement that every article has video or an infographic.

## Responsibility Boundary

### SmartDox

- owns metadata parsing, normalization, article/locale association, and
  deterministic page and Notice projection;
- treats publication metadata as the source of media association; and
- preserves existing site behavior when no article-media record exists.

### SimpleModeling.org

- publishes its video on YouTube;
- supplies the locale-specific published watch URL and site-visible infographic
  asset path; and
- renders Notice media through its existing card widgets.

### Cozy BoK

- creates or registers the same metadata for an internally hosted BoK video;
- keeps video binaries and derived artifacts in a Git-excluded artifact
  repository; and
- stages the site and artifact repository under one BoK URL space.

## Stage 1.1: Publication Contract and Compatibility

Stage Status:

- Current status: DONE
- Owner: SmartDox
- Update rule: mark work complete only from the Phase 1 checklist.
- Checklist basis: `AMP1-01`

Focus:

- define the normalized article-media model and publication input shape;
- promote the article-media contract into `docs/design` and `docs/spec` before
  parser or site-projection implementation;
- make `PublishMetadata` the only association input boundary and keep
  `DocumentPropertiesParser` out of scope;
- define missing, invalid, draft, and published media outcomes; and
- preserve compatible `VideoPublication` source-package matching, `.video`
  source-page rewriting, player/caption/link rendering, and diagnostics while
  deterministically mapping its optional `articlePath` into the normalized
  model with locale-neutral compatibility, explicit-record precedence, and
  conflict diagnostics; suppress an invalid legacy publicPath only from the
  adapter without invalidating its registry or source-page behavior.

## Stage 1.2: SmartDox Site Projection

Stage Status:

- Current status: DONE
- Owner: SmartDox
- Update rule: mark work complete only from the Phase 1 checklist.
- Checklist basis: `AMP1-02`
- Closure evidence: Stage review findings 2, bounded fixes 2, focused
  re-review CLEAN; `Test/compile` succeeded and the three focused integration
  specifications passed 30/30 on 2026-08-04.

Focus:

- resolve article and locale media before Notice encoding;
- emit the same optional media data for global and category-local notices; and
- project an article-top callout or player without duplicating URLs in Dox
  source.

Projection schema decision:

- Notice media inherits the normalized article-media schema names:
  `public_path`, `media_type`, `alt`, `presentation`, `status`, `provider`,
  `watch_url`, and `content_url`;
- only `projectableVideo` is projected, so an emitted video has
  `status: published`; and
- absent infographic and video data omit the optional media block.

Slice ledger:

- `AMP1-02A` — Article and Notice Projection — ACCEPTED
  - Authority: `docs/design/article-media-publication.md` and
    `docs/spec/article-media-publication.md`.
  - Acceptance: all `AMP1-02` checklist outcomes, including exact-locale
    global/category Notice equality, article-top external/site-hosted
    selection, and unchanged absent-media behavior.
  - Repository boundary: SmartDox only.
  - Focused validation: `ArticleMediaProjectionSpec`, preserved
    `VideoPublicationCompatibilitySpec`, and `git diff --check`.
  - Acceptance evidence: media projection specification 7/7, compatibility
    specification 3/3, final Stage integration specifications 30/30,
    preserved public JVM descriptors, and clean focused re-reviews after the
    bounded Slice and Stage review fixes.

## Stage 1.3: SimpleModeling.org Acceptance Boundary

Stage Status:

- Current status: DONE
- Owner: SmartDox / SimpleModeling.org
- Update rule: mark work complete only from the Phase 1 checklist.
- Checklist basis: `AMP1-03`
- Closure evidence: Stage integration review findings 2, bounded repairs 2,
  focused re-review CLEAN; `Test/compile` succeeded and the three focused
  integration specifications passed 31/31 on 2026-08-04.

Focus:

- prove the Notice schema serves the existing widget path;
- verify a published YouTube link and detailed infographic are locale-specific;
- retain a usable article card when media is absent; and
- hand off the widget presentation work without making it a SmartDox core
  dependency.

Slice ledger:

- `AMP1-03A` — SimpleModeling.org Notice/Widget Acceptance — ACCEPTED
  - Authority: `docs/design/article-media-publication.md`,
    `docs/spec/article-media-publication.md`, and the `AMP1-03` checklist.
  - Acceptance: a bilingual SimpleModeling.org fixture supplies detailed
    infographic and published YouTube references; generated global/category
    Notices resolve the exact locale media; adding media preserves numbered
    Notice ordering and the existing card identity fields; and a card without
    media remains usable.
  - Repository boundary: SmartDox mutation and validation only;
    SimpleModeling.org is read-only evidence, and its existing dirty work is
    protected.
  - Focused validation: `ArticleMediaProjectionSpec` and `git diff --check`.
  - Non-goal: Arcadia widget HTML/CSS/JavaScript implementation, which remains
    a downstream handoff.
  - Acceptance evidence: bilingual SimpleModeling.org metadata and article
    fixtures; exact English/Japanese infographic and YouTube projection;
    unchanged global/category numbered Notice ordering and explicit widget
    card fields; preserved media-free card behavior; focused specification
    9/9; Stage review findings `AMP1-03-R1` and `AMP1-03-R2` resolved; clean
    focused re-review; and final Stage integration specifications 31/31.

## Phase Closure

Stage Status:

- Current status: CLOSED
- Owner: SmartDox
- Update rule: reopen only through a new authoritative phase decision; ordinary
  follow-up work belongs to a successor phase or maintenance task.
- Checklist basis: `AMP1-04`

Closure evidence:

- Each implementation Stage completed focused executable specifications,
  `Test/compile`, read-only review, bounded finding repair, and a clean focused
  re-review where repairs were made.
- The final release gate runs the three Phase 1 integration specifications and
  the full SmartDox test suite from the committed Step tree before accepting
  this closure record.
- The SimpleModeling.org handoff is the bilingual fixture and Notice/widget
  acceptance in `AMP1-03A`, including locale-specific infographic and YouTube
  references, stable Notice ordering and card identity, and media-free cards.
- The Cozy handoff is the provider-neutral publication contract and the tested
  `site-hosted` projection: a published `content_url` produces the article
  player and matching Notice media while media generation, artifact staging,
  and BoK integration remain outside SmartDox Phase 1.
- Phase 1 introduced no accepted hygiene follow-up and selects no successor
  phase.

## Completion Criteria

Phase 1 closes when the reviewed design and specification fix the
PublishMetadata-only article-media contract, and SmartDox can consume registered article-media metadata,
preserve `VideoPublication` compatibility, and deterministically emit the
same resolved optional media for an article page plus every corresponding
global/category Notice. Executable specifications must cover locale resolution,
missing media, unpublished external video, and the existing video-publication
path.

## References

- `docs/phase/phase-1-checklist.md`
- `docs/design/article-media-publication.md`
- `docs/spec/article-media-publication.md`
- `docs/notes/article-media-publication-and-site-projection.md`
- `docs/journal/2026/08/article-media-site-integration-2026-08-03.md`
