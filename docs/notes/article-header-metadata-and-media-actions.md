# Article Header Metadata and Media Actions

Status: proposal

Date: 2026-09-08

This note is exploratory and non-normative. It records the proposed SmartDox
site-page presentation boundary for later promotion into design and
specification documents.

## Problem

SmartDox already resolves article PDF, summary-slides PDF, video, and
infographic references from publication metadata. The current article-page
projection does not present them as one discoverable set:

- article and summary-slide PDFs are plain links in a callout after the lead;
- video is rendered in the same late callout, but does not form a clear action
  group with the PDFs;
- infographic is exposed to Notice consumers but is not projected into the
  article page; and
- the area immediately below the title has no stable semantic region in which
  article tags and other compact metadata can later coexist with media
  actions.

The registered assets are therefore usable but visually weak and difficult to
find. Cozy-built BoK articles inherit the same limitation because they consume
the common SmartDox site projection.

## Proposed Page Anatomy

The generated article begins with three distinct regions:

```text
Article title
  Article header metadata region
    compact metadata strip
    media action group
  Effective LEAD
  Inline infographic figure
  Article body
```

The title remains the primary heading. The region immediately below it is an
article-header companion, not part of the lead or body.

### Compact Metadata Strip

The metadata strip is reserved for concise, symbolically recognizable facts
such as tags, publication or update date, language, author, content kind, and
publication status when those values are actually available. The first
delivery should begin with metadata already represented reliably by SmartDox,
especially tags and dates, rather than manufacture missing values.

Each item has machine-readable semantics and accessible text. An icon may
support recognition, but must not be the only label. Long descriptions and
raw metadata dumps do not belong in this strip. The strip may wrap on narrow
screens while retaining deterministic item order.

### Media Action Group

Available article media are rendered as a compact group of button-styled
links in this order:

1. watch video;
2. summary-slides PDF;
3. article PDF; and
4. infographic.

Only available, projectable media produce actions; unavailable media do not
produce disabled or empty controls. The labels are localized, remain usable
without icons, and expose appropriate link/media semantics. Styling must allow
the group to wrap responsively and remain intelligible when CSS or JavaScript
is unavailable.

The infographic action targets the inline infographic figure on the same
page. The figure itself links to the registered full-size asset, so the action
provides discovery while the image provides direct inspection. External video
and document actions retain their registered destinations. Site-hosted video
may target its stable player or content reference according to the existing
publication contract.

## Infographic Placement

When a locale-resolved infographic exists, SmartDox projects it as a semantic
figure immediately after the effective LEAD and before the first body section.
The registered alt text is preserved; an available caption may be displayed,
and the image links to the original registered asset.

If an article has no effective LEAD, the deterministic fallback is after the
article-header region and before the first body section. Absence of an
infographic leaves the page structure unchanged and omits its action.

The inline figure is independent of video availability. SmartDox must not
infer one medium from another or discover files by scanning generated output.

## Ownership and Cozy BoK Reuse

SmartDox owns the provider-neutral page anatomy, semantic HTML/classes,
localized action labels, accessibility hooks, deterministic ordering, and
publication-metadata projection. A site theme may refine appearance, but
should not have to reconstruct media roles from filenames or Notice data.

Cozy continues to generate, register, and publish BoK media artifacts. A Cozy
BoK supplies the existing article-media publication roles and receives the
same SmartDox article header, action group, and inline infographic behavior as
any other DoxSite consumer. No Cozy-only HTML transformation or duplicated
registry schema is introduced.

## Compatibility and Validation Direction

- Keep the current `article_pdf`, `summary_slides_pdf`, `video`, and
  `infographic` registry roles and exact-locale resolution semantics.
- Preserve Notice media projection for cards, dashboards, and other consumers.
- Give the new header, metadata, action, and infographic regions stable
  semantic class names without making their styling a site-specific contract.
- Prove deterministic placement with and without LEAD, partial media sets,
  locale-specific labels, tag/date metadata, and no-media pages.
- Prove keyboard navigation, meaningful link text, image alternative text,
  responsive wrapping, and a useful print representation.
- Verify the same generated contract with a direct SmartDox fixture and a
  Cozy BoK publication fixture.

## Questions for Design Promotion

- Which existing SmartDox tag source is canonical for the first metadata-strip
  delivery, and how are hierarchical tags presented compactly?
- Which date, author, language, kind, and status fields are sufficiently stable
  to include in the initial strip?
- Should the site-hosted video action scroll to an inline player or open its
  registered player/content destination?
- Which baseline presentation belongs to SmartDox and which refinements remain
  owned by a consuming site theme?
