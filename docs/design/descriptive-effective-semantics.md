# Descriptive Effective Semantics

status=design
published_at=2026-06-23

---

# Overview

SmartDox is the semantic reference implementation for descriptive metadata used
by SmartDox, Cozy, CNCF, and SimpleModeling tools. Source documents normalize
metadata into `Explanation` / `DocumentMetaData` and then into downstream site,
publication, RDF, and dashboard metadata.

The common descriptive fields are:

- `headline`
- `brief`
- `summary`
- `description`
- `lead`
- `abstract`
- `remarks`
- `tooltip`

# Effective Rules

Effective values are consumer-specific and use these precedence orders:

| Effective accessor | Precedence |
| --- | --- |
| `effectiveHeadline` | `headline -> brief -> tooltip` |
| `effectiveBrief` | `brief -> summary -> lead -> abstract -> headline` |
| `effectiveSummary` | `summary -> lead -> abstract -> description -> brief` |
| `effectiveDescription` | `description -> abstract -> lead -> summary` |
| `effectiveTooltip` | `tooltip -> brief -> headline -> summary -> abstract` |

Explicit metadata wins over text distilled from body content. If a parser creates
a distilled summary from body text, code that needs authored metadata should
inspect explicit metadata properties before falling back to distilled effective
values.

# Cross-Product Use

- SmartDox owns parsing and Dox IR normalization.
- goldenport-scala-library exposes compatible `DescriptiveAttributes` effective
  accessors for non-Dox metadata.
- CNCF projects component, operation, and entity metadata with the same field
  names and effective order.
- Cozy consumes SmartDox metadata and must not define a separate effective order.

# Glossary And Markdown

Glossary term sources may be SmartDox or Markdown. Markdown is parsed through
SmartDox Markdown mode and normalized into Dox IR. Term title, summary, brief,
description, and body definition use the same metadata path as article pages.
`reading` is term-specific metadata and remains outside the generic descriptive
field set.
