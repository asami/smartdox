# Markdown source AST contract

Status: frozen Phase75 / Step75.1 / Slice75.1B2 specification
Date: 2026-10-01

## Selection and metadata

After source admission, `.md` and `.markdown` select Pure by default. Explicit
`Config.MarkdownMode.Enhanced` selects the existing mixed Markdown/Dox grammar.
Source contents never select a mode or trigger fallback. Both modes return the
existing SmartDox `Document`; `.dox` and `.org` retain their existing parser.
Pure uses CommonMark plus GFM tables and projects its parsed AST into native
SmartDox nodes without invoking any Dox grammar on source or AST leaves.

Config retains its original six constructor, product, apply, unapply and copy
parameters. Explicit Markdown mode survives configuration copies, including
style and filename changes. Selecting a mode changes only the fresh result;
original configurations and shared Pure/Enhanced presets retain their modes.
Reconstruction through the six-field constructor starts with Pure.

The static `parse(config, String)` API splits and merges YAML front matter once
for both modes. Pure starts with an empty `Head`; HEAD, SUMMARY, LEAD, status,
_source, macros, include directives and Org brackets remain body content.
Direct class parsing, parseOne and parseFragment remain body APIs. Pure parse2
uses the static metadata API. Structured LogicalBlock APIs retain their route.

## Native projection

Headings terminate at one line, create Sections at their authored level (# maps
to level 1 and existing h2 output), and nest by relative level in each container.
Preamble and equal-level siblings remain. Emphasis, strong, code and links map to
Em, Strong, Code and Hyperlink; titles use I18NString. Unresolved references are
literal. Soft breaks become a space; hard breaks become br. Tight lists flatten
direct paragraphs, loose lists retain them, ordered starts other than 1 become
start attributes. Nested lists remain typed; `term :: definition` stays text.

Quotes and thematic breaks map to BlockQuote and HorizontalRule. Fenced and
indented code map to literal Program with empty callouts; the first fenced-info
token becomes kind. GFM tables map to Table/THead/TBody/TR/TH/TD and aligned
Colgroup/Col; no alignment means no Colgroup.

Inline and reference images use CommonMark syntax, plain child-text alt,
optional title attributes and existing local resource admission. Japanese and
empty alt are retained. Physical, virtual and absent resource origins preserve
normalization and unsupported-resource diagnostics; no existence/cwd probe.
The raw Dox-inline image grammar retains its restrictions.

Raw HTML fragments use JSoup, ordinary native inline formatting and generic
Html5/Html5Inline nodes, with native hyperlinks/images. HTML comments disappear.
Only HTML-bearing inline sequences use CommonMark HTML rendering followed by
DOM projection. HTML text never enters Markdown or Dox grammar again; Dox tags,
HEAD and includes inside HTML have no operational interpretation.

Locations use the first CommonMark span (1-based body line/column) and configured
file when enabled. Image diagnostics use the original body slices designated
by source spans; these slices are attribution only. Missing spans retain the
configured location; disabled locations remain absent.

## Evidence and exclusions

[MarkdownDocumentParserSpec](../../src/test/scala/org/smartdox/parser/MarkdownDocumentParserSpec.scala)
provides Given/When/Then behavior evidence; legacy outputs remain explicitly
Enhanced in DoxMarkdownParserSpec. Site production uses YAML publication status;
HEAD-only Pure pages are excluded, and literal HEAD cannot override YAML.
Publication projection retains attribution, digest, determinism and its service
include-rejection policy, while single-star Markdown produces Em.

No new AST, renderer, CLI/site mode setting, remote-image, autolink, footnote,
strikethrough, task-list or content fallback contract is introduced. Validation,
review and acceptance remain parent-owned; this document records no completion.
