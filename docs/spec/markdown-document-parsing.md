# Markdown document parsing

This contract addresses SDX-75-03 in [Cozy Phase 75.1](../../../cozy/docs/phase/phase-75.1.md).
The paired executable specification is
[MarkdownDocumentParserSpec](../../src/test/scala/org/smartdox/parser/MarkdownDocumentParserSpec.scala);
the conversion decisions are recorded in [the design](../design/markdown-document-parsing.md).

## Input and metadata

The package-internal `MarkdownDocumentParser.parseC(config, source)` returns a
`Consequence[Document]`. Its plain `parse(config, source)` entry uses the same
interpretation and retains existing `IllegalArgumentException` image diagnostics.
This component does not replace the existing Dox2Parser Markdown APIs.

Configured Markdown YAML front matter is split and merged by the existing
`Dox2ParserFrontMatter`. Only the remaining body enters CommonMark with GFM
tables. YAML title is authoritative; without it, the first root-level heading's
converted inline contents provide the title. The heading stays in the body.
YAML status and bibliography properties remain metadata. Body `HEAD`, status,
include, site-link and bibliography syntax remains ordinary Markdown content;
it has no SmartDox metadata, macro, include or automatic i18n interpretation.

## Shared document semantics

Actual CommonMark AST output is rendered and its HTML fragment is converted
directly to existing shared Dox constructors. Original Markdown bytes never
enter a Dox body parser or an HTML body parser.

Root-level ATX and Setext headings form ordered Sections. A heading closes
preceding headings of the same or higher level and nests under the preceding
lower level. Section levels preserve actual heading levels, title inlines and
labels/attributes; skipped levels do not create synthetic Sections. A preamble
remains before the first Section. Headings inside a quotation or raw container
remain elements inside that container.

The following typed mappings apply to renderer-generated Markdown constructs.
Raw-origin elements follow the preservation contract below instead.

| Rendered Markdown element | Shared representation |
| --- | --- |
| p, strong, em, code | Paragraph, Strong, Em, Code |
| ul, ol, li | Ul, Ol, Li with nesting and ordered start attributes |
| blockquote | Quotation.SimpleQuote |
| table, thead, tbody, tr, th, td | Table, THead, TBody, TR, TH, TD with alignment attributes |
| a | Hyperlink with literal converted inlines, destination and title |
| pre | Html5 pre containing Code with literal text and language class |
| br, hr | Html5Inline br, HorizontalRule |
| other elements | Html5 blocks or Html5Inline inlines retaining attributes and children |
| text | Text with whole decoded text, including inline soft breaks |

Code preserves spaces, newlines and literal directives, including `// <1>`;
it does not enter Program callout extraction. Renderer separator whitespace
outside block contents is ignored; inline text and code are not trimmed.
Raw HTML preserves DOM element names, all attributes and children in DOM order
using existing Html5 blocks and Html5Inline inlines. This applies recursively
to the whole raw subtree, including parser-implied elements such as tbody.
Raw blockquote classes, hr identifiers, table captions/footers and their
compartment/cell attributes survive. Raw anchors retain literal hrefs,
including spaces, and flow-content children such as paragraphs. They do not
acquire the typed Hyperlink's URI or inline-only constraints. Root raw h1-h6
still form Sections and retain their labels/attributes as described above.
Comments are omitted and script/style data is retained literally without
execution. No sanitizer or additional URL policy is introduced.

## Markdown and raw HTML images

Markdown images, including resolved reference images, use the existing shared
`MarkdownImageAdmission` with the configured physical or virtual resource
origin and location. Destinations come from actual CommonMark Image nodes;
alt/title come from rendered image attributes. Relative normalization,
containment, supported suffixes and diagnostic categories remain unchanged.
Traversal, absolute/external resources, absent origins and query/fragment
destinations retain their existing rejection behavior. Plain parse throws;
parseC captures the failure in Consequence.

The renderer records actual CommonMark Image and raw HtmlBlock/HtmlInline
start/end offsets in one parse-local rendered buffer. Jsoup tracks positions
in that rendered fragment. An element starting within a raw span enters
generic subtree conversion; an img is Markdown-origin only when its start
equals a recorded image start, including inside a raw inline wrapper.
Other img tags remain Html5Inline with their raw attributes. User attributes,
including marker-looking attributes, do not control this distinction. No
attributes, IDs, nonce, digest or source-copy markers are injected. These
temporary HTML offsets are not original Markdown source locations and are
neither persisted nor compared between workflow stages.

## Admitted site sources and public aliases

P751-01C2 connects ordinary admitted `.md` and `.markdown` sources to this
parser through `DoxSiteBuilder._markdown_page`. Admission precedes grammar
selection. The Builder retains its existing physical/virtual source-parent
configuration; package articles remain direct `index.dox` inputs to the Dox
parser. Body HEAD/status/include/site/bibliography text is literal Markdown;
YAML alone supplies metadata for site strategy. Existing site localization
continues to project localized YAML titles through the established pipeline.

`DoxSiteEffectiveContent` alone owns suffix mapping and parsed-tree aliases.
Ordinary Markdown retains its actual logical/source suffix and publishes as
same-stem `.html`. HTML lookup first tries the historical Dox/package candidate,
then the same resolved normalized stem's `.md`, then `.markdown`. Each candidate
must be an effective page already in the parsed tree. Direct source lookup,
package aliases, leading-slash/backslash normalization and other suffixes retain
their contracts. There is no filesystem scan, source reparse, fabricated Dox
source or new collision rejection rule. The existing string `effectivePath`
overloads keep their legacy behavior.

HTML/Antora, metadata, bibliography and site/publication links consume the same
effective owner. Publication verification uses the selected actual Markdown
regular file and preserves Published filtering, HTTPS, exact JA/EN titles,
relative-link fragments, containment and symlink diagnostics. The paired site
specification is [DoxSiteMarkdownSpec](../../src/test/scala/org/smartdox/doxsite/DoxSiteMarkdownSpec.scala),
P751-01C2-01–08. The modified DoxSiteSpec HEAD scenario retains literal body
status under Full strategy; its separate YAML Production coverage remains.

## Delivery evidence

P751-01C1 authors this parser, shared conversion, image-policy extraction and
paired documentation/specification only. Pre-repair focused validation passed
17/17 representative tests (`p751-01c1-focused-20261006`) and 171/171 regression
tests (`p751-01c1-accumulator-20261006`). Independent review identified raw HTML
attribute/child loss and raw-anchor conversion failures. The bounded repair
adds scenarios P751-01C1-18–24 for raw quotes, rules, complete and implied table
subtrees, literal/flow-content anchors and Markdown images inside raw wrappers.
Parent-verified focused invocation
`p751-01c1-raw-html-cycle4-observable-test-20261006` on subject revision 6
passed 24/24 tests in one suite with no failures or aborts; SBT and wrapper
exited 0 and the shared lock was released. The earlier cycle-4 Test compilation
failure and subsequent equality abort remain recorded history, distinct from
this successful run.

Scenario 19 compares both successful parse results through the full ordered
node projection of runtime class name, attributes, location and per-node
rendering. This avoids the pre-existing recursive `HorizontalRule.equals`
predicate while retaining the direct raw-rule and typed-rule assertions.
`HYG-P751-SDX-HORIZONTAL-RULE-EQUALITY-001` is accepted nonblocking Hygiene,
materialized with the other eight retained records in the Cozy Phase 75.1
[follow-up journal](../../../cozy/docs/journal/2026/10/2026-10-06-phase-75.1-hygiene-follow-up.md).
The sealed independent M2 repair re-review accepts C1 PASS with zero Current
Boundary Blockers in review ledger `phase75-1-review-ledger-20261006` revision 6.
The current C1 representative evidence is 24/24; the earlier 171/171 result is
retained historical regression evidence and was not rerun for cycle 4.
C1 consumed cumulative Step repair cycle 4; the global count is now Step 5 /
Phase 0 after the bounded C2 test-only correction, with no reset.

C2 site integration is authored and focused validation passed on
`p751-01c2-validation-subject-20261006` revision 2. Parent-verified invocation
`p751-01c2-heading-repair-focused-20261006` passed 8/8 tests in one suite;
`p751-01c2-accumulator-20261006` passed 195/195 tests in nine suites. Both had
zero failures/aborts, `sbt_exit=0`, `wrapper_exit=0`, `lock=released`, and accepted
typed parent verification. Current195 includes the repaired C1 24 scenarios and
is distinct from historical171.

Initial `p751-01c2-focused-20261006` compiled successfully but passed 7/8:
`TEST-P751-01C2-GENERATED-HEADING-001` failed its exact Section-title oracle
because the unchanged site appends an I18NFragment Bibliography section.
Its SBT/wrapper exits were 1 and the lock was released. The parent-admitted
M1 correction changes only the test oracle to exact ordered localized-title
mapping, without product repair, filtering or weakened expectations.
Independent complete-Step protected conformance review remains pending.
Step, Phase and full-release acceptance remain OPEN. C1 itself did
not change site selection, legacy Markdown specifications or PDF/publication
entry points; C2 changes only the admitted site parser and shared route owner.
