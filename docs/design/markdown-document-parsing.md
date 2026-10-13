# Markdown document conversion design

This decision implements the producer grammar boundary for SDX-75-03 in
[Cozy Phase 75.1](../../../cozy/docs/phase/phase-75.1.md). Observable semantics
are defined by [the specification](../spec/markdown-document-parsing.md) and
[MarkdownDocumentAstSpec](../../src/test/scala/org/smartdox/parser/MarkdownDocumentAstSpec.scala).

## Responsibilities

`private[smartdox] object MarkdownDocumentParser` owns front-matter splitting,
CommonMark parsing and existing metadata merging. Both entry points invoke the
same private interpretation; `parseC` wraps it in `Consequence.execute` and
plain `parse` preserves compatibility image exceptions.

`private[smartdox] object MarkdownDocumentAst` is a separate stable object. It
renders the actual parsed AST with CommonMark 0.30.0 and its GFM table extension,
then uses existing Jsoup 1.18.1 to parse only that rendered HTML fragment with a
body Element context. Direct constructors retain typed document, section,
inline, list and table semantics without invoking PureParser or Dox syntax.
Root heading levels determine nesting; renderer whitespace between blocks is
discarded while literal content, soft breaks and code whitespace survive.
YAML alone supplies title metadata through existing metadata merge. Without a
YAML title, headings remain body Sections without becoming metadata. No special
handling is attached to HEAD. This is the selected 2026-10-13 integration policy;
earlier dated fallback-title evidence below remains historical.

The `ParseContext` overload retains the existing Dox2Parser Pure Markdown
projection. Its Enhanced profile remains the Dox grammar path. The config and
`parseC` entry points retain the site AST conversion, including raw HTML origin
handling. Both preserve headings as body content and share Markdown image
admission. The existing parser and AST specification scenarios are retained in
separate suites.

## Raw HTML and image association

One HtmlNodeRendererFactory creates a CoreHtmlNodeRenderer subclass advertising
CommonMark Image, HtmlBlock and HtmlInline nodes. Each visit measures the local
Java StringBuilder before and after normal rendering. Image spans retain the
original Image node; raw HTML spans retain only their rendered offsets. A
position-tracking Jsoup HTML fragment parser supplies DOM element starts.
An element whose start lies within a raw span enters generic subtree
conversion. This uses renderer positions only, not source-location claims or
workflow provenance. The maps, spans and buffer live only for this conversion;
no user-visible marker is inserted.

Generic conversion retains the normal element name, every attribute, location
and children in order through existing Html5/Html5Inline constructors. It stays
generic recursively even when Jsoup inserts an implicit tbody without a tracked
start. That avoids typed quote/rule attribute loss, typed table compartment
filtering and typed Hyperlink URI/inline constraints on raw anchors. Raw
HtmlInline may contain only an opening tag; its DOM descendants still receive
generic conversion. Comments remain omitted and TextNode/DataNode contents stay
literal. Root h1-h6 handling runs before this dispatch, preserving accepted
Section levels and raw heading labels. Generated Markdown keeps its existing
typed conversion branches.

For every img, including one nested in a generic raw subtree, equality with a
recorded Image start associates that tag with its original AST destination.

Associated images call the single shared MarkdownImageAdmission with the
source-identified inline config, rendered plain alt and original destination.
Title attributes are retained on ReferenceImg. Raw HTML img tags at other
positions keep their attributes and become Html5Inline, including tags whose
attributes resemble provenance markers. This preserves raw HTML separately
from the established Markdown image-origin policy without duplicating it.

The admission helper moves mechanically to a stable package-private object in
MarkdownImageInlineParser.scala; legacy state products and diagnostics remain
unchanged. No sanitizer, image-fetch behavior or additional containment policy
is added. Raw script/style data is represented literally and never executed.

## Site integration decision

P751-01C2 switches only `DoxSiteBuilder._markdown_page` to
`MarkdownDocumentParser.parse`, using the unchanged
`_source_parser(node, Dox2Parser.Config.markdown)` context after public admission.
The existing Dox/org/default parsers, cache/source origins and Page products
remain. This bounded adapter consumes the accepted C1 grammar without migrating
global Dox2Parser or PDF source APIs.

The stable `DoxSiteEffectiveContent` owner extends ordinary public suffix mapping
to md/markdown and makes HTML lookup a finite ordered parsed-tree operation:
legacy Dox/package, md, markdown. Its node accessor alone determines whether a
candidate is effective and supplies its actual logical path, physical source
path and parsed page. The legacy string resolver remains unchanged; consumers
need no duplicate suffix mapper or filesystem fallback. Publication context and
LinkEnabler implementations keep their existing title, locale, fragment,
Published, HTTPS and physical-file diagnostics.

Eight semantic site scenarios in
[DoxSiteMarkdownSpec](../../src/test/scala/org/smartdox/doxsite/DoxSiteMarkdownSpec.scala)
cover grammar/status, normalized routes and Antora, private package closure,
physical/virtual images, real localized publication links, failures and ordered
collision precedence. Fixtures own only their unique directories below
`target/doxsite-markdown`. The old HEAD-as-metadata scenario is replaced with
Full strategy body-literal expectations; legacy YAML Production coverage stays.

## Retained acceptance evidence

C1's parser and specifications are authored. Before this raw HTML repair,
parent-selected focused validation passed 17/17 representative tests
(`p751-01c1-focused-20261006`) and 171/171 regression tests
(`p751-01c1-accumulator-20261006`). Independent review identified raw HTML
attribute/child loss and raw-anchor conversion failures; the frozen repair
adds seven semantic scenarios to the existing 17. Parent-verified focused
invocation `p751-01c1-raw-html-cycle4-observable-test-20261006` on subject
revision 6 passed 24/24 tests in one suite with no failures or aborts; SBT and
wrapper exited 0 and the shared lock was released. The earlier cycle-4 Test
compilation failure and subsequent equality abort remain recorded history,
distinct from the current successful run.

Scenario 19 compares the full ordered node projections of both successful
parse results: runtime class name, attributes, location and per-node rendering.
The direct raw-rule and typed-rule assertions remain. This comparison avoids
the pre-existing recursive `HorizontalRule.equals` predicate;
`HYG-P751-SDX-HORIZONTAL-RULE-EQUALITY-001` is accepted nonblocking Hygiene,
materialized with eight predecessor records in the Cozy Phase 75.1
[follow-up journal](../../../cozy/docs/journal/2026/10/2026-10-06-phase-75.1-hygiene-follow-up.md).
Sealed independent M2 repair re-review accepts C1 PASS with zero Current
Boundary Blockers in review ledger `phase75-1-review-ledger-20261006` revision 6.
Current representative evidence is 24/24; the 171/171 accumulator is retained
historical evidence, not a cycle-4 rerun. C1 consumed Step repair cycle 4;
global cumulative counts are now Step 5 / Phase 0, without reset.

C2's site integration is authored and focused validation passed on
`p751-01c2-validation-subject-20261006` revision 2. Parent-verified
`p751-01c2-heading-repair-focused-20261006` passed 8/8 tests in one suite and
`p751-01c2-accumulator-20261006` passed 195/195 in nine suites. Both had zero
failures/aborts, `sbt_exit=0`, `wrapper_exit=0`, `lock=released`, and accepted
typed parent verification. Current195 includes repaired C1 24 and is distinct
from historical171.

The retained initial `p751-01c2-focused-20261006` compiled successfully but
passed 7/8 with `TEST-P751-01C2-GENERATED-HEADING-001`: the exact Section-title
oracle omitted the unchanged site's appended I18NFragment Bibliography section.
SBT/wrapper exited 1 and the lock was released. The parent-admitted M1 test-only
correction uses exact ordered localized-title mapping; no product repair,
filtering or expectation weakening is included. Independent complete-Step
protected conformance review remains pending. Legacy
Dox2Parser Markdown/PDF/publication signatures remain unchanged. SDX-75-03,
Step, Phase and full-release acceptance remain OPEN until actual acceptance.
