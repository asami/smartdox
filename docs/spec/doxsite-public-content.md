# DoxSite Public Content

Status: normative Phase 75.1 producer contract
Date: 2026-10-06

## Admission and identity

[DoxSiteEffectiveContent](../../src/main/scala/org/smartdox/doxsite/DoxSiteEffectiveContent.scala)
owns document admission and the effective physical, logical and public view.
A Document Project is a container named `<slug>.dox` with content in its
direct `index.dox` child. Among recognized ancestor projects, the outermost
project owns admission: only its direct index is public document input.
An arbitrary descendant with `.dox`, `.org`, `.md` or `.markdown` suffix is
private, including a valid nested Document Project. Its contents must not be
parsed to discover public pages, links, Antora inputs, metadata or references.
Private names are not a denylist. A `.dox` directory without a direct index
is an ordinary directory, and its ordinary descendants remain eligible.
Existing availability and path-pattern rules remain applicable. Direct package
metadata and static resources retain their established handling.

An ordinary article retains its source path. A selected package article has
physical source `<directory>/<slug>.dox/index.dox`, logical source
`<directory>/<slug>.dox`, and public page `<directory>/<slug>.html`. Its physical
index remains addressable in the parsed tree but is not a second effective
article. Tree/path lookup accepts the logical source, physical index and
`index.html` alias through the shared effective-path resolver. Public-path
mapping preserves leading slash and backslash normalization and the established
`.dox`/package-index flattening. P751-01B introduced no Markdown route semantics;
P751-01C2 extends this same owner as specified below.

## Admitted Markdown suffixes and aliases

Ordinary admitted `.md` and `.markdown` pages enter the accepted
[Markdown document parser](markdown-document-parsing.md) after admission, using
the existing Builder physical/virtual source context. Their logical and physical
source paths retain the actual Markdown suffix; their canonical public path is
same-stem `.html`. Package `index.dox` remains Dox input, and arbitrary package
descendants remain private regardless of Markdown syntax validity.

For requested HTML aliases, tree/path lookup tries the historical effective
Dox/package candidate first, then corresponding same resolved normalized stem
`.md`, then `.markdown`. Only parsed effective pages can supply an owner. Direct
source lookup, legacy string `effectivePath` overloads, package aliases and other
suffix behavior remain. Leading-slash/backslash normalization is retained. No
new collision rejection, origin-root scan, filesystem reparse or fabricated
`.dox` target is permitted.

YAML controls Markdown status and other metadata. Body HEAD/status/include/site
and bibliography syntax stays literal while actual Markdown constructs retain
their shared AST meaning. LinkEnabler and SitePublicationContext use the shared
lookup unchanged; actual Markdown regular files supply publication verification.
Published filtering, exact JA/EN titles, HTTPS routes, relative-link fragments,
root containment, symlink/realpath checks and diagnostics remain established
contracts.

## Parsed content and consumers

[DoxSiteBuilder.Rule](../../src/main/scala/org/smartdox/doxsite/DoxSiteBuilder.scala)
delegates admission to the owner before document parsing. LinkCollector,
LinkCollection and Antora continue to consume selected effective articles.
[DoxSite](../../src/main/scala/org/smartdox/doxsite/DoxSite.scala) delegates public
mapping and collects bibliography article references from admitted parsed
effective pages, using logical source paths. Metadata and inline `bib:[key]`
references are retained once; private and Production-rejected draft references
do not enter the bibliography. Curated entries and declared `bibliography/*.bib`
supplementation retain their established precedence. No origin-root document
walk or second source parse may supply article references.

[LinkEnabler](../../src/main/scala/org/smartdox/doxsite/LinkEnabler.scala) uses
effective tree/path metadata lookup for real `site:[...]` links. Its full-tree
fallback remains unfiltered. Localized titles and canonical relative HTML links
retain logical package identity, including reciprocal LinkCollection relations.

Incoming relations use the same parsed effective-content owner as public alias
lookup. Document identity uses the decoded URI path, resolves it from the
effective logical source base, and ignores query and fragment; an empty path
denotes the base document. Physical package-index bases normalize to the logical
package. A parsed target contributes its actual logical path, including `.md`
or `.markdown`; an absent target retains the legacy effective-path fallback.
Authored hrefs are unchanged, and rendered hrefs retain the established link
policy. Related Articles excludes reciprocal sources using that same tree-aware
identity, while retaining nonreciprocal incoming sources. No scan, reparse,
new URL validation, collision or missing-target policy is introduced.
Public zero-argument `LinkCollection.Builder` and `LinkCollector.SiteScanner`
construction and the public string-only `filterNot` retain legacy behavior.

[SitePublicationContext](../../src/main/scala/org/smartdox/doxsite/SitePublicationContext.scala)
uses the same selected metadata and Published predicate. Target verification
checks the owner-selected physical index, retaining existing normalization,
root containment, leaf-symlink, regular-file and realpath checks. Source-parent
interpretation also delegates to effective identity. Selected publication
configuration, HTTPS, exact JA/EN titles, raw references/fragments, structured
failures and public case-class signatures remain unchanged. Missing, private,
unpublished, symlink and outside-root targets retain existing diagnostics.

## Prior contracts and remaining work

This contract supersedes the bibliography-source-reparse direction in the
proposed Phase 11 [source-root specification](doxsite-source-root-semantics.md)
and [design](../design/doxsite-source-root-semantics.md). Physical image roots,
virtual roots, cache/source-origin and include semantics remain unchanged:
bibliography reuses the AST already parsed with its selected document context.
Phase 13 [facade compatibility](parser-pdf-decomposition-compatibility.md) and
[responsibility design](../design/parser-pdf-responsibility-decomposition.md)
remain in force; admission now delegates from Rule to the shared owner.
The accepted [physical include contract](doxsite-physical-include-base.md) is
preserved. P751-01C1's real Markdown parser is independently accepted PASS:
current cycle-4 focused evidence is 24/24 and sealed M2 re-review is recorded
in `phase75-1-review-ledger-20261006` revision 6. Earlier 17/17 and 171/171 runs
remain historical evidence; the 171 regression tests were not rerun in cycle 4.
The compilation failure and equality abort remain history. Nine nonblocking
Hygiene records, including `HYG-P751-SDX-HORIZONTAL-RULE-EQUALITY-001`, are
materialized in the Cozy Phase 75.1 journal. C1 consumed Step repair cycle 4;
global cumulative counts are now Step 6 / Phase 0 after the C2 test-only
cycle-5 correction and parent-verified consumption of the incoming cycle-6
repair. The sealed complete-Step protected conformance review reports
FINDINGS in ledger revision 7: `CPB-P751-01C2-MARKDOWN-INCOMING-001` identifies
string-only incoming and reciprocal keys that lose parsed Markdown owners and
include fragments in document identity. The frozen cycle-6 repair is focused
validated on subject revision 4; independent M2 re-review remains pending.
The parent consumed cycle 6 once from verified owned changes, with Step 6 /
Phase 0 / nonconverging 0. Review ledger revision 7 retains six reviews and
zero full reviews. Earlier focused results apply to their recorded subject.
SDX-75-03, Step and Phase acceptance remain OPEN.

## Executable specification

[DoxSitePublicContentSpec](../../src/test/scala/org/smartdox/doxsite/DoxSitePublicContentSpec.scala)
defines P751-01B-01–07: ordinary/package identity and resources; generic private
closure and nested projects; selected bibliography references; reciprocal HTML
links; publication aliases; structured publication failures; and ordinary `.dox`
directories with following Antora siblings. Fixtures and cleanup are scoped to
`target/doxsite-public-content`, with fresh contexts and no renderer/network.
[DoxSiteSpec](../../src/test/scala/org/smartdox/doxsite/DoxSiteSpec.scala) retains
the curated/BibTeX, image-root and fault-tolerance accumulator;
[DoxSitePhysicalIncludeSpec](../../src/test/scala/org/smartdox/doxsite/DoxSitePhysicalIncludeSpec.scala)
and [PdfOperationClassSpec](../../src/test/scala/org/smartdox/service/operations/PdfOperationClassSpec.scala)
retain include and publication compatibility evidence. Authored scenarios do
not themselves claim validation, Step acceptance or Phase closure.

[DoxSiteMarkdownSpec](../../src/test/scala/org/smartdox/doxsite/DoxSiteMarkdownSpec.scala)
adds P751-01C2-01–10 for literal grammar/YAML, Production selection, nested
routes/metadata/Antora, private closure/ordinary siblings, physical/virtual
images, real site/publication aliases, failures and parsed collision precedence.
Its fixtures and cleanup are scoped to `target/doxsite-markdown`. On
`p751-01c2-validation-subject-20261006` revision 2, parent-verified
`p751-01c2-heading-repair-focused-20261006` passed 8/8 in one suite and
`p751-01c2-accumulator-20261006` passed 195/195 in nine suites. Both had zero
failures/aborts, `sbt_exit=0`, `wrapper_exit=0`, `lock=released`, and accepted
typed verification. Current195 includes repaired C1 24 and remains distinct
from historical171 and accepted A/B/C1 predecessor evidence.

Initial `p751-01c2-focused-20261006` compiled successfully but passed 7/8,
with `TEST-P751-01C2-GENERATED-HEADING-001` failing its exact Section-title
oracle because the unchanged site appends an I18NFragment Bibliography section.
SBT/wrapper exited 1 and the lock was released. The parent-admitted M1 test-only
correction uses exact ordered localized-title mapping, without product repair,
filtering or weakened expectations. The subsequent sealed review reports the
incoming blocker above. Cycle 6 strengthens C2-06 with actual incoming source
identities and JA/EN Related Articles, adds C2-09's source/HTML alias matrix with
fragments and package physical aliases, and adds C2-10's reciprocal exclusion
with retained observers for Markdown, Dox and package targets. Each observes
exact incoming identities/counts and localized Related Articles section hrefs
and titles.

Initial cycle-6 representative `p751-01c2-incoming-cycle6-focused-20261006`
compiled on subject revision 3 and passed 9/10, with the shared lock released.
`TEST-P751-01C2-CANONICAL-INCOMING-COUNT-001` failed only C2-06's incoming
count: expected 2, actual 1. The unchanged href grouping and final collection
of enabled canonical links merge source/HTML aliases into one incoming record.
The parent froze one exact count correction to 1; exact source identity and
Related Articles size-2/href/title expectations remained, as did C2-09/10.

Parent-accepted typed closure `cycle6-focused-closure-p751-01c2-20261006`
revision 1 records `p751-01c2-incoming-cycle6-combined-20261006` on
`p751-01c2-validation-subject-20261006` revision 4: 205/205 tests in ten suites,
zero failures/aborts, `sbt_exit=0`, `wrapper_exit=0`, `lock=released`. One actual
command covered the ten C2 representatives and unchanged nine-suite 195-test
accumulator; it was not full repository validation. The repair is focused
validated, not independently closed. Independent M2 re-review remains pending;
Step/Phase and all outcome checklists stay OPEN. No commit, full-repository,
native-site/browser or deployment result is claimed.
