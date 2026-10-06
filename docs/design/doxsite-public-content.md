# DoxSite Public Content Design

Status: normative Phase 75.1 producer design
Date: 2026-10-06

The [public-content specification](../spec/doxsite-public-content.md) fixes the
behavior. The stable package-internal `DoxSiteEffectiveContent` object is the
single owner; no new trait, type framework, public product, schema or dependency
is introduced. Its existing `EffectiveContent` retains logicalPath, sourcePath
and the parsed page. The node overload suppresses a second physical-index
article; the tree/path overload first resolves the existing effective alias.
P751-01B moved public-path mapping unchanged from DoxSite into this owner.
P751-01C2 extends that sole owner to ordinary Markdown public suffixes.

## Markdown site integration

The Builder selects the accepted C1 MarkdownDocumentParser only for admitted
ordinary md/markdown sources with its existing source-parent configuration.
Package index and default Dox/org parsing remain. YAML supplies site metadata;
body HEAD/status/include/site/bib bytes receive Markdown interpretation only.

Public mapping changes only the final ordinary md/markdown suffix to html.
Tree/path HTML alias lookup retains the historical effective Dox/package
candidate, then tries the corresponding resolved normalized md and markdown
nodes in that order. Each node passes through the existing effective accessor,
so private or non-effective containers cannot reopen admission. Direct source
lookup and legacy effectivePath string overloads remain. This finite parsed-tree
selection avoids a filesystem scanner, reparse, fabricated Dox source and new
collision policy. Publication context verifies the actual owner-selected
Markdown source without implementation changes; LinkEnabler shares the same
metadata lookup and existing locale/title/fragment semantics.

The owner also resolves relation document identity from decoded URI paths.
It first normalizes the physical package base, then shares the legacy safe
base-path resolution helper. Query and fragment do not contribute to the
document key; an empty path refers to the base document. Parsed lookup supplies
the actual logical owner, with the existing effective-path fallback only when
no parsed target exists. This does not mutate an authored href or add another
suffix mapper, parse, scan or URL policy.

The collector configures a scanner and link builder with the actual parsed
tree through package-internal `forSite` factories. Private optional-tree
constructors and public zero-argument secondary constructors preserve existing
construction and named method labels. Unconfigured constructors retain their
string-only calculation. A package-internal tree-aware incoming `filterNot`
overload uses the same owner for reciprocal exclusion; the legacy public
overload is unchanged. LinkEnabler selects the tree-aware overload using its
selected site. Incoming-only candidates therefore share the actual target key
with complete parsed documents, and reciprocal filtering shares that identity.

## Responsibility

| Boundary | Decision |
| --- | --- |
| Effective-content owner | Classify document inputs under the outermost recognized project, select the parsed index and map physical/logical/public identity. |
| Builder Rule | Delegate document classification while preserving availability, suffix, metadata and resource handling. |
| DoxSite bibliography | Visit selected effective pages at container/content entries, extract entries with the physical node and page, and article references with logical source identity. Keep declared BibTeX supplementation. |
| LinkEnabler | Use effective tree/path metadata for both selected and unfiltered full trees. Preserve all glossary behavior. |
| Publication context | Use selected effective metadata and sourcePath, then apply existing physical target checks and publication/locale rules. |
| LinkCollector, LinkCollection | Configure relations with the parsed tree and use the sole owner for incoming document keys and reciprocal exclusion; preserve legacy public construction and overloads. |
| Antora | Retain existing owner consumption and logical article outputs. |

The ancestor search must reach the outermost recognized project before deciding
whether a document is its direct index. Selecting the nearest ancestor would
allow a nested valid project to reopen the outer package's private closure.
No filesystem scan or parse is needed for this classification. Direct metadata
and resources are not document suffixes and retain established processing.

The origin-root bibliography reference scanner and its source-parsing helpers
are retired. Reference extraction reuses admitted parsed ASTs after strategy
selection, so private bytes and Production-rejected drafts cannot bypass public
admission. The declared bibliography data scan remains solely for `.bib` entry
supplementation; curated precedence and existing extraction are unchanged.

Publication aliases resolve metadata through the shared owner, and verification
uses its physical sourcePath rather than checking a package directory as a
regular file. Existing segment resolution, raw target/fragment behavior,
normalization, root/symlink/regular-file/realpath defenses, HTTPS and exact-locale
failures remain intact. No new resolver fallback or security policy is added.

## Compatibility and evidence

This design supersedes only the source-reparse direction of the proposed
Phase 11 [source-root design](doxsite-source-root-semantics.md). Selected ASTs
keep their physical image-root/include context and virtual-root semantics.
Phase 13 [stable facades](parser-pdf-responsibility-decomposition.md) remain;
Rule delegates admission without changing its signature or parsed products.
P751-01A [physical include design](doxsite-physical-include-base.md) is preserved.
Markdown grammar/AST and suffix-route extension remained outside P751-01B;
the accepted C1 component and authored C2 integration supply those boundaries.

The implementation is in
[the owner](../../src/main/scala/org/smartdox/doxsite/DoxSiteEffectiveContent.scala),
[Builder](../../src/main/scala/org/smartdox/doxsite/DoxSiteBuilder.scala),
[DoxSite](../../src/main/scala/org/smartdox/doxsite/DoxSite.scala),
[LinkEnabler](../../src/main/scala/org/smartdox/doxsite/LinkEnabler.scala) and
[publication context](../../src/main/scala/org/smartdox/doxsite/SitePublicationContext.scala).
The bounded relation repair also touches
[LinkCollection](../../src/main/scala/org/smartdox/doxsite/LinkCollection.scala) and
[LinkCollector](../../src/main/scala/org/smartdox/doxsite/LinkCollector.scala).
The seven observable contracts are authored in
[DoxSitePublicContentSpec](../../src/test/scala/org/smartdox/doxsite/DoxSitePublicContentSpec.scala),
with existing [DoxSiteSpec](../../src/test/scala/org/smartdox/doxsite/DoxSiteSpec.scala)
and include/PDF accumulator coverage retained. Parent validation and independent
review determine acceptance; focused evidence below does not close the Step.

C1 cycle-4 current focused evidence passed 24/24; sealed independent M2
re-review accepted PASS with zero Current Boundary Blockers in
`phase75-1-review-ledger-20261006` revision 6. Earlier 17/17 representative and
171/171 regression results remain history; no 171-test rerun is claimed.
The compilation failure and equality abort remain recorded. Nine accepted
nonblocking Hygiene records, including horizontal-rule equality recursion,
are materialized in the Cozy Phase 75.1 journal; no equality repair is included.
C1 consumed Step repair cycle 4; global cumulative counts are now Step 6 /
Phase 0 after the C2 test-only cycle-5 correction and parent-verified consumption
of the incoming cycle-6 repair, with no reset.

[DoxSiteMarkdownSpec](../../src/test/scala/org/smartdox/doxsite/DoxSiteMarkdownSpec.scala)
authors ten C2 contracts for grammar/status, routes and projections, admission,
images, publication links/failures, precedence, incoming identity and reciprocal
exclusion. C2 focused validation passed before the cycle-6 repair
on `p751-01c2-validation-subject-20261006` revision 2: parent-verified
`p751-01c2-heading-repair-focused-20261006` passed 8/8 in one suite and
`p751-01c2-accumulator-20261006` passed 195/195 in nine suites. Both had zero
failures/aborts, `sbt_exit=0`, `wrapper_exit=0`, `lock=released`, and accepted
typed parent verification. Current195 includes repaired C1 24 and is distinct
from historical171 and predecessor accepted evidence.

Initial `p751-01c2-focused-20261006` compiled successfully but passed 7/8:
`TEST-P751-01C2-GENERATED-HEADING-001` failed its exact Section-title oracle
because the unchanged site appends an I18NFragment Bibliography section.
SBT/wrapper exited 1 and the lock was released. The parent-admitted M1 correction
changes only the oracle to exact ordered localized-title mapping; it adds no
product repair, filtering or expectation weakening. The subsequent sealed
complete-Step protected conformance review reports FINDINGS in ledger revision
7 for `CPB-P751-01C2-MARKDOWN-INCOMING-001`: string-only relation keys disagree
with parsed Markdown owners and include fragments. Cycle 6 authors the frozen
tree-aware repair, strengthens C2-06's incoming and JA/EN Related Articles
expectations, and adds C2-09's unique-source alias matrix and C2-10's reciprocal
exclusion with nonreciprocal observers for md, markdown, Dox and packages.
These use exact incoming identity/count expectations and localized Related
Articles section href/title expectations. The parent consumed cycle 6 once
from verified owned changes; cumulative Step 6 / Phase 0 / nonconverging 0
remain. Ledger revision 7 retains six reviews and zero full reviews.

Initial cycle-6 representative `p751-01c2-incoming-cycle6-focused-20261006`
compiled on subject revision 3 and passed 9/10, with the shared lock released.
`TEST-P751-01C2-CANONICAL-INCOMING-COUNT-001` failed C2-06's expected-2 versus
actual-1 incoming count. Parent-confirmed unchanged href grouping and final
recollection of enabled canonical links combine source/HTML aliases. One exact
oracle correction changed the count to 1, retaining exact source identity,
Related Articles size-2/href/title assertions and all C2-09/10 counts.

Parent-accepted typed closure `cycle6-focused-closure-p751-01c2-20261006`
revision 1 records `p751-01c2-incoming-cycle6-combined-20261006` on
`p751-01c2-validation-subject-20261006` revision 4: 205/205 in ten suites,
zero failures/aborts, `sbt_exit=0`, `wrapper_exit=0`, `lock=released`. This was
one actual command covering the ten C2 representatives and unchanged nine-suite
195-test accumulator, not full repository validation. The repair is focused
validated; independent M2 re-review remains pending and the blocker is not
independently closed. Earlier subject-revision-2 evidence remains historical.
Step, Phase and full-release acceptance stay OPEN; no commit, full-repository,
native-site/browser or deployment claim is made.
