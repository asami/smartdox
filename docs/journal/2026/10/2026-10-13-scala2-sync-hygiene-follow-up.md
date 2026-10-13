# Scala 2 repository synchronization hygiene — 2026-10-13

These nonblocking findings were recorded by the independent integration review.
Product behavior and historical acceptance records are preserved.

<!-- cncf-phase-finding:HYG-SDX-SCALA2-SYNC-DUPLICATE-COMMONMARK-001:begin -->
## HYG-SDX-SCALA2-SYNC-DUPLICATE-COMMONMARK-001

Status: OPEN. Priority: P3. Discovery: 2026-10-13 repository-sync review.

HYG-SDX-SCALA2-SYNC-DUPLICATE-COMMONMARK-001 — owner: smartdox. The Scala12 merge result repeats the identical commonmark and commonmark-ext-gfm-tables 0.30.0 declarations in build.sbt:65–73; each parent declares each once. This is nonblocking configuration maintenance because versions and scopes agree and settled-tree validation passed. Follow-up: retain one canonical declaration for each dependency without changing resolution semantics.

<!-- cncf-phase-finding:HYG-SDX-SCALA2-SYNC-DUPLICATE-COMMONMARK-001:end -->

<!-- cncf-phase-finding:HYG-SDX-SCALA2-SYNC-INHERITED-NAMING-001:begin -->
## HYG-SDX-SCALA2-SYNC-INHERITED-NAMING-001

Status: OPEN. Priority: P3. Discovery: 2026-10-13 repository-sync review.

HYG-SDX-SCALA2-SYNC-INHERITED-NAMING-001 — owner: smartdox. Whole-file review retains parent-authored naming debt in Dox2LatexConverter.scala (includeFile and other camelCase locals; private isHeader parameter) and LinkEnabler.scala (newLinks, beforeOK, afterPos, afterOK). Both-parent attribution shows no new merge naming regression. Follow-up is a separate behavior-preserving naming task under shared RULE.md; no API/schema changes or incidental cleanup are authorized by this review.

<!-- cncf-phase-finding:HYG-SDX-SCALA2-SYNC-INHERITED-NAMING-001:end -->

<!-- cncf-phase-finding:HYG-SDX-SCALA2-SYNC-INHERITED-SOURCE-SIZE-001:begin -->
## HYG-SDX-SCALA2-SYNC-INHERITED-SOURCE-SIZE-001

Status: OPEN. Priority: P3. Discovery: 2026-10-13 repository-sync review.

HYG-SDX-SCALA2-SYNC-INHERITED-SOURCE-SIZE-001 — owner: smartdox. Whole-file review measures DoxSite.scala at 2015 lines (local parent 2013; remote parent 1991), LinkEnabler.scala at 1280 and AntoraGenerator.scala at 1200. Existing pipeline, metadata/output, exported companion and link/generator responsibilities cross the shared source-size guidance. The merge introduces no new oversized file or demonstrated structural failure. Follow-up requires a separately planned, compatibility-preserving decomposition with consumer and initialization-order validation; it is not part of this merge repair.

<!-- cncf-phase-finding:HYG-SDX-SCALA2-SYNC-INHERITED-SOURCE-SIZE-001:end -->
