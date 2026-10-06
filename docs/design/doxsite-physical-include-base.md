# DoxSite physical include base adapter

status=active
since=2026-10-06
owner=SmartDox

## Decision

Keep origin handling at the existing private
[DoxSiteBuilder](../../src/main/scala/org/smartdox/doxsite/DoxSiteBuilder.scala)
parser boundary. `_source_parser` returns a final `Dox2Parser.Config` rather than
a filename/configuration pair.

First apply `baseconfig.withFilename(node.pathname)` to retain the admitted
suffix's parser style and filename identity. If the site has an origin,
canonicalize `new File(origin, node.pathnameRelative)`, retain its parent with
`withResourceRoot`, and set `file` to `Some(canonical.toURI)`. `File.toURI`
preserves an absolute, encoded physical base, including spaces and Japanese
characters. If the site has no origin, retain the filename-based relative
identity and apply the existing virtual resource-parent helper.

At `Dox2Parser.Config.fileTextResolverContext`, construct a native local file
base directly when `uri.getScheme == "file" && !uri.isOpaque &&
uri.getAuthority == null && uri.getQuery == null && uri.getFragment == null`.
Use `new File(uri).getParentFile`, then prepend `FileResourceLocator(parent)`
to the default file resolver resources. Copy only `fileResolverContext`, keeping
its existing resources and parameters and the surrounding text context. An
absent file keeps the default; non-native URI forms keep `withBaseFile`.

The selected Goldenport `UriUtils.getFile` converts through `URI.toURL` and
`URL.getFile` into `File(String)`, leaving encoded path characters literal.
Native `File(URI)` supplies the required single decoding for the physical
sibling include, including literal percent, plus and hash characters. The
selector restricts this correction to the Java `File.toURI` form; it introduces
no validation or rejection policy for authority, opaque, query or fragment
forms. Manual percent decoding and `URLDecoder` are outside this decision.

Both Dox and Markdown callers pass that final configuration directly to
`Dox2Parser.parse`. They must not call `parseWithFilename` afterward: its
filename assignment applies `withPathname`/relative normalization and would
erase the physical base already chosen by the adapter.

## Responsibility boundary

For `category/article.dox/index.dox`, include resolution uses the physical
index. The existing
[effective-content owner](../../src/main/scala/org/smartdox/doxsite/DoxSiteEffectiveContent.scala)
still exposes logical `category/article.dox`, and existing site projection
emits public `category/article.html`. The authored `_fragment.dox` target stays
relative. Only native physical context construction changes; resolver APIs,
global path normalization and package mapping retain their behavior. Ordinary
physical files use the same adapter. Virtual Realm sources retain their
separate resource origin.

Preserve parser/resolver APIs, cache lookup and `lastModified`, metadata and
language filtering, source admission and package inventory. This decision does
not repair Markdown grammar or add configuration, dependencies or public API.

## Evidence boundary

The [specification](../spec/doxsite-physical-include-base.md) is exercised by
[DoxSitePhysicalIncludeSpec](../../src/test/scala/org/smartdox/doxsite/DoxSitePhysicalIncludeSpec.scala)
through real parsed effective content and rendered HTML, not source-shape
checks. Physical fixtures are absolute paths under repository `target/`;
the encoded-root scenario never changes the process working directory.
[Dox2ParserPhysicalIncludeSpec](../../src/test/scala/org/smartdox/parser/Dox2ParserPhysicalIncludeSpec.scala)
adds real native includes under spaces/Japanese and literal `%25+hash#` roots,
a real cwd-relative ASCII include, and existing non-fetching HTTPS host-root
candidate projection: `https://example.invalid/docs/source.dox` plus
`_fragment.dox` projects `https://example.invalid/_fragment.dox` without a fetch.
Its fixtures are owned directories under
`target/dox2-physical-include`; original DoxSite assertions are retained.
[Cozy Phase 75.1](../../../cozy/docs/phase/phase-75.1.md) owns validation,
independent review and acceptance for P751-01A. Focused validation passed:
8 of 8 scenarios in 2 suites (`p751-01a-corrected-focused-20261006`) and the
separate existing accumulator passed 50 of 50 scenarios in 2 suites
(`p751-01a-accumulator-20261006`); both recorded `sbt_exit=0`, `wrapper_exit=0`
and `lock=released`. Independent review remains pending; these executions
constitute neither independent review nor Step/Phase acceptance, which remain
OPEN.
