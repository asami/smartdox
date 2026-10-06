# DoxSite physical include base

status=active
since=2026-10-06
owner=SmartDox

## Contract

DoxSite selects a parser from the admitted source filename before assigning its
resource origin. For a physical site, the parser receives the canonical source
file as an absolute `File.toURI` URI and its canonical parent as the resource
root. An authored relative include such as `include::_fragment.dox[]` resolves
against that physical source, using the existing include resolver contract.

A Document Project has three distinct identities:

| Role | Example |
| --- | --- |
| Physical parser source | `category/article.dox/index.dox` |
| Logical effective document | `category/article.dox` |
| Public page | `category/article.html` |

The physical index remains the include base. Existing effective-content and
public-route ownership determine the logical and public identities; the parser
adapter does not reconstruct that mapping. Ordinary `category/ordinary.dox`
sources retain the same relative include behavior.

For an origin-less Realm, filename-based relative source identity and virtual
resource-parent selection remain in effect. Physical URI encoding must support
spaces, Japanese characters and literal percent, plus and hash characters
without relying on the process working directory.

`Dox2Parser.Config.fileTextResolverContext` constructs the physical include base
directly for native local file URIs: `uri.getScheme == "file" && !uri.isOpaque &&
uri.getAuthority == null && uri.getQuery == null && uri.getFragment == null`.
It obtains the parent with `new File(uri).getParentFile` and prepends that
`FileResourceLocator` to the default resolver resources. Copying only the file
resolver context preserves fallback resources, parameters and the text context.
An absent file retains the default context; all other URI forms retain the
existing `withBaseFile` behavior, including relative sources and non-file URLs.

## Scope and compatibility

The final parser configuration is passed directly to `Dox2Parser.parse`.
Calling `parseWithFilename` after assigning the physical URI would apply
`withPathname` and relative normalization again, losing the absolute base.
The bounded native physical context construction above is the only parser
change. Resolver APIs, global URI normalization, public-source admission,
package inventory, cache selection and modification-time behavior are preserved.
Markdown grammar, HEAD metadata, language filtering and suffix-directed style
are unchanged by this slice.

## Executable specification and ownership

[DoxSitePhysicalIncludeSpec](../../src/test/scala/org/smartdox/doxsite/DoxSitePhysicalIncludeSpec.scala)
defines four real DoxSite scenarios: physical Document Project inclusion and
canonical routing, ordinary physical inclusion, an encoded physical root, and
origin-less rendering. Each uses a fresh Context and Full strategy; physical
fixtures and work paths live under `target/` and cleanup owns only its fixture.

[Dox2ParserPhysicalIncludeSpec](../../src/test/scala/org/smartdox/parser/Dox2ParserPhysicalIncludeSpec.scala)
defines direct parser compatibility for a native physical source under spaces
and Japanese characters, literal `%25+hash#` characters, a cwd-relative ASCII
source, and existing non-fetching HTTPS host-root candidate projection:
`https://example.invalid/docs/source.dox` plus `_fragment.dox` projects
`https://example.invalid/_fragment.dox` without a fetch. The physical cases
parse real authored includes and assert their sibling sentinels. Original
DoxSite scenarios and assertions remain unchanged.

The adapter is [DoxSiteBuilder](../../src/main/scala/org/smartdox/doxsite/DoxSiteBuilder.scala).
The existing mapping owner is
[DoxSiteEffectiveContent](../../src/main/scala/org/smartdox/doxsite/DoxSiteEffectiveContent.scala).
See the [design](../design/doxsite-physical-include-base.md) and
[Cozy Phase 75.1](../../../cozy/docs/phase/phase-75.1.md), Step P751-01,
Slice P751-01A (SDX-75-04). Focused validation passed: 8 of 8 scenarios in
2 suites (`p751-01a-corrected-focused-20261006`) and the separate existing
accumulator passed 50 of 50 scenarios in 2 suites
(`p751-01a-accumulator-20261006`); both recorded `sbt_exit=0`, `wrapper_exit=0`
and `lock=released`. Independent review remains pending; these executions
constitute neither independent review nor Step/Phase acceptance, which remain
OPEN.
