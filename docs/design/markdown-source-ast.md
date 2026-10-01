# Markdown source AST design

Status: frozen Phase75 / Step75.1 / Slice75.1B2 design
Date: 2026-10-01

The [source contract](../spec/markdown-source-ast.md) fixes this user-authorized
parser boundary. A package-private MarkdownDocumentParser accepts ParseContext
and Markdown body bytes. CommonMark Java 0.30.0 plus its GFM tables extension,
with BLOCKS_AND_INLINES source spans, parses once. A native adapter assembles
Sections in each block container and projects typed nodes. Program's generated
apply preserves literal contents and empty callouts; Program.create is excluded.
No adapter path calls DoxInlineParser, DoxLinesParser, LogicalBlocks, document
assembly, HEAD processing or include resolution.

Dox2Parser.Config retains its original six constructor, product, apply, unapply
and copy parameters: isDebug, isLocation, blocksConfig, linesConfig, file and
style. Markdown mode is private execution metadata initialized to Pure, exposed
through markdownMode and preserved by the explicit six-argument copy.
withMarkdownMode copies first and sets the mode only on that fresh unpublished
result, preserving the original configuration and shared presets. Like existing
inline resource context, mode is outside case-class product, equality and
hashCode; whole Config equality is not a parser-result cache key. The parent
found no production Config-keyed cache or equality consumer in SmartDox/Cozy
source. Reconstruction through the original constructor starts Pure.
pureMarkdown and markdown remain Pure; enhancedMarkdown is obtained through
withMarkdownMode(Enhanced). Filename, style, inline, paragraph-option and
resource-origin copies preserve mode, including temporary SmartDox/Org styles.
String dispatch selects Pure only for Markdown style plus Pure mode. Enhanced
and other styles retain LogicalBlocks and existing assembly/resolver behavior;
structured block input retains that route regardless of mode.

Static front-matter splitting/merging stays at its current boundary, outside
the adapter. Direct class APIs remain body-oriented. Pure parse2 delegates to
static parse; legacy parse2 paths remain unchanged. Source spans are relative
to the split body, preserving existing metadata API scope.

MarkdownImageInlineParser gains only an admission bridge: parsed alt, normalized
destination, original span source and node location enter existing admission,
then optional title decorates ReferenceImg. The raw-inline state machine is
unchanged. Resource origin remains explicit, and admission never reads files.

JSoup 1.18.1 handles only CommonMark HtmlBlock fragments and rendered sibling
sequences containing HtmlInline. DOM text/elements become native ordinary HTML,
formatting, links and images without Dox tag dispatch. Normal Markdown inlines
use direct AST mapping. Comments are omitted; no sanitizer or fetcher is added.

Producer source admission, package descendants, Phase71 lifecycle, public AST,
renderers and Cozy product adapters stay outside this boundary. Cozy's two
normative Phase documents record Pure/Enhanced direction without status changes.
Producer validation/review and the planned unchanged-SNAPSHOT dependency refresh
are parent responsibilities; historical receipts do not validate this tree.
