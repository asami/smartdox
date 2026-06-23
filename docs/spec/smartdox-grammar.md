# SmartDox Grammar

Status: draft specification
Date: 2026-04-30

This document describes the SmartDox grammar implemented by the current parser
and covered by the parser tests. It is normative for stable syntax. Syntax that
is still under design is tracked in `docs/journal`.

## Scope

SmartDox is a document grammar that combines:

- Org-style section and annotation syntax
- Markdown-style headings, links, and lists
- SmartDox-specific site links, includes, metadata, and literate blocks

The parser produces a Dox document tree containing document head metadata,
sections, paragraphs, lists, tables, figures, programs, links, and inline
markup.

Descriptive metadata effective semantics are defined in `docs/design/descriptive-effective-semantics.md`.

## Document

A document is a sequence of block elements.

```ebnf
document     ::= block*
block        ::= section
               | paragraph
               | list
               | table
               | image
               | figure
               | program
               | annotation
               | block-macro
               | horizontal-rule
               | quotation
               | comment
```

Blank lines separate paragraphs. Consecutive non-special text lines are
normalized into paragraph text.

## Sections

SmartDox accepts both Org-style and Markdown-style section headings.

```ebnf
org-section      ::= "*"+" " inline-text line-end block*
markdown-section ::= "#"+ " " inline-text line-end block*
setext-section   ::= inline-text line-end "="+" line-end block*
```

Examples:

```dox
* First
** Second
```

```markdown
# First
## Second
```

```markdown
First
=====
```

The visible section title is parsed as inline content.

### Section Title Termination

A blank line terminates the section title. This rule is intentional: SmartDox
allows a section title to span multiple adjacent non-blank lines. Therefore, when
a section title is meant to be one line, the heading line must be followed by a
blank line before body content or child sections.

Correct one-line title:

```dox
# Overview

This is body text.
```

Correct multi-line title:

```dox
# Long section title
continued title line

This is body text.
```

Incorrect when the second line is intended as body content:

```dox
# Overview
This is body text.
```

Reserved metadata section names such as `HEAD`, `HEADLINE`, `BRIEF`, `SUMMARY`,
and `DESCRIPTION` are single-line names. Their metadata body must start after a
blank line.

## Metadata Head

A top-level section named `HEAD` is treated as document metadata.

```dox
# HEAD

status=work-in-progress
published_at=2026-04-06
```

Metadata text inside `HEAD` is parsed as HOCON-style properties and merged into
the document head. This rule applies in both SmartDox authoring and Markdown
mode; `.md` / `.markdown` sources may use `# HEAD` for SmartDox operational
metadata when YAML front matter is not enough. The leading properties paragraph
inside `HEAD` is metadata-only and is not parsed as Markdown body text.

## Summary And Lead

Top-level `SUMMARY` and `LEAD` sections are document-level descriptive sections.

```dox
## SUMMARY

Short summary.

## LEAD

Introductory lead text.
```

The parser and site pipeline may use these sections to populate document
metadata and generated page content.

## Paragraphs

A paragraph is one or more adjacent text lines that are not recognized as a
special block.

```ebnf
paragraph ::= inline-line+
```

Example:

```dox
Hello
SmartDox
```

This is rendered as one paragraph with normalized whitespace.

## Lists

Unordered lists use `-`.

```ebnf
ul-item ::= indent "- " inline-text line-end continuation*
```

Ordered lists use a decimal number followed by `.`.

```ebnf
ol-item ::= indent digit+ ". " inline-text line-end continuation*
```

Definition lists use `- term :: description`.

```ebnf
dl-item ::= indent "- " inline-text " :: " inline-text line-end continuation*
```

Indentation defines nesting.

```dox
- One
 - Two
 Two continuation
```

## Tables

Pipe tables are supported.

```ebnf
table          ::= annotation* table-line+
table-line     ::= table-row | table-separator
table-row      ::= "|" table-cell ("|" table-cell)* "|"?
table-cell     ::= inline-text
table-separator ::= "|" ("-" | "|" | "+" | ":" | " ")+
```

Header, body, footer, and column alignment are inferred from separator rows.

```dox
#+CAPTION: Title
#+LABEL: tablelabel
| h1 | h2 |
|----|----|
| a  | b  |
```

CSV-backed table inclusion is supported by:

```dox
#+table: "test.csv" src
```

## Images And Figures

Image references use double brackets.

```ebnf
image ::= "[[" image-uri "]]"
```

Supported image suffixes include `png`, `jpeg`, `jpg`, `gif`, and `pdf`.

```dox
[[image/simple.png]]
```

If a caption or label annotation immediately precedes an image, the image is
treated as a figure.

```dox
#+CAPTION: Figure
#+LABEL: fig
[[image/simple.png]]
```

Generated image blocks are supported for `dot`, `ditaa`, `sm_org`, and
`sm_csv`.

```dox
#+begin_dot image/simple.png
DOT source
#+end_dot
```

## Programs And Verbatim Blocks

Source blocks use Org-style begin/end annotations.

```ebnf
program ::= "#+begin_src" parameters line-end verbatim-text "#+end_src"
```

Example:

```dox
#+begin_src scala
object Example {}
#+end_src
```

`#+begin_example` and generic `#+begin_NAME` blocks are parsed as verbatim
program blocks. The block kind is carried as program metadata when available.

## Comments

Line comments begin with `#`.

```dox
visible
# hidden
also visible
```

Comment sections start with `COMMENT`.

```dox
* COMMENT Hidden
```

Comment blocks are also supported.

```dox
#+begin_comment
hidden
#+end_comment
```

## Block Macros

Block macros use Asciidoc-style syntax.

```ebnf
block-macro ::= name "::" target "[" attributes? "]"
```

Supported names:

- `include`
- `link`
- `xref`

Examples:

```dox
include::src/test/resources/abc.dox[]
link::target.dox[label="Target"]
xref::target.dox[Target]
```

`include` produces an include node. `link` and `xref` produce hyperlinks.

## Horizontal Rules And Quotations

Horizontal rules are `---` or `----`.

```dox
---
```

A simple quotation starts with `> `.

```dox
> quoted text
```

## Inline Markup

Inline text supports the following forms.

```ebnf
bold       ::= "*" inline-text "*"
italic     ::= "/" inline-text "/"
underline  ::= "_" inline-text "_"
code       ::= "=" raw-text "="
pre        ::= "~" raw-text "~"
delete     ::= "+" inline-text "+"
```

Examples:

```dox
*bold* /italic/ _underline_ =code= ~pre~ +deleted+
```

XML-style inline tags are supported for known inline elements such as `b`, `i`,
`u`, `code`, `pre`, `del`, `tt`, and `span`.

```dox
<span>*span*</span>
```

Literal inline text can be written with:

```dox
<[literal]>
<t>*not parsed*</t>
```

## Links

Org-style links:

```ebnf
org-link ::= "[[" uri "]]" | "[[" uri "][" label "]]"
```

Markdown links:

```ebnf
markdown-link ::= "[" label "](" uri ")"
```

Automatic URL links are recognized for plain URLs.

```dox
[[http://example.com/][Example]]
[Example](http://example.com/)
http://example.com/
```

## SmartDox Site Links

Site links use an explicit inline macro:

```dox
site:[target.dox]
```

Within a DoxSite context, the target is resolved as a site-local document. The
generated link points to the target HTML page and uses the target document title
as the visible label.

The legacy shorthand remains accepted for migration:

```dox
[target.dox]
```

Legacy shorthand emits a deprecation warning and is normalized to the same
internal hyperlink form.

## Inline Macros

General inline macros use:

```ebnf
inline-macro ::= name ":[" raw-contents "]"
```

Known forms:

- `pass:[raw]` preserves raw inline contents.
- `site:[target.dox]` creates a site-local hyperlink.

Unknown inline macros are preserved as inline macro nodes.

## Markdown Mode

Markdown parsing supports:

- ATX headings: `# Heading`
- Setext headings: `Heading` followed by `==`
- Pipe tables with optional alignment row
- Unordered lists
- Ordered lists
- Definition lists using `- term :: definition`

Markdown mode shares the SmartDox document tree with the Org-style parser.
YAML front matter is accepted as document metadata. SmartDox operational
metadata sections such as `# HEAD`, `## HEADLINE`, `## BRIEF`, `## SUMMARY`, and
`## DESCRIPTION` are also accepted in Markdown mode and are normalized into the
same Dox metadata IR.

Glossary term sources may also use Markdown. The term title is resolved from the
SmartDox document title or Markdown front matter `title`; metadata properties
such as `reading`, `brief`, `summary`, `status`, and `published_at` are resolved
through the same Dox metadata path. The generated glossary metadata is therefore
independent of whether the source file is `.dox`, `.md`, or `.markdown`.

Multilingual authoring remains a SmartDox authoring feature, not a Markdown-mode
feature.

## Stability

Stable syntax in this file is backed by parser implementation and tests.
Unstable or partially implemented grammar is tracked in the journal so it can
be promoted here after behavior and tests are settled.
