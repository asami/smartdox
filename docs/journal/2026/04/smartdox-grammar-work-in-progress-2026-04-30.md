# SmartDox Grammar Work In Progress

Date: 2026-04-30

## Context

`docs/spec/smartdox-grammar.md` was added as the current grammar reference. It
captures behavior that is implemented and covered by the existing parser tests.

This journal entry keeps notes for grammar areas that are active or partially
specified, but should not yet be treated as final specification text.

## Work In Progress Areas

### Fenced Code Blocks

Triple-backquote blocks are parsed through the newer block pipeline as raw
verbatim blocks and become `Program` nodes. The intended user-facing SmartDox
syntax still needs a precise rule for:

- language/kind extraction
- caption/title metadata
- nested SmartDox text inside opaque code
- interaction with CML and generated examples

The likely target form is Markdown-compatible:

````text
```scala
object Example {}
```
````

The specification should be promoted after tests cover kind, caption, and
opaque content behavior.

### HEAD, SUMMARY, And LEAD

`HEAD` is already recognized as metadata, and `SUMMARY` / `LEAD` are used by
the site pipeline. The exact grammar still needs consolidation around:

- allowed heading levels
- whether these sections must be top-level
- how multilingual fragments are represented
- precedence between explicit metadata and section-derived metadata

### Multilingual Inline Text

The delimiter `｜` is currently used by auto-i18n workflows in titles, list
items, and metadata-facing text. The grammar needs a final statement for:

- where the delimiter is active
- where it must be treated as opaque text
- interaction with links, table cells, code, and program blocks

### SmartDox Inside SmartDox

Embedding a SmartDox document inside another SmartDox document is needed for
samples and literate explanations. The open question is whether the normative
form should be:

- fenced code blocks
- `#+begin_example`
- `pass:[...]` or a block-level pass form
- a dedicated SmartDox verbatim block

The requirement is that embedded SmartDox can be shown verbatim without being
parsed as part of the outer document.

### Site Link Labels

`site:[target.dox]` now normalizes to a hyperlink and resolves to the target
document title inside a DoxSite context. Remaining decisions:

- whether explicit labels are needed
- possible syntax such as `site:[target.dox, label="Label"]`
- behavior outside a DoxSite context
- removal schedule for legacy `[target.dox]`

### Definition Lists

Definition lists are implemented in the newer line parser and markdown parser.
The older parser has an ignored hanging test for Org-style definition lists.
Before the grammar is marked stable across all parser modes, define which parser
is authoritative for:

- nested definition lists
- continuation lines
- inline markup in terms

### Generic Begin Blocks

Generic `#+begin_NAME` / `#+end_NAME` blocks are accepted and currently become
program-like verbatim blocks. The grammar still needs rules for:

- known block names
- unknown block preservation
- attributes and parameters
- whether non-program block names should map to semantic Dox nodes

## Promotion Rule

Move an item from this journal entry into `docs/spec/smartdox-grammar.md` after:

- syntax is implemented in the primary parser path
- behavior has focused tests
- DoxSite behavior, if any, is covered
- opaque/verbatim behavior is explicit
