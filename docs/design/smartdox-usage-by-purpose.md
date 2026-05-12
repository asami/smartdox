# SmartDox Usage By Purpose

Status: design note
Date: 2026-04-30

This document summarizes the recommended SmartDox notation by common authoring
purpose. The grammar details are defined in `docs/spec/smartdox-grammar.md`.

## Image Display

Use double-bracket image references for ordinary image display.

```dox
[[images/address-model.png]]
```

Use `#+CAPTION` and `#+LABEL` immediately before the image when the image should
be treated as a figure.

```dox
#+CAPTION: Address model overview
#+LABEL: fig-address-model
[[images/address-model.png]]
```

Use generated image blocks when the source text should be kept with the
document and rendered as an image artifact.

```dox
#+begin_dot images/address-model.png
digraph Address {
  Address -> CountryCode
  Address -> PostalCode
}
#+end_dot
```

Guideline:

- Use plain `[[...]]` for screenshots, diagrams, and static assets.
- Use figure annotations when the image needs a caption, cross reference, or
  stable label.
- Use `#+begin_dot` or related generated-image blocks when the diagram source is
  part of the document.

## External Article References

Use normal links for external articles.

```dox
[[https://schema.org/PostalAddress][schema.org PostalAddress]]
```

Markdown-style links are also accepted in SmartDox inline parsing.

```dox
[schema.org PostalAddress](https://schema.org/PostalAddress)
```

For site-local articles, use `site:[...]`.

```dox
site:[literate-modeling/what-is-literate-model.dox]
```

In a DoxSite context, `site:[...]` resolves to the generated HTML page and uses
the target document title as the visible label.

Guideline:

- Use explicit labels for external URLs so generated pages remain readable.
- Use `site:[target.dox]` for internal article references.
- Avoid legacy `[target.dox]`; it is accepted only as a migration form.

## Tables

Use pipe tables for small structured tables.

```dox
| Field | Meaning |
|-------|---------|
| addressCountry | Country code |
| postalCode | Postal or ZIP code |
```

Use caption and label annotations when the table is part of the document
structure.

```dox
#+CAPTION: Address field mapping
#+LABEL: table-address-fields
| Field | Meaning |
|-------|---------|
| addressCountry | Country code |
| postalCode | Postal or ZIP code |
```

Use CSV-backed table inclusion when the table is maintained outside the
document.

```dox
#+table: "data/address-fields.csv" src
```

Guideline:

- Use inline pipe tables for short examples and explanatory mappings.
- Use external CSV when the table is large, generated, or maintained as data.
- Keep cell content simple; complex nested block content belongs outside the
  table.

## Program Examples

Use `#+begin_src` for source code examples.

```dox
#+begin_src scala
case class Address(
  addressCountry: CountryCode,
  postalCode: PostalCode
)
#+end_src
```

Use `#+begin_example` for verbatim text that should not be interpreted as code.

```dox
#+begin_example
addressCountry: JP
postalCode: "160-0022"
#+end_example
```

Use inline `pass:[...]` only for small raw inline fragments.

```dox
pattern: pass:[^[A-Z]{2}$]
```

Guideline:

- Use `#+begin_src <kind>` for code that has a language or tool kind.
- Use `#+begin_example` for command output, sample data, and opaque document
  fragments.
- Use `pass:[...]` for inline text that must bypass inline parsing.
- Avoid using paragraph indentation alone to express code blocks; choose an
  explicit verbatim form.
