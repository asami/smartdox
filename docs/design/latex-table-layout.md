# LaTeX table layout design

## Decision

Implement the [observable contract](../spec/latex-table-layout.md) within the
existing [converter](../../src/main/scala/org/smartdox/converters/Dox2LatexConverter.scala).
Keep `longtable`, its existing row terminators and caption/head/body/footer
projection. Replace natural-width `l` columns with equal `p` paragraph columns.

For `n = max(1, Table.width)`, where width includes header, body and footer,
each paragraph content width is:

```text
(linewidth - 2*n*tabcolsep - (n+1)*arrayrulewidth) / n
```

The two cell padding contributions per column and all `n+1` vertical rules
therefore fit in the current `\linewidth`. The emitted standard e-TeX
expression uses parenthesized subtraction and division, terminated by `\relax`:

```latex
p{\dimexpr(\linewidth-8\tabcolsep-5\arrayrulewidth)/4\relax}
```

Repeat the dimension for each column, retaining the leading, intervening and
trailing `|`. e-TeX dimension arithmetic is described in the
[e-TeX manual, section 3.5](https://mirrors.ctan.org/systems/doc/etex/etex_man.pdf).
No fixed page-width literal is required. Standard `p` columns retain standard
LaTeX multiline last-column and row-terminator semantics; this change introduces
no alignment command that would redefine the existing row terminator.

No smaller font, scaling, truncation, new table model, API, dependency, package
or PDF-engine change is included. Caption escaping, bold headers and `hline`,
body/footer ordering, constructors and preambles remain with their existing
owners. Native page-breaking and visual fit are evaluated separately through
the actual Cozy PDF consumer.
