# LaTeX table layout

## Observable contract

SmartDox tables rendered by `Dox2LatexConverter` occupy the current available
LaTeX `\linewidth`. The total declared table width includes cell padding and
all leading, intervening and trailing vertical rules. Equal paragraph columns
wrap breakable Japanese and English prose within that width. Caption, header,
body and footer text must remain complete and in their original order, with
existing LaTeX escaping and bold header semantics preserved.

The widest header, body or footer determines the column count. A table with
zero-width compartments still declares one usable column. The converter keeps
the `longtable` environment so that the native renderer can break tables across
pages. Conversion must preserve all rows, including tables with 64 or more
rows; conversion alone does not establish native pagination or absence of PDF
clipping.

This contract covers prose that the selected LaTeX engine can break, including
Japanese text and English words separated by ordinary spaces. It does not add
arbitrary splitting of unbreakable tokens. It does not shrink fonts, scale or
truncate tables, change the table AST/API, or change PDF engines or preambles.

## Executable and native acceptance

[Dox2LatexTableSpec](../../src/test/scala/org/smartdox/converters/Dox2LatexTableSpec.scala)
defines complete bilingual cells, escaping and compartment order, both engines
and formats, empty and widest-compartment tables, many-row preservation, and
ScalaCheck variable-column/row behavior. One fixture uses the actual SmartDox
pipe parser.

Actual Cozy PDF acceptance is separate and pending in
[Cozy Phase 71.7](/Users/asami/src/dev2025/cozy/docs/phase/phase-71.7.md).
The selected native route consumes the corrected development SNAPSHOT through
normal dependency resolution and renders separate four-column, 32-body-row
Japanese and English documents. PDF text, transformed rule geometry, multiple
pages and every rendered page require observation before acceptance.
