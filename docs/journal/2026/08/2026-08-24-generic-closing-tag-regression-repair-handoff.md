# SmartDox Generic Closing-Tag Regression Repair Handoff

Status: OPEN
Created: 2026-08-24
Source Repository: `/Users/asami/src/dev2025/smartdox` at
`1ba56ece7d699ec78adf635a2490c2d2825c821d`
Upstream Grammar Producer: `/Users/asami/src/dev2025/goldenport-scala-library` at
`266e58ab0f87a519f90858c82fce8d8ff40aeedd`
Target Repositories: `/Users/asami/src/dev2025/goldenport-scala-library` and
`/Users/asami/src/dev2025/smartdox`
Affected Consumer (read-only and out of scope):
`/Users/asami/src/dev2025/simplemodeling-org` at
`efa979b5fb5ae04f28756eff18db5f3e8ebd3b48`
Suggested Invocation: `$cncf-diagnose-fix /Users/asami/src/dev2025/smartdox/docs/journal/2026/08/2026-08-24-generic-closing-tag-regression-repair-handoff.md`

## Purpose

Repair the generic SmartDox tag parser regression that converts valid documents
containing a standalone closing tag such as `</div>` into `Error:` documents.
Restore the established XML-style tag grammar without adding a source-document
workaround or a special case for the observed article.

## Incident Evidence

The SimpleModeling.org article
`src/main/doxsite/development-process/knowledge-modeling-for-ai-collaboration.dox`
links to `object-modeling-as-structural-foundation.dox` through the valid
`site:[object-modeling-as-structural-foundation.dox]` macro. The link label is
`Error:/development-process/object-modeling-as-structural-foundation.dox`
because the target article was converted to an error document before site-link
resolution.

The generated target page records:

```text
org.goldenport.exception.NoReachDefectException
SkipOneState#character_State(..., >): d
```

The source article was introduced in SimpleModeling.org commit `a737e20` on
2026-08-17. It has an ordinary multiline `<div lang="ja"> ... </div>` block.
The observed error originates at the standalone closing-tag location.

The repair has two grammar boundaries. SmartDox Dox2Parser must use the
goldenport easy-HTML logical-line configuration with both double-quote and
back-quote protection enabled; otherwise a slash in a quoted `https://` value
can be mistaken for self-closing syntax while a multiline element is grouped.
The upstream goldenport XML states already group multiline, nested, paired,
and self-closing elements, but the existing executable cases did not combine
those forms with a quoted URL. The current SmartDox parser correction is
already present in the dirty worktree; this handoff closes its cross-repository
specification evidence without changing that production source.

SmartDox commit `496fa8c1af8ef2e75d473f4b05c25061397fb5d8` on 2026-08-21
(`Phase 7: complete generic inline open-tag grammar`) added this
self-closing-tag path to `OpenTagState`:

```scala
case '/' => SkipOneState(config, resultOpenEnd(Vector.empty), '>')
```

At the beginning of `</div>`, that path treats the closing-tag slash as a
self-closing-tag slash and then requires `>` as the next character. It instead
receives `d`, which exactly explains the generated exception.

The generated WIP site contains the same failure for five series articles in
both Japanese and English (ten pages):

- `why-reconstruct-software-development-methodology.dox`
- `what-simplemodeling-has-pursued.dox`
- `modeling-technology-system.dox`
- `model-structure-and-views.dox`
- `object-modeling-as-structural-foundation.dox`

## Frozen Repair Boundary

- Allowed repositories:
  - `/Users/asami/src/dev2025/goldenport-scala-library` as the upstream
    LogicalLines grammar producer; its mutation is executable-spec-only.
  - `/Users/asami/src/dev2025/smartdox` as the SmartDox consumer/parser/spec
    repository; its parser correction is part of the existing dirty baseline,
    while this closure edits only the paths listed below.
- Exact owned edit paths:
  - `/Users/asami/src/dev2025/goldenport-scala-library/src/test/scala/org/goldenport/parser/LogicalLinesSpec.scala`
  - `/Users/asami/src/dev2025/smartdox/docs/journal/2026/08/2026-08-24-generic-closing-tag-regression-repair-handoff.md`
  - `/Users/asami/src/dev2025/smartdox/docs/spec/smartdox-grammar.md`
  - `/Users/asami/src/dev2025/smartdox/src/test/scala/org/smartdox/parser/DoxInlineParserSpec.scala`
  - `/Users/asami/src/dev2025/smartdox/src/test/scala/org/smartdox/parser/Dox2ParserSpec.scala`
- The SmartDox `Dox2Parser` lines configuration is expected to be
  `LogicalLines.Config.easyHtml.copy(useDoubleQuote = true, useBackQuote = true)`.
  No SmartDox production-source edit is admitted in this closure.
- SimpleModeling.org is read-only and out of scope; downstream regeneration
  remains a separate consumer step.
- No `DoxSite` source or generated-output edit is part of this closure.
- Allowed semantic change: restore correct recognition of already-supported
  paired generic tags while retaining the accepted terminal boolean-attribute
  and immediate self-closing-tag forms.
- Prohibited expansion: article-source rewrites, `site:` link workarounds,
  a `div`-only special case, new public parser APIs, generic HTML feature
  expansion, rendering changes, unrelated parser refactoring, publication,
  commit, or deployment.
- Preserve the current unrelated worktree changes in SmartDox and
  SimpleModeling.org. Do not reset, stash, rewrite, or regenerate over them
  as part of this repair.

## Required Grammar Contract

Amend the existing `Inline Markup` section of
`docs/spec/smartdox-grammar.md` so its grammar distinguishes these forms:

```ebnf
generic-open-tag         ::= "<" tag-name attributes? ">"
generic-closing-tag      ::= "</" tag-name ">"
generic-self-closing-tag ::= "<" tag-name attributes? "/>"
generic-element          ::= generic-open-tag inline-content generic-closing-tag
```

The slash in `generic-closing-tag` is structural and must never be interpreted
as the self-closing marker. The self-closing marker is valid only after a
non-empty tag name and any accepted attributes, immediately before `>`.
Paired tags must match by name. A correctly paired generic tag must remain
valid when its opening tag, content, and closing tag are separated by logical
lines. This records existing SmartDox behavior; it does not extend the accepted
tag vocabulary.

## Required Executable Specifications

Add Given/When/Then executable specifications that prove all of the following.

1. `LogicalLines` groups one multiline generic element into exactly one
   unchanged logical line when `easyHtml` enables double-quote and back-quote
   protection, including a quoted `https://` attribute, a nested paired tag,
   an immediate self-closing image-like child, and an own-line matching close.
2. `DoxInlineParser` accepts a paired generic tag and preserves its contents.
3. `DoxInlineParser` distinguishes `</span>` from `<span/>`; neither form is
   misclassified as the other.
4. Nested paired tags and an immediate self-closing child retain authored order.
5. `Dox2Parser` accepts a multiline `<div lang="ja"> ... </div>` block whose
   closing tag begins a logical line, including a quoted `https://` attribute,
   an inline `span`, and an image reference in its contents.
6. A malformed self-closing form such as `<span enabled/ >` remains on its
   documented deterministic failure path.

The new cases must fail against the regressed parser and pass only after the
grammar correction. Do not replace this contract with a single fixture that
checks merely for the absence of `Error:` output.

## Required Validation

1. Run the focused `LogicalLinesSpec` suite through the serialized CNCF SBT
   command path.
2. Run the focused SmartDox `DoxInlineParserSpec` and `Dox2ParserSpec` suites
   through the serialized CNCF SBT command path.
3. Run the full SmartDox validation required by the repair workflow after the
   focused suites pass.
4. Run `git diff --check` for the frozen boundary.
5. Verify that no source or generated output in SimpleModeling.org was changed
   by the SmartDox repair.
6. Only after a corrected SmartDox runtime is selected through the repository
   release/version rules, perform a separately authorized SimpleModeling.org
   site regeneration. Confirm that the five listed source articles produce
   their authored titles and that the Part 6 `site:` link label resolves to
   `Object Modeling as a Structural Foundation` / `構造基盤としてのオブジェクトモデリング`.

## Completion Contract

- The generic closing-tag parser defect is repaired at its grammar boundary.
- The Markdown grammar and executable grammar specifications agree.
- The focused and full validation evidence is recorded by the repair workflow.
- The downstream site regeneration remains a separate consumer step until its
  runtime version and the SimpleModeling.org dirty-worktree boundary are
  explicitly authorized.

## Non-goals

- Altering any SimpleModeling.org article to avoid `</div>`.
- Hiding parse exceptions by changing error-document titles or link labels.
- Publishing a SmartDox artifact or updating a consumer dependency.
- General parser-state decomposition; the existing parser-size hygiene item is
  not part of this repair.
