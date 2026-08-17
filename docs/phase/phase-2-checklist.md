# Phase 2 Checklist

This checklist is the authoritative progress ledger for Phase 2: RDF-Grounded
Terminology Semantics.

## TERM2-01: Terminology and RDF Identity Contract

Status: PLANNED

- [ ] Promote the reviewed terminology design into `docs/design` and the
      normative grammar, inline-semantics, tag-system, and extraction specs.
- [ ] Define one RDF URI node as the canonical glossary concept identity.
- [ ] Define separate identities and relations for concept, source document,
      and public glossary page resources.
- [ ] Select and document separate TBox and term-instance namespaces.
- [ ] Define deterministic CURIE expansion and absolute-IRI normalization.
- [ ] Define `<term ref>`, `<dfn about>`, and `<noterm>` grammar, attributes,
      nesting, rendering, and extraction semantics.
- [ ] Define display forms, localized first use, and abbreviation expansion.
- [ ] Define separate visible and speech forms for each output locale; specify
      that a first-use English annotation is not spoken after the localized
      canonical label by default.
- [ ] Define structured glossary metadata for preferred labels, short labels,
      aliases, abbreviations, scope, and bare-label linking policy.
- [ ] Define compatibility and diagnostic outcomes before implementation.

## TERM2-02: Parser, AST, and Resolution

Status: PLANNED

- [ ] Add a dedicated inline term-reference AST node carrying the shared
      resource IRI and display form.
- [ ] Add a dedicated inline non-term AST node that suppresses only terminology
      resolution.
- [ ] Extend `<dfn>` with a concept resource without conflating it with the
      existing local HTML `id` anchor.
- [ ] Parse CURIE and absolute-IRI references and preserve source locations.
- [ ] Resolve explicit references before automatic glossary matching.
- [ ] Apply `linkable`, `context-only`, and `explicit-only` deterministically.
- [ ] Diagnose unresolved resources, duplicate concept nodes, ambiguous surface
      forms, invalid prefixes, and labels incompatible with the selected form.
- [ ] Prove that no resolution depends on glossary registration order.

## TERM2-03: Glossary and Knowledge Projection

Status: PLANNED

- [ ] Use the shared RDF URI node as the glossary registry key.
- [ ] Remove canonical identity dependence on English label, random `<dfn>`
      anchor, source path, and generated `category:slug` dashboard IDs.
- [ ] Preserve preferred labels, alternate labels, abbreviations, scope, and
      linking policy in generated term metadata.
- [ ] Render explicit and automatic term links from the resolved concept node.
- [ ] Project a locale-appropriate speech label separately from visible
      bilingual markup, without reconstructing speech text from rendered HTML.
- [ ] Emit the concept node in HTML metadata and term occurrence output.
- [ ] Emit coherent RDF/JSON-LD relating concept, definition, source document,
      and public glossary page.
- [ ] Emit BoK occurrence records containing concept node, surface form,
      locale, occurrence kind, resolution kind, source path, and location.
- [ ] Verify a downstream consumer can use the output without reconstructing
      identity from the displayed label.

## TERM2-04: Compatibility and SimpleModeling.org Acceptance

Status: PLANNED

- [ ] Preserve existing unambiguous automatic glossary linking.
- [ ] Preserve existing `<dfn>` documents that do not declare `about`.
- [ ] Preserve `span strategy="stable"` during migration and specify
      `<noterm>` as the terminology-specific replacement.
- [ ] Prove Japanese canonical labels with English labels at first use.
- [ ] Prove `オブジェクトモデリング（Object Modeling）` is visible at first
      use while Japanese narration says `オブジェクトモデリング` only once.
- [ ] Prove abbreviation expansion for `CML（Cozy Modeling Language）` or its
      locale-appropriate equivalent.
- [ ] Prove the visible expansion of an abbreviation does not cause duplicated
      narration unless an explicit speech override requests it.
- [ ] Prove explicit short-label references for technical ownership,
      composition, and realization concepts.
- [ ] Prove ordinary-language occurrences of the same surface forms remain
      unlinked when marked non-term or governed by `explicit-only`.
- [ ] Prove two concepts sharing one surface form never resolve to each other.
- [ ] Provide bounded SimpleModeling.org fixtures without bulk-migrating the
      site or changing glossary definitions outside fixture needs.

## TERM2-05: Closure

Status: PLANNED

- [ ] Run focused parser, glossary, link, site, RDF, and extraction specs.
- [ ] Run the full SmartDox test suite through the serialized SBT launcher.
- [ ] Run `git diff --check`.
- [ ] Complete a read-only post-implementation review.
- [ ] Fix all current-boundary blockers and complete a clean focused re-review
      when fixes are made.
- [ ] Record the SimpleModeling.org and textus-bok/MCP handoff boundaries.
- [ ] Commit validated changes with required version updates.
- [ ] Close Phase 2 from checklist results.
