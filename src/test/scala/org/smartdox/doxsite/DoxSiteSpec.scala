package org.smartdox.doxsite

import scalaz._, Scalaz._

import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.GivenWhenThen
import org.junit.runner.RunWith
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.goldenport.i18n.I18NContext
import org.goldenport.tree.TreeTransformer
import org.smartdox._
import org.smartdox.SmartDoxSpecVocabulary
import org.smartdox.parser.UseDoxParser
import org.smartdox.generator.Context
import org.smartdox.transformers.AutoI18nTransformer
import io.circe.parser

/*
 * @since   Feb. 24, 2025
 *  version Feb. 28, 2025
 *  version Mar.  1, 2025
 *  version Apr.  3, 2025
 *  version Jun. 17, 2025
 *  version Aug. 16, 2025
 *  version Apr. 20, 2026
 * @version Jun. 25, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxSiteSpec
    extends AnyWordSpec
    with GivenWhenThen
    with UseDoxParser
    with SmartDoxSpecVocabulary {
  val context = Context.create()
  implicit val dctx = context.dateTimeContext
  "DoxSite" should {
    "create" in {
      Given("an existing SmartDox site source directory")
      When("SmartDox creates a DoxSite model from the source directory")
      val site = create_site(context, new File("src/test/resources/site1"))

      Then("the site model is created for downstream site generation")
      site should not be null
    }

    "resolve site inline macro as internal link" in {
      Given("a site source that uses the site:[...] inline macro")
      val config = DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full)

      When("SmartDox renders the site through the DoxSite pipeline")
      val site = create_site(context, new File("src/test/resources/site-link"), config)
      val realm = site_realm(site, context)
      implicit val i18ncontext: I18NContext = context.i18NContext
      val html = html_at(realm, "/en/index.html", "en/index.html")

      Then("the site macro is resolved to a local HTML link")
      html should include_html("href=\"target.html\"")
      html should include_html("Target Page")
      And("the unresolved source macro does not leak into the generated page")
      html should not (include_text("site:[target.dox]"))
    }

    "emit localized document fragments after glossary and site link processing" in {
      val dir = Files.createTempDirectory("smartdox-document-fragments")
      try {
        Given("a site with a home document, an internal site link, a glossary term, and a manual page")
        _write(dir.resolve("site.conf"), "site { output { locale_mode = \"single_locale_root\" } }\n")
        _write(dir.resolve("index.dox"),
          """Fragment Home
            |=============
            |
            |# HEAD
            |
            |status=published
            |published_at=2026-06-24
            |
            |## HEADLINE
            |
            |Fragment headline.
            |
            |## BRIEF
            |
            |Fragment brief.
            |
            |# Body
            |
            |Runtime refers to site:[target.dox].
            |""".stripMargin)
        _write(dir.resolve("target.dox"),
          """Target Page
            |===========
            |
            |# HEAD
            |
            |status=published
            |published_at=2026-06-24
            |
            |# Body
            |
            |Target body.
            |""".stripMargin)
        _write(dir.resolve("manual/index.dox"),
          """Manual
            |======
            |
            |# HEAD
            |
            |status=published
            |published_at=2026-06-24
            |
            |# Body
            |
            |Runtime is a manual operation word.
            |""".stripMargin)
        _write(dir.resolve("glossary/architecture/runtime.dox"),
          """Runtime
            |=======
            |
            |# HEAD
            |
            |status=published
            |published_at=2026-06-24
            |
            |# Definition
            |
            |Runtime term.
            |""".stripMargin)

        When("SmartDox builds the site and machine-readable metadata")
        val site = DoxSite.create(context, dir.toFile, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
        val realm = site.toRealm(context)
        implicit val i18ncontext: I18NContext = context.i18NContext
        val fragments = realm.getString("metadata/documents/fragments.json").get
        val homebody = _fragment_body(fragments, "index.dox", "ja")
        val manualbody = _fragment_body(fragments, "manual/index.dox", "ja")

        Then("the fragment metadata contains localized document identity and effective description")
        fragments should include_metadata(""""source_path" : "index.dox"""")
        fragments should include_metadata(""""public_path" : "index.html"""")
        fragments should include_metadata(""""headline" : "Fragment headline."""")
        fragments should include_metadata(""""brief" : "Fragment brief."""")
        fragments should include_metadata(""""locale" : "ja"""")
        fragments should not (include_metadata(""""locale" : "en""""))

        And("the body HTML is produced after site link and glossary processing")
        homebody should include_html("""href="target.html"""")
        homebody should include_html("""class="glossary"""")
        homebody should not (include_text("site:[target.dox]"))

        And("manual fragments keep the existing glossary auto-link exclusion")
        manualbody should include_html("Runtime is a manual operation word")
        manualbody should not (include_html("""class="glossary""""))

        And("document fragments remain available when public HTML output is scoped to Home only")
        val homeonlyconfig = DoxSite.Config.default.copy(
          strategy = DoxSite.Strategy.Full,
          outputTreeTransformerConfig = Some(
            TreeTransformer.Config(
              TreeTransformer.Config.Scope(
                TreeTransformer.Config.Scope.Policy.HomeOnly
              )
            )
          )
        )
        val homeonlysite = DoxSite.create(context, dir.toFile, None, homeonlyconfig)
        val homeonlyrealm = homeonlysite.toRealm(context)
        val homeonlyfragments =
          homeonlyrealm.getString("metadata/documents/fragments.json").get
        homeonlyfragments should include_metadata(""""source_path" : "index.dox"""")
        homeonlyfragments should include_metadata(""""source_path" : "manual/index.dox"""")
      } finally {
        _delete(dir)
      }
    }

    "keep program text opaque during auto i18n" in {
      Given("a document whose program block contains auto-i18n separator text")
      val dox = Document(Head(), Body(List(Program.create("alpha｜beta"))))
      val txctx = DoxSiteTransformer.Context(
        DoxSiteTransformer.Config(),
        context,
        TreeTransformer.Context.default[Node],
        context.doxContext
      )

      When("the automatic i18n transformer processes the document")
      val transformed = Dox.transform(dox, new AutoI18nTransformer(txctx))

      Then("program text remains opaque and is not split into localized spans")
      transformed should contain_program_text("alpha｜beta")
    }

    "keep auto i18n active in section title and list items" in {
      Given("a document whose section title and list item contain auto-i18n separator text")
      val dox = Document(
        Head(),
        Body(
          List(
            Section(List(Text("Address Sample｜住所モデルの文芸モデル")), List(
              Ul(List(Li(List(Text("自然言語｜Natural Language")))))
            ))
          )
        )
      )
      val txctx = DoxSiteTransformer.Context(
        DoxSiteTransformer.Config(),
        context,
        TreeTransformer.Context.default[Node],
        context.doxContext
      )

      When("the automatic i18n transformer processes structured text outside program blocks")
      val transformed = Dox.transform(dox, new AutoI18nTransformer(txctx))

      Then("the section title is converted into localized span alternatives")
      transformed should have_first_section_title_alternatives(2)
      transformed should have_first_section_title_span
      And("list item text is also converted into localized span alternatives")
      transformed should have_first_list_item_span
    }


    "skip automatic glossary links in Manual pages" in {
      val dir = Files.createTempDirectory("smartdox-manual-glossary-skip")
      try {
        Given("a Manual page that contains a term also defined in the site glossary")
        _write(dir.resolve("site.conf"), "site { output { locale_mode = \"single_locale_root\" } }\n")
        _write(dir.resolve("manual/index.dox"),
          """Manual
            |======
            |
            |# HEAD
            |
            |status=work-in-progress
            |published_at=2026-06-05
            |
            |# Body
            |
            |Runtime is a manual operation word.
            |""".stripMargin)
        _write(dir.resolve("glossary/architecture/runtime.dox"),
          """Runtime
            |=======
            |
            |# HEAD
            |
            |status=work-in-progress
            |published_at=2026-06-05
            |
            |# Definition
            |Runtime term.
            |""".stripMargin)
        When("SmartDox generates the site with glossary auto-linking enabled")
        val site = create_site(context, dir.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
        val realm = site_realm(site, context)
        implicit val i18ncontext: I18NContext = context.i18NContext
        val html = html_at(realm, "/ja/manual/index.html", "ja/manual/index.html")

        Then("Manual content is rendered as normal text")
        html should include_html("Runtime is a manual operation word")
        And("Manual pages keep the glossary auto-link exclusion")
        html should not (include_html("class=\"glossary\""))
        html should not (include_html("glossary/architecture/runtime"))
      } finally {
        _delete(dir)
      }
    }

    "keep published dox pages in production strategy" in {
      val dir = Files.createTempDirectory("smartdox-production-published")
      try {
        Given("a production site with one published SmartDox article and one work-in-progress article")
        _write(dir.resolve("site.conf"), "site { output { locale_mode = \"single_locale_root\" } }\n")
        _write(dir.resolve("published.dox"),
          """Published Article
            |=================
            |
            |# HEAD
            |
            |status=published
            |published_at=2026-06-08
            |
            |## SUMMARY
            |
            |Published summary.
            |
            |# Body
            |
            |Published body.
            |""".stripMargin)
        _write(dir.resolve("wip.dox"),
          """Work In Progress Article
            |========================
            |
            |# HEAD
            |
            |status=work-in-progress
            |published_at=2026-06-08
            |
            |# Body
            |
            |WIP body.
            |""".stripMargin)
        When("SmartDox generates the site with the production strategy")
        val site = create_site(context, dir.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site_realm(site, context)
        implicit val i18ncontext: I18NContext = context.i18NContext

        Then("published pages are included in the production output")
        html_at(realm, "/ja/published.html", "ja/published.html") should include_html("Published body.")
        And("work-in-progress pages are excluded from the production output")
        realm.getString("/ja/wip.html").orElse(realm.getString("ja/wip.html")) shouldBe None
      } finally {
        _delete(dir)
      }
    }

    "parse Markdown pages through Markdown mode and YAML front matter metadata" in {
      val dir = Files.createTempDirectory("smartdox-markdown-front-matter")
      try {
        Given("a Markdown article with YAML front matter and GitHub-style Markdown body")
        _write(dir.resolve("site.conf"), "site { output { locale_mode = \"single_locale_root\" } }\n")
        _write(dir.resolve("published.md"),
          """---
            |title: Published Markdown
            |headline: Published Markdown Headline
            |brief: Published Markdown brief.
            |status: published
            |published_at: 2026-06-23
            |---
            |
            |# Published Markdown Body
            |
            |Markdown body with **bold** text and [a link](https://example.com).
            |""".stripMargin)
        _write(dir.resolve("wip.md"),
          """---
            |title: WIP Markdown
            |status: work-in-progress
            |published_at: 2026-06-23
            |---
            |
            |# WIP Markdown Body
            |
            |WIP markdown body.
            |""".stripMargin)
        When("SmartDox parses Markdown through filename-based Markdown mode")
        val site = create_site(context, dir.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site_realm(site, context)
        implicit val i18ncontext: I18NContext = context.i18NContext
        val html = html_at(realm, "/ja/published.html", "ja/published.html")

        Then("Markdown body syntax is rendered through the Dox IR")
        html should include_html("Published Markdown Body")
        html should include_html("Markdown body with")
        html should include_html("https://example.com")
        And("YAML front matter participates in production status filtering")
        realm.getString("/ja/wip.html").orElse(realm.getString("ja/wip.html")) shouldBe None
      } finally {
        _delete(dir)
      }
    }

    "use Markdown SmartDox HEAD metadata for site strategy" in {
      val dir = Files.createTempDirectory("smartdox-markdown-head-metadata")
      try {
        Given("a Markdown document that uses SmartDox HEAD metadata instead of YAML front matter")
        _write(dir.resolve("site.conf"), "site { output { locale_mode = \"single_locale_root\" } }\n")
        _write(dir.resolve("published.md"),
          """# HEAD
            |
            |status=published
            |published_at=2026-06-23
            |
            |## SUMMARY
            |
            |Published Markdown HEAD summary.
            |
            |# Published Body
            |
            |Published markdown body.
            |""".stripMargin)
        _write(dir.resolve("wip.md"),
          """# HEAD
            |
            |status=work-in-progress
            |published_at=2026-06-23
            |
            |# WIP Body
            |
            |WIP markdown body.
            |""".stripMargin)
        When("SmartDox parses the Markdown document through the Dox parser path")
        val site = create_site(context, dir.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site_realm(site, context)
        implicit val i18ncontext: I18NContext = context.i18NContext

        Then("SmartDox HEAD metadata controls production inclusion")
        html_at(realm, "/ja/published.html", "ja/published.html") should include_html("Published markdown body.")
        And("work-in-progress Markdown HEAD metadata keeps draft content out of production")
        realm.getString("/ja/wip.html").orElse(realm.getString("ja/wip.html")) shouldBe None
      } finally {
        _delete(dir)
      }
    }

    "emit Markdown glossary term metadata through Dox IR" in {
      val dir = Files.createTempDirectory("smartdox-markdown-glossary-term")
      try {
        Given("a glossary term authored as Markdown with front matter metadata")
        _write(dir.resolve("site.conf"), "site { output { locale_mode = \"single_locale_root\" } }\n")
        _write(dir.resolve("glossary/architecture/runtime.md"),
          """---
            |title: Runtime
            |brief: Runtime summary from Markdown front matter.
            |reading: らんたいむ
            |status: published
            |published_at: 2026-06-23
            |---
            |
            |Runtime definition from Markdown.
            |""".stripMargin)
        When("SmartDox builds glossary metadata from the normalized Dox IR")
        val site = create_site(context, dir.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site_realm(site, context)
        implicit val i18ncontext: I18NContext = context.i18NContext
        val terms = metadata_at(realm, "metadata/glossary/terms.json")

        Then("front matter metadata becomes glossary term metadata")
        terms should include_metadata(""""id" : "architecture:runtime"""")
        terms should include_metadata(""""title" : "Runtime"""")
        terms should include_metadata(""""reading" : "らんたいむ"""")
        terms should include_metadata("Runtime summary from Markdown front matter.")
        terms should include_metadata("\"source_path\" : \"glossary/architecture/runtime.md\"")
        And("the Markdown body becomes the glossary definition fragment")
        terms should include_metadata("Runtime definition from Markdown.")
      } finally {
        _delete(dir)
      }
    }

    "bibliography/reference source knowledge" which {
      "emit bibliography metadata with inline citations and RDF source refs" in {
        val dir = Files.createTempDirectory("smartdox-bibliography-metadata")
        try {
          Given("a BoK site with curated bibliography source documents")
          _write(dir.resolve("site.conf"), "site { output { locale_mode = \"single_locale_root\" } }\n")
          _write(dir.resolve("bibliography/concept/design-patterns.bib.dox"),
            """Design Patterns
              |===============
              |
              |# HEAD
              |
              |title = "Design Patterns"
              |brief = "Reusable object-oriented design catalog."
              |status = "published"
              |published_at = "1994-10-21"
              |id = "bib:design-patterns"
              |key = "gamma1995designpatterns"
              |type = "book"
              |authors = ["Erich Gamma", "Richard Helm", "Ralph Johnson", "John Vlissides"]
              |publisher = "Addison-Wesley"
              |identifiers.isbn = "9780201633610"
              |terms = ["pattern", "object-oriented design"]
              |citation = "Gamma et al. Design Patterns. Addison-Wesley, 1994."
              |
              |# Overview
              |
              |A reference book for design patterns.
              |""".stripMargin)
          _write(dir.resolve("bibliography/technology/crossref.bib.md"),
            """---
              |title: Crossref REST API
              |brief: Metadata search API for scholarly references.
              |id: bib:crossref-api
              |type: web-page
              |source_url: https://api.crossref.org
              |accessed_at: 2026-06-24
              |terms:
              |  - bibliography
              |bibtex:
              |  source_url: https://api.crossref.org/works
              |---
              |
              |# Crossref REST API
              |
              |A web reference used for bibliography search.
              |""".stripMargin)
          And("article documents declare structured bibliography references in Markdown and SmartDox HEAD metadata")
          _write(dir.resolve("technology/design-article.md"),
            """---
              |title: Design Article
              |bibliography:
              |  refs:
              |    - bib:design-patterns
              |    - doi:10.5555/unresolved-reference
              |---
              |
              |# Design Article
              |
              |This article cites a local bibliography entry and an external DOI.
              |""".stripMargin)
          _write(dir.resolve("technology/dox-design-article.dox"),
            """Dox Design Article
              |==================
              |
              |# HEAD
              |
              |title = "Dox Design Article"
              |bibliography.refs = ["openlibrary:works/OL31219436W"]
              |
              |# Overview
              |
              |This Dox article cites an external OpenLibrary bibliography id.
              |""".stripMargin)
          And("article body text uses inline bib citation keys for prose citations")
          _write(dir.resolve("technology/inline-bibliography.dox"),
            """Inline Bibliography
              |===================
              |
              |# HEAD
              |
              |title = "Inline Bibliography"
              |
              |# Overview
              |
              |This article cites Design Patterns with bib:[gamma1995designpatterns] and Refactoring with bib:[fowler1999refactoring].
              |""".stripMargin)
          And("local BibTeX collections are available as resolver sources and supplement sources")
          _write(dir.resolve("bibliography/concept/design-patterns.bib"),
            """@book{design-patterns,
              |  title = {BibTeX Shadow Design Patterns},
              |  author = {Shadow, Writer},
              |  year = {1999}
              |}
              |""".stripMargin)
          _write(dir.resolve("bibliography/technology/refactoring.bib"),
            """@book{fowler1999refactoring,
              |  title = {Refactoring: {Improving} the Design of Existing Code},
              |  author = {Fowler, Martin},
              |  year = {1999},
              |  publisher = {Addison-Wesley},
              |  isbn = {9780201485677}
              |}
              |""".stripMargin)

          When("SmartDox builds BoK site metadata from the normal DoxSite pipeline")
          val site = DoxSite.create(context, dir.toFile, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
          val realm = site.toRealm(context)
          implicit val i18ncontext: I18NContext = context.i18NContext
          val bibliography = realm.getString("metadata/bibliography/bibliography.json").get
          val ttl = realm.getString("site.ttl").get

          Then("curated bibliography sources become deterministic bibliography entries")
          bibliography should include_metadata("\"id\" : \"bib:design-patterns\"")
          bibliography should include_metadata("\"entry_type\" : \"book\"")
          bibliography should include_metadata("\"category\" : \"concept\"")
          bibliography should include_metadata("\"isbn\" : \"9780201633610\"")
          bibliography should include_metadata("\"key\" : \"gamma1995designpatterns\"")
          bibliography should include_metadata("\"id\" : \"bib:crossref-api\"")
          bibliography should include_metadata("\"entry_type\" : \"web-page\"")
          bibliography should include_metadata("\"source_url\" : \"https://api.crossref.org\"")
          And("structured article references that have no local definition become unresolved external references")
          bibliography should include_metadata("\"id\" : \"doi:10.5555/unresolved-reference\"")
          bibliography should include_metadata("\"id\" : \"openlibrary:works/OL31219436W\"")
          And("inline bib citations resolve by bibliography id, bibliography key, and BibTeX citation key")
          bibliography should include_metadata("\"source_refs\"")
          bibliography should include_metadata("\"citation_key\" : \"gamma1995designpatterns\"")
          bibliography should include_metadata("\"citation_key\" : \"fowler1999refactoring\"")
          bibliography should include_metadata("\"public_path\" : \"technology/inline-bibliography.html\"")
          bibliography should not (include_metadata("\"id\" : \"gamma1995designpatterns\""))
          bibliography should include_metadata("\"id\" : \"bib:fowler1999refactoring\"")
          And("BibTeX-only entries are materialized without overriding curated bibliography metadata")
          bibliography should include_metadata("\"source_kind\" : \"bibtex-only\"")
          bibliography should include_metadata("Refactoring: {Improving} the Design of Existing Code")
          bibliography should include_metadata("\"isbn\" : \"9780201485677\"")
          bibliography.indexOf("bib:design-patterns") should be < bibliography.indexOf("bib:crossref-api")
          "\"id\" : \"bib:design-patterns\"".r.findAllIn(bibliography).size shouldBe 1
          bibliography should include_metadata("\"source_path\" : \"bibliography/concept/design-patterns.bib.dox\"")
          bibliography should not (include_metadata("BibTeX Shadow Design Patterns"))

          And("article-to-bibliography source references are represented in the site RDF graph")
          ttl should include_metadata("technology/inline-bibliography")
          ttl should include_metadata("https://schema.org/citation")
          ttl should include_metadata("http://purl.org/dc/terms/references")

          And("article pages render inline citation links and a generated References section")
          val article = realm.getString("/ja/technology/inline-bibliography.html").
            orElse(realm.getString("ja/technology/inline-bibliography.html")).
            orElse(realm.getString("/en/technology/inline-bibliography.html")).
            orElse(realm.getString("en/technology/inline-bibliography.html")).
            get
          article should include_html("bibliography-citation")
          article should include_html("[Gamma et al. 1994]")
          article should include_html("[Fowler 1999]")
          article should include_html("Bibliography")

          And("bibliography entries themselves are represented in the site RDF graph")
          ttl should include_metadata("bibliography/concept/design-patterns")
          ttl should include_metadata("BibliographicResource")
          ttl should include_metadata("isbn:9780201633610")
          ttl should include_metadata("Gamma et al. Design Patterns.")
          ttl should include_metadata("object-oriented design")
          ttl should include_metadata("https://api.crossref.org")
        } finally {
          _delete(dir)
        }
      }
    }

    "keep scenario metadata as DocumentMetaData properties without semantic extraction" in {
      val dir = Files.createTempDirectory("smartdox-scenario-source-metadata")
      try {
        Given("a BoK source tree that contains scenario metadata in Markdown front matter and SmartDox HEAD")
        _write(dir.resolve("site.conf"), "site { output { locale_mode = \"single_locale_root\" } }\n")
        _write(dir.resolve("concept/reserve-room.md"),
          """---
            |title: 会議室を予約する
            |brief: 会議室予約のユースケース。
            |scenario:
            |  type: use-case
            |  id: UC-ROOM-RESERVE
            |  primary_actor: 社員
            |  goal: 会議室を予約する
            |status: published
            |published_at: 2026-06-24
            |---
            |
            |# UseCase
            |
            |## 会議室を予約する
            |""".stripMargin)
        _write(dir.resolve("concept/simple.dox"),
          """Simple Scenario
            |===============
            |
            |# HEAD
            |
            |scenario.type = "simple"
            |scenario.id = "SC-SIMPLE"
            |status = "published"
            |
            |# Steps
            |
            |- 知識を探す
            |""".stripMargin)

        When("SmartDox builds the site through the normal Dox parser path")
        val site = create_site(context, dir.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site_realm(site, context)
        implicit val i18ncontext: I18NContext = context.i18NContext
        val markdown = html_at(realm, "/ja/concept/reserve-room.html", "ja/concept/reserve-room.html")
        val smartdox = html_at(realm, "/ja/concept/simple.html", "ja/concept/simple.html")

        Then("SmartDox renders Markdown scenario source documents as ordinary pages")
        markdown should include_html("会議室を予約する")
        And("SmartDox renders SmartDox scenario source documents as ordinary pages")
        smartdox should include_html("Simple Scenario")
        And("SmartDox does not own scenario semantic extraction metadata")
        realm.getString("metadata/scenarios/scenarios.json") shouldBe None
      } finally {
        _delete(dir)
      }
    }

    "preserve multilingual head title" in {
      Given("a document head with English and Japanese title alternatives")
      val dox = Document(
        Head(metadata = org.smartdox.metadata.DocumentMetaData.empty.withTitle(List(
          Span.createEn("Literate Model Example: Address"),
          Span.createJa("文芸モデルの実例：住所")
        ))),
        Body(Nil)
      )
      val txctx = DoxSiteTransformer.Context(
        DoxSiteTransformer.Config(),
        context,
        TreeTransformer.Context.default[Node],
        context.doxContext
      )
      When("the automatic i18n transformer processes the document head")
      val transformed = Dox.transform(dox, new AutoI18nTransformer(txctx)).asInstanceOf[Document]
      implicit val jactx = context.targetI18NContext.withLocale(java.util.Locale.JAPANESE)

      Then("the default title remains available for fallback consumers")
      transformed should have_default_head_title("Literate Model Example: Address")
      And("the target locale title is selected for localized consumers")
      transformed should have_localized_head_title("文芸モデルの実例：住所")
    }
  }

  private def _write(path: java.nio.file.Path, content: String): Unit = {
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, content.getBytes(StandardCharsets.UTF_8))
  }

  private def _fragment_body(content: String, sourcepath: String, locale: String): String = {
    val json = parser.parse(content).toOption.get
    val fragments = json.hcursor.downField("fragments").as[Vector[io.circe.Json]].toOption.get
    fragments.flatMap { fragment =>
      val cursor = fragment.hcursor
      val source = cursor.downField("source_path").as[String].toOption
      val lang = cursor.downField("locale").as[String].toOption
      if (source.contains(sourcepath) && lang.contains(locale))
        cursor.downField("body_html").as[String].toOption
      else
        None
    }.head
  }

  private def _delete(path: java.nio.file.Path): Unit =
    if (Files.exists(path)) {
      val stream = Files.walk(path)
      try {
        import scala.collection.JavaConverters._
        stream.iterator.asScala.toVector.reverse.foreach(Files.deleteIfExists)
      } finally {
        stream.close()
      }
    }

}
