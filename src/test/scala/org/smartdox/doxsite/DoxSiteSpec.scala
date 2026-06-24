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
class DoxSiteSpec extends AnyWordSpec with Matchers with GivenWhenThen with UseDoxParser {
  val context = Context.create()
  implicit val dctx = context.dateTimeContext
  "DoxSite" should {
    "create" in {
      val site = DoxSite.create(context, new File("src/test/resources/site1"))
      println(site)
    }

    "resolve site inline macro as internal link" in {
      val config = DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full)
      val site = DoxSite.create(context, new File("src/test/resources/site-link"), None, config)
      val realm = site.toRealm(context)
      implicit val i18ncontext: I18NContext = context.i18NContext
      val html = realm.getString("/en/index.html").orElse(realm.getString("en/index.html")).get

      html should include ("href=\"target.html\"")
      html should include ("Target Page")
      html should not include ("site:[target.dox]")
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
        fragments should include (""""source_path" : "index.dox"""")
        fragments should include (""""public_path" : "index.html"""")
        fragments should include (""""headline" : "Fragment headline."""")
        fragments should include (""""brief" : "Fragment brief."""")
        fragments should include (""""locale" : "ja"""")
        fragments should not include (""""locale" : "en"""")

        And("the body HTML is produced after site link and glossary processing")
        homebody should include ("""href="target.html"""")
        homebody should include ("""class="glossary"""")
        homebody should not include ("site:[target.dox]")

        And("manual fragments keep the existing glossary auto-link exclusion")
        manualbody should include ("Runtime is a manual operation word")
        manualbody should not include ("""class="glossary"""")

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
        homeonlyfragments should include (""""source_path" : "index.dox"""")
        homeonlyfragments should include (""""source_path" : "manual/index.dox"""")
      } finally {
        _delete(dir)
      }
    }

    "keep program text opaque during auto i18n" in {
      val dox = Document(Head(), Body(List(Program.create("alpha｜beta"))))
      val txctx = DoxSiteTransformer.Context(
        DoxSiteTransformer.Config(),
        context,
        TreeTransformer.Context.default[Node],
        context.doxContext
      )
      val transformed = Dox.transform(dox, new AutoI18nTransformer(txctx))
      Dox.toDox(Dox.toTree(transformed))
    }

    "keep auto i18n active in section title and list items" in {
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
      val transformed = Dox.transform(dox, new AutoI18nTransformer(txctx))

      transformed match {
        case document: Document =>
          val section = document.body.contents.head.asInstanceOf[Section]
          section.title should have size 2
          section.title.head shouldBe a [Span]
          section.contents.head shouldBe a [Ul]
          section.contents.head.asInstanceOf[Ul].contents.head.contents.head shouldBe a [Span]
        case other =>
          fail(s"Unexpected transformed dox: $other")
      }
    }


    "skip automatic glossary links in Manual pages" in {
      val dir = Files.createTempDirectory("smartdox-manual-glossary-skip")
      try {
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
        val site = DoxSite.create(context, dir.toFile, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
        val realm = site.toRealm(context)
        implicit val i18ncontext: I18NContext = context.i18NContext
        val html = realm.getString("/ja/manual/index.html").orElse(realm.getString("ja/manual/index.html")).get

        html should include ("Runtime is a manual operation word")
        html should not include ("class=\"glossary\"")
        html should not include ("glossary/architecture/runtime")
      } finally {
        _delete(dir)
      }
    }

    "keep published dox pages in production strategy" in {
      val dir = Files.createTempDirectory("smartdox-production-published")
      try {
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
        val site = DoxSite.create(context, dir.toFile, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site.toRealm(context)
        implicit val i18ncontext: I18NContext = context.i18NContext

        realm.getString("/ja/published.html").orElse(realm.getString("ja/published.html")).get should include ("Published body.")
        realm.getString("/ja/wip.html").orElse(realm.getString("ja/wip.html")) shouldBe None
      } finally {
        _delete(dir)
      }
    }

    "parse Markdown pages through Markdown mode and YAML front matter metadata" in {
      val dir = Files.createTempDirectory("smartdox-markdown-front-matter")
      try {
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
        val site = DoxSite.create(context, dir.toFile, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site.toRealm(context)
        implicit val i18ncontext: I18NContext = context.i18NContext
        val html = realm.getString("/ja/published.html").orElse(realm.getString("ja/published.html")).get

        html should include ("Published Markdown Body")
        html should include ("Markdown body with")
        html should include ("https://example.com")
        realm.getString("/ja/wip.html").orElse(realm.getString("ja/wip.html")) shouldBe None
      } finally {
        _delete(dir)
      }
    }

    "use Markdown SmartDox HEAD metadata for site strategy" in {
      val dir = Files.createTempDirectory("smartdox-markdown-head-metadata")
      try {
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
        val site = DoxSite.create(context, dir.toFile, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site.toRealm(context)
        implicit val i18ncontext: I18NContext = context.i18NContext

        realm.getString("/ja/published.html").orElse(realm.getString("ja/published.html")).get should include ("Published markdown body.")
        realm.getString("/ja/wip.html").orElse(realm.getString("ja/wip.html")) shouldBe None
      } finally {
        _delete(dir)
      }
    }

    "emit Markdown glossary term metadata through Dox IR" in {
      val dir = Files.createTempDirectory("smartdox-markdown-glossary-term")
      try {
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
        val site = DoxSite.create(context, dir.toFile, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site.toRealm(context)
        implicit val i18ncontext: I18NContext = context.i18NContext
        val terms = realm.getString("metadata/glossary/terms.json").get

        terms should include (""""id" : "architecture:runtime"""")
        terms should include (""""title" : "Runtime"""")
        terms should include (""""reading" : "らんたいむ"""")
        terms should include ("Runtime summary from Markdown front matter.")
        terms should include ("\"source_path\" : \"glossary/architecture/runtime.md\"")
        terms should include ("Runtime definition from Markdown.")
      } finally {
        _delete(dir)
      }
    }

    "bibliography/reference source knowledge" which {
      "emit bibliography metadata with inline citations and RDF source refs" in {
        val dir = Files.createTempDirectory("smartdox-bibliography-metadata")
        try {
          Given("a site with bibliography entries for a book and a web reference")
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

          When("SmartDox builds BoK site metadata")
          val site = DoxSite.create(context, dir.toFile, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
          val realm = site.toRealm(context)
          implicit val i18ncontext: I18NContext = context.i18NContext
          val bibliography = realm.getString("metadata/bibliography/bibliography.json").get
          val ttl = realm.getString("site.ttl").get

          Then("bibliography entries are generated deterministically from the bibliography source tree")
          bibliography should include ("\"id\" : \"bib:design-patterns\"")
          bibliography should include ("\"entry_type\" : \"book\"")
          bibliography should include ("\"category\" : \"concept\"")
          bibliography should include ("\"isbn\" : \"9780201633610\"")
          bibliography should include ("\"key\" : \"gamma1995designpatterns\"")
          bibliography should include ("\"id\" : \"bib:crossref-api\"")
          bibliography should include ("\"entry_type\" : \"web-page\"")
          bibliography should include ("\"source_url\" : \"https://api.crossref.org\"")
          bibliography should include ("\"id\" : \"doi:10.5555/unresolved-reference\"")
          bibliography should include ("\"id\" : \"openlibrary:works/OL31219436W\"")
          bibliography should include ("\"source_refs\"")
          bibliography should include ("\"citation_key\" : \"gamma1995designpatterns\"")
          bibliography should include ("\"citation_key\" : \"fowler1999refactoring\"")
          bibliography should include ("\"public_path\" : \"technology/inline-bibliography.html\"")
          bibliography should not include ("\"id\" : \"gamma1995designpatterns\"")
          bibliography should include ("\"id\" : \"bib:fowler1999refactoring\"")
          bibliography should include ("\"source_kind\" : \"bibtex-only\"")
          bibliography should include ("Refactoring: {Improving} the Design of Existing Code")
          bibliography should include ("\"isbn\" : \"9780201485677\"")
          bibliography.indexOf("bib:design-patterns") should be < bibliography.indexOf("bib:crossref-api")
          "\"id\" : \"bib:design-patterns\"".r.findAllIn(bibliography).size shouldBe 1
          bibliography should include ("\"source_path\" : \"bibliography/concept/design-patterns.bib.dox\"")
          bibliography should not include ("BibTeX Shadow Design Patterns")

          And("article citation links are represented in the site RDF graph")
          ttl should include ("technology/inline-bibliography")
          ttl should include ("https://schema.org/citation")
          ttl should include ("http://purl.org/dc/terms/references")

          And("article pages render inline bibliography citations and a References section")
          val article = realm.getString("/ja/technology/inline-bibliography.html").
            orElse(realm.getString("ja/technology/inline-bibliography.html")).
            orElse(realm.getString("/en/technology/inline-bibliography.html")).
            orElse(realm.getString("en/technology/inline-bibliography.html")).
            get
          article should include ("bibliography-citation")
          article should include ("[Gamma et al. 1994]")
          article should include ("[Fowler 1999]")
          article should include ("Bibliography")

          And("bibliography pages are represented in the site RDF graph")
          ttl should include ("bibliography/concept/design-patterns")
          ttl should include ("BibliographicResource")
          ttl should include ("isbn:9780201633610")
          ttl should include ("Gamma et al. Design Patterns.")
          ttl should include ("object-oriented design")
          ttl should include ("https://api.crossref.org")
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
        val site = DoxSite.create(context, dir.toFile, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
        val realm = site.toRealm(context)
        implicit val i18ncontext: I18NContext = context.i18NContext
        val markdown = realm.getString("/ja/concept/reserve-room.html").orElse(realm.getString("ja/concept/reserve-room.html")).get
        val smartdox = realm.getString("/ja/concept/simple.html").orElse(realm.getString("ja/concept/simple.html")).get

        Then("SmartDox renders the documents but does not emit scenario semantic metadata")
        markdown should include ("会議室を予約する")
        smartdox should include ("Simple Scenario")
        realm.getString("metadata/scenarios/scenarios.json") shouldBe None
      } finally {
        _delete(dir)
      }
    }

    "preserve multilingual head title" in {
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
      val transformed = Dox.transform(dox, new AutoI18nTransformer(txctx)).asInstanceOf[Document]
      implicit val jactx = context.targetI18NContext.withLocale(java.util.Locale.JAPANESE)
      transformed.head.distillTitleStringDefault shouldBe Some("Literate Model Example: Address")
      transformed.head.distillTitleString shouldBe Some("文芸モデルの実例：住所")
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
