package org.smartdox.doxsite

import scalaz._, Scalaz._

import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
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

/*
 * @since   Feb. 24, 2025
 *  version Feb. 28, 2025
 *  version Mar.  1, 2025
 *  version Apr.  3, 2025
 *  version Jun. 17, 2025
 *  version Aug. 16, 2025
 *  version Apr. 20, 2026
 *  version Jun.  8, 2026
 * @version Jun. 23, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxSiteSpec extends AnyWordSpec with Matchers with UseDoxParser {
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
