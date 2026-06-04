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
 * @version Jun.  5, 2026
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
      implicit val i18nContext: I18NContext = context.i18NContext
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
        implicit val i18nContext: I18NContext = context.i18NContext
        val html = realm.getString("/ja/manual/index.html").orElse(realm.getString("ja/manual/index.html")).get

        html should include ("Runtime is a manual operation word")
        html should not include ("class=\"glossary\"")
        html should not include ("glossary/architecture/runtime")
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
