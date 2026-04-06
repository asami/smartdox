package org.smartdox.doxsite

import scalaz._, Scalaz._

import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import java.io.File
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
 * @version Aug. 16, 2025
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
}
