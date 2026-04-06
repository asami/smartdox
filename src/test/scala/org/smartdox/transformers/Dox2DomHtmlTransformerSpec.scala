package org.smartdox.transformers

import java.util.Locale
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.smartdox._
import org.smartdox.generator.Context

@RunWith(classOf[JUnitRunner])
class Dox2DomHtmlTransformerSpec extends AnyWordSpec with Matchers {
  "Dox2DomHtmlTransformer" should {
    "use localized document title" in {
      val context = Context.create().withTargetI18NContext(Locale.JAPANESE)
      val dox = Document(
        Head(metadata = org.smartdox.metadata.DocumentMetaData.empty.withTitle(List(
          Span.createEn("Literate Model Example: Address"),
          Span.createJa("文芸モデルの実例：住所")
        ))),
        Body(Nil)
      )
      val dom = new Dox2DomHtmlTransformer(context, Dox2DomHtmlTransformer.Rule.empty).documentOut(dox)
      val title = dom.getElementsByTagName("title").item(0).getTextContent
      title shouldBe "文芸モデルの実例：住所"
    }
  }
}
