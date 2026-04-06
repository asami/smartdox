package org.smartdox.parser

import scalaz._, Scalaz._
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._

/*
 * @since   Nov. 29, 2020
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxInlineParserSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDoxParser {
  "plain" should {
    "simple" in {
      val r = DoxInlineParser.parse("特性一覧")
      println(r)
    }

    "parse markdown link with bilingual label" in {
      val r = DoxInlineParser.parse("[Literate Model｜文芸モデル](/Users/asami/src/dev2025/simplemodeling-org/src/main/doxsite/literate-modeling/what-is-literate-model.dox)")
      r shouldBe a [Hyperlink]
      val link = r.asInstanceOf[Hyperlink]
      link.href.toString shouldBe "/Users/asami/src/dev2025/simplemodeling-org/src/main/doxsite/literate-modeling/what-is-literate-model.dox"
      link.contents should have size 1
      link.contents.head shouldBe a [Text]
      link.contents.head.asInstanceOf[Text].contents shouldBe "Literate Model｜文芸モデル"
    }
  }
}
