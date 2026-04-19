package org.smartdox.parser

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scalaz._, Scalaz._
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._

/*
 * @since   Nov. 29, 2020
 *  version Aug. 16, 2025
 * @version Apr. 19, 2026
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

    "parse pass inline macro as raw contents" in {
      val r = DoxInlineParser.parse(DoxInlineParser.Config.smartdox, "pass:[^[A-Z]{2}$]")
      r shouldBe a [InlineMacro]
      val macroNode = r.asInstanceOf[InlineMacro]
      macroNode.name shouldBe "pass"
      macroNode.contents shouldBe "^[A-Z]{2}$"
      macroNode.toText shouldBe "^[A-Z]{2}$"
      macroNode.toData shouldBe "pass:[^[A-Z]{2}$]"
    }

    "parse site inline macro as migration target" in {
      val r = DoxInlineParser.parse(DoxInlineParser.Config.smartdox, "site:[overview]")
      r shouldBe a [InlineMacro]
      val macroNode = r.asInstanceOf[InlineMacro]
      macroNode.name shouldBe "site"
      macroNode.contents shouldBe "overview"
      macroNode.toData shouldBe "site:[overview]"
    }

    "warn legacy single bracket site link" in {
      val buffer = new ByteArrayOutputStream()
      val r = scala.Console.withErr(new PrintStream(buffer)) {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, "[overview.dox]")
      }
      buffer.toString("UTF-8") should include (
        "warning: Deprecated SmartDox site link '[overview.dox]'. Use 'site:[overview.dox]' instead."
      )
      r shouldBe Hyperlink(Vector(Text("overview.dox")), "overview.dox")
    }

    "keep markdown link label without legacy site link warning" in {
      val buffer = new ByteArrayOutputStream()
      val r = scala.Console.withErr(new PrintStream(buffer)) {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, "[overview](overview.dox)")
      }
      buffer.toString("UTF-8") should not include "Deprecated SmartDox site link"
      r shouldBe a [Hyperlink]
      val link = r.asInstanceOf[Hyperlink]
      link.href.toString shouldBe "overview.dox"
      link.contents shouldBe Vector(Text("overview"))
    }

    "keep asciidoc link label without legacy site link warning" in {
      val buffer = new ByteArrayOutputStream()
      scala.Console.withErr(new PrintStream(buffer)) {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, "link:overview.dox[Overview]")
      }
      buffer.toString("UTF-8") should not include "Deprecated SmartDox site link"
    }

    "keep quoted bracket text without legacy site link warning" in {
      val buffer = new ByteArrayOutputStream()
      val r = scala.Console.withErr(new PrintStream(buffer)) {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, """["SimpleEntity"]""")
      }
      buffer.toString("UTF-8") should not include "Deprecated SmartDox site link"
      r shouldBe Text("""[&quot;SimpleEntity&quot;]""")
    }
  }
}
