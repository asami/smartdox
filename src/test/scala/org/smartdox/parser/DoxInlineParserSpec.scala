package org.smartdox.parser

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scalaz._, Scalaz._
import org.scalatest.GivenWhenThen
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._

/*
 * @since   Nov. 29, 2020
 *  version Aug. 16, 2025
 *  version Apr. 20, 2026
 * @version Aug. 21, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxInlineParserSpec extends AnyWordSpec with Matchers with ScalazMatchers with GivenWhenThen with UseDoxParser {
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

    "parse site inline macro as internal link" in {
      val r = DoxInlineParser.parse(DoxInlineParser.Config.smartdox, "site:[overview.dox]")
      r shouldBe Hyperlink(Vector(Text("overview.dox")), "overview.dox")
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

    "parse back quoted angle brackets as code inside span" in {
      val r = DoxInlineParser.parse(
        DoxInlineParser.Config.smartdox,
        """<span lang="ja">`minimal.main.hello` は `<component>.<service>.<operation>` の形です。</span>"""
      )
      val codes = _collect_code_text(r)
      codes should contain ("minimal.main.hello")
      codes should contain ("<component>.<service>.<operation>")
    }

    "keep inline tag outside back quote inside span" in {
      val r = DoxInlineParser.parse(
        DoxInlineParser.Config.smartdox,
        """<span lang="ja">This is <i>important</i>.</span>"""
      )
      _contains_italic(r) shouldBe true
      _collect_code_text(r) shouldBe Nil
    }
  }

  "generic inline tags" should {
    "preserve a terminal boolean attribute as an empty-string value" in {
      Given("a generic span with a terminal boolean attribute and text content")
      val source = "<span enabled>text</span>"

      When("the inline parser reads the source")
      val (_, result, _) = DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)

      Then("the supported span retains the boolean attribute and its contents")
      val nodes = result.toOption.get
      nodes should have size 1
      val span = nodes.head.asInstanceOf[Span]
      span.attributes.list shouldBe List("enabled" -> "")
      span.contents shouldBe List(Text("text"))
    }

    "preserve quoted and terminal boolean attributes in source order" in {
      Given("a generic span with a quoted attribute followed by a boolean attribute")
      val source = "<span lang=\"ja\" enabled>text</span>"

      When("the inline parser reads the source")
      val (_, result, _) = DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)

      Then("both attributes are retained in their authored order")
      val nodes = result.toOption.get
      nodes should have size 1
      val span = nodes.head.asInstanceOf[Span]
      span.attributes.list shouldBe List("lang" -> "ja", "enabled" -> "")
      span.contents shouldBe List(Text("text"))
    }

    "create empty supported nodes for generic self-closing forms" in {
      Given("generic spans in empty and boolean self-closing forms")
      val sources = Vector("<span/>", "<span enabled/>")

      When("the inline parser reads both sources")
      val results = sources.map { source =>
        val (_, result, _) = DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)
        result.toOption.get
      }
      val recovered = results.map { nodes =>
        nodes.flatMap(_nodes).collect { case m: Span => m }
      }

      Then("each form produces one empty span without requiring a close tag")
      recovered should have size 2
      recovered.foreach { spans =>
        spans should have size 1
        spans.head shouldBe a [Span]
        spans.head.contents shouldBe Nil
      }
      recovered.head.head.attributes.list shouldBe Nil
      recovered(1).head.attributes.list shouldBe List("enabled" -> "")
    }

    "preserve standalone self-closing nodes through the public parse method" in {
      Given("standalone generic spans in empty and boolean self-closing forms")
      val sources = Vector("<span/>", "<span enabled/>")

      When("the public inline parser parses each source")
      val parsed = sources.map(source => DoxInlineParser.parse(DoxInlineParser.Config.smartdox, source))

      Then("both supported spans remain present with empty contents and authored attributes")
      parsed should have size 2
      parsed.foreach { dox =>
        dox shouldBe a [Span]
        dox.asInstanceOf[Span].contents shouldBe Nil
      }
      parsed.head.asInstanceOf[Span].attributes.list shouldBe Nil
      parsed(1).asInstanceOf[Span].attributes.list shouldBe List("enabled" -> "")
    }

    "retain nested boolean and self-closing generic nodes in authored order" in {
      Given("a generic parent containing a terminal boolean child and self-closing children")
      val source = "<span>before<span enabled>flag</span><span/><span enabled/>after</span>"

      When("the public inline parser parses the nested generic source")
      val parsed = DoxInlineParser.parse(DoxInlineParser.Config.smartdox, source)

      Then("the parent retains each nested node, empty contents, and source order")
      parsed shouldBe a [Span]
      val parent = parsed.asInstanceOf[Span]
      parent.contents should have size 5
      parent.contents(0) shouldBe Text("before")
      parent.contents(1) shouldBe a [Span]
      val booleanchild = parent.contents(1).asInstanceOf[Span]
      booleanchild.attributes.list shouldBe List("enabled" -> "")
      booleanchild.contents shouldBe List(Text("flag"))
      parent.contents(2) shouldBe a [Span]
      parent.contents(2).asInstanceOf[Span].contents shouldBe Nil
      parent.contents(3) shouldBe a [Span]
      val selfclosingbooleanchild = parent.contents(3).asInstanceOf[Span]
      selfclosingbooleanchild.attributes.list shouldBe List("enabled" -> "")
      selfclosingbooleanchild.contents shouldBe Nil
      parent.contents(4) shouldBe Text("after")
    }

    "retain self-closing nodes and surrounding text in authored order" in {
      Given("ordinary text surrounding empty and boolean self-closing spans")
      val source = "before<span/>middle<span enabled/>after"

      When("the inline parser reads the mixed result-flow source")
      val (_, result, _) = DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)
      val nodes = result.toOption.get.flatMap {
        case m: Fragment => m.contents
        case m => Vector(m)
      }

      Then("the parser retains both spans and all surrounding text in source order")
      nodes should have size 5
      nodes(0) shouldBe Text("before")
      nodes(1) shouldBe a [Span]
      nodes(1).asInstanceOf[Span].attributes.list shouldBe Nil
      nodes(1).asInstanceOf[Span].contents shouldBe Nil
      nodes(2) shouldBe Text("middle")
      nodes(3) shouldBe a [Span]
      nodes(3).asInstanceOf[Span].attributes.list shouldBe List("enabled" -> "")
      nodes(3).asInstanceOf[Span].contents shouldBe Nil
      nodes(4) shouldBe Text("after")
    }

    "reject a slash that is not immediately followed by a closing angle bracket" in {
      Given("a self-closing span whose slash is followed by whitespace")
      val source = "<span enabled/ >"

      When("the inline parser reads the malformed source")
      val error = intercept[org.goldenport.exception.NoReachDefectException] {
        DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)
      }

      Then("the existing deterministic parser failure boundary is retained")
      error shouldBe a [org.goldenport.exception.NoReachDefectException]
    }
  }

  private def _collect_code_text(dox: Dox): List[String] = {
    val own = dox match {
      case m: Code => List(m.contents.map(_.toText).mkString)
      case _ => Nil
    }
    own ++ dox.elements.toList.flatMap(_collect_code_text)
  }

  private def _contains_italic(dox: Dox): Boolean =
    dox.isInstanceOf[Italic] || dox.elements.exists(_contains_italic)

  private def _nodes(dox: Dox): Vector[Dox] =
    dox +: dox.elements.toVector.flatMap(_nodes)
}
