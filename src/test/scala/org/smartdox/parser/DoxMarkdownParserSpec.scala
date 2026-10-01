package org.smartdox.parser

import scalaz._, Scalaz._
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers

/*
 * @since   Oct. 23, 2024
 *  version Oct. 31, 2024
 *  version Nov.  2, 2024
 *  version Aug. 16, 2025
 * @version Oct.  1, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxMarkdownParserSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDox2Parser with GivenWhenThen {
  "Foundation" should {
    "simple" which {
      "plain" in {
        Given("an Enhanced ATX heading with the established SmartDox heading level")
        val in = "# OK"
        val out = "<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>"
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established document HTML output is retained")
        result.toString shouldBe out
      }
      "Title" in {
        Given("an Enhanced Setext heading with the established document output")
        val in = """OK
==
"""
        val out = "<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>"
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established document HTML output is retained")
        result.toString shouldBe out
      }
      "Title and Section" in {
        Given("an Enhanced Setext title followed by an ATX section and a multiline paragraph")
        val in = """Title
=====

# Section

This is
a pen."""
        val out = "<!DOCTYPE html><html><head/><body><section><h2>Title</h2></section><section><h2>Section</h2><p>This is a pen.</p></section></body></html>"
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established document HTML output is retained")
        result.toString shouldBe out
      }
    }
  }
  "Table" should {
    "ghf" which {
      "plain" in {
        Given("an Enhanced pipe table with header and body rows")
        val in = """| foo | bar |
| --- | --- |
| baz | bim |"""
        val out = """<table><thead><tr><th>foo</th><th>bar</th></tr></thead><tbody><tr><td>baz</td><td>bim</td></tr></tbody></table>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
      "align" in {
        Given("an Enhanced pipe table with left, center and right aligned columns")
        val in = """| foo | bar | boo |
| :-- | :-: | --: |
| baz | bim | bom |"""
        val out = """<table><colgroup><col align="left"/><col align="center"/><col align="right"/></colgroup><thead><tr><th>foo</th><th>bar</th><th>boo</th></tr></thead><tbody><tr><td>baz</td><td>bim</td><td>bom</td></tr></tbody></table>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
    }
  }
  "UL" should {
    "plain" which {
      "plain" in {
        Given("an Enhanced tight unordered list with one item")
        val in = """- abc"""
        val out = """<ul><li>abc</li></ul>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
      "2 lines" in {
        Given("an Enhanced tight unordered list with two items")
        val in = """- abc
- xyz"""
        val out = """<ul><li>abc</li><li>xyz</li></ul>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
      "in section" in {
        Given("an Enhanced unordered list inside an ATX section")
        val in = """# A

- abc
"""
        val out = """<section><h2>A</h2><ul><li>abc</li></ul></section>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
    }
  }
  "OL" should {
    "plain" which {
      "plain" in {
        Given("an Enhanced tight ordered list with one item")
        val in = """1. abc"""
        val out = """<ol><li>abc</li></ol>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
      "2 lines" in {
        Given("an Enhanced tight ordered list with two items")
        val in = """1. abc
2. xyz"""
        val out = """<ol><li>abc</li><li>xyz</li></ol>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
      "in section" in {
        Given("an Enhanced ordered list inside an ATX section")
        val in = """# A

1. abc
"""
        val out = """<section><h2>A</h2><ol><li>abc</li></ol></section>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
    }
  }
  "DD" should {
    "plain" which {
      "plain" in {
        Given("an Enhanced legacy definition list with one term and definition")
        val in = """- a :: b"""
        val out = """<dl><dt>a</dt><dd>b</dd></dl>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
      "2 lines" in {
        Given("an Enhanced legacy definition list with two term/definition pairs")
        val in = """- a :: b
- x :: y
"""
        val out = """<dl><dt>a</dt><dd>b</dd><dt>x</dt><dd>y</dd></dl>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
      "in section" in {
        Given("an Enhanced legacy definition list inside an ATX section")
        val in = """# A

- a :: b
"""
        val out = """<section><h2>A</h2><dl><dt>a</dt><dd>b</dd></dl></section>"""
        When("the source is explicitly interpreted as Enhanced Markdown")
        val result = _enhanced(in)
        Then("the established compact HTML output is retained")
        result.toString shouldBe s"<!DOCTYPE html><html><head/><body>$out</body></html>"
      }
    }
  }
  private def _enhanced(in: String): org.smartdox.Dox =
    Dox2Parser.parse(Dox2Parser.Config.enhancedMarkdown, in)
}
