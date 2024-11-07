package org.smartdox.parser

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers

/*
 * @since   Oct. 23, 2024
 *  version Oct. 31, 2024
 * @version Nov.  2, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxMarkdownParserSpec extends WordSpec with Matchers with ScalazMatchers with UseDox2Parser {
  "Foundation" should {
    "simple" which {
      "plain" in {
        val in = "# OK"
        val out = "<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>"
        parse_markdown(in, out)
      }
      "Title" in {
        val in = """OK
==
"""
        val out = "<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>"
        parse_markdown(in, out)
      }
      "Title and Section" in {
        val in = """Title
=====

# Section

This is
a pen."""
        val out = "<!DOCTYPE html><html><head/><body><section><h2>Title</h2></section><section><h2>Section</h2><p>This is a pen.</p></section></body></html>"
        parse_markdown(in, out)
      }
    }
  }
  "Table" should {
    "ghf" which {
      "plain" in {
        val in = """| foo | bar |
| --- | --- |
| baz | bim |"""
        val out = """<table><thead><tr><th>foo</th><th>bar</th></tr></thead><tbody><tr><td>baz</td><td>bim</td></tr></tbody></table>"""
        parse_markdown_simple(in, out)
      }
      "align" in {
        val in = """| foo | bar | boo |
| :-- | :-: | --: |
| baz | bim | bom |"""
        val out = """<table><colgroup><col align="left"/><col align="center"/><col align="right"/></colgroup><thead><tr><th>foo</th><th>bar</th><th>boo</th></tr></thead><tbody><tr><td>baz</td><td>bim</td><td>bom</td></tr></tbody></table>"""
        parse_markdown_simple(in, out)
      }
    }
  }
  "UL" should {
    "plain" which {
      "plain" in {
        val in = """- abc"""
        val out = """<ul><li>abc</li></ul>"""
        parse_markdown_simple(in, out)
      }
      "2 lines" in {
        val in = """- abc
- xyz"""
        val out = """<ul><li>abc</li><li>xyz</li></ul>"""
        parse_markdown_simple(in, out)
      }
      "in section" in {
        val in = """# A

- abc
"""
        val out = """<section><h2>A</h2><ul><li>abc</li></ul></section>"""
        parse_markdown_simple(in, out)
      }
    }
  }
  "OL" should {
    "plain" which {
      "plain" in {
        val in = """1. abc"""
        val out = """<ol><li>abc</li></ol>"""
        parse_markdown_simple(in, out)
      }
      "2 lines" in {
        val in = """1. abc
2. xyz"""
        val out = """<ol><li>abc</li><li>xyz</li></ol>"""
        parse_markdown_simple(in, out)
      }
      "in section" in {
        val in = """# A

1. abc
"""
        val out = """<section><h2>A</h2><ol><li>abc</li></ol></section>"""
        parse_markdown_simple(in, out)
      }
    }
  }
  "DD" should {
    "plain" which {
      "plain" in {
        val in = """- a :: b"""
        val out = """<dl><dt>a</dt><dd>b</dd></dl>"""
        parse_markdown_simple(in, out)
      }
      "2 lines" in {
        val in = """- a :: b
- x :: y
"""
        val out = """<dl><dt>a</dt><dd>b</dd><dt>x</dt><dd>y</dd></dl>"""
        parse_markdown_simple(in, out)
      }
      "in section" in {
        val in = """# A

- a :: b
"""
        val out = """<section><h2>A</h2><dl><dt>a</dt><dd>b</dd></dl></section>"""
        parse_markdown_simple(in, out)
      }
    }
  }
}
