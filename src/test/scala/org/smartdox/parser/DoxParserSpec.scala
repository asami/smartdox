package org.smartdox.parser

import scalaz._
import Scalaz._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalaz.ScalazMatchers

/*
 * @since   Dec. 24, 2011
 * @version Dec. 31, 2011
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxParserSpec extends WordSpec with ShouldMatchers with ScalazMatchers {
  "Foundation" should {
    "simple" that {
      val in = "* OK"
      val out = "<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>"
      "plain" in {
        parse_orgmode(in, out)
      }
      "scalaz" in {
        parse_orgmode_z(in, out)
      }
      "short" in {
        parse_orgmode("* OK", out)
      }
    }
    "nest" that {
      "first/second" in {
        parse_orgmode("* First\n** Second\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2><section><h3>Second</h3></section></section></body></html>")
      }
      "first,contents/second,contents" in {
        parse_orgmode("* First\n1st contents.\n** Second\n2nd *contents*.\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2>1st contents.<section><h3>Second</h3>2nd <b>contents</b>.</section></section></body></html>")
      }
    }
    "ul" that {
      "typical" in {
        parse_orgmode("* First\n - first\n - second\n - third\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ul><li>first</li><li>second</li><li>third</li></ul></section></body></html>")
      }
      "nest" in {
        parse_orgmode("* First\n - first\n  - first.first\n  - first.second\n - second\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ul><li>first<ul><li>first.first</li><li>first.second</li></ul></li><li>second</li></ul></section></body></html>")
      }
    }
    "ol" that {
      "typical" in {
        parse_orgmode("* First\n 1. first\n 2. second\n 3. third\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ol><li>first</li><li>second</li><li>third</li></ol></section></body></html>")
      }
      "nest" in {
        parse_orgmode("* First\n 1. first\n  1. first.first\n  2. first.second\n 2. second\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ol><li>first<ol><li>first.first</li><li>first.second</li></ol></li><li>second</li></ol></section></body></html>")
      }
    }
    "dl" that {
      "typical" in {
        parse_orgmode("* First\n - first :: one\n - second :: two\n - third :: three\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2><dl><dt>first</dt><dd>one</dd><dt>second</dt><dd>two</dd><dt>third</dt><dd>three</dd></dl></section></body></html>")
      }
    }
    "inline" that {
      "typical" in {
        parse_orgmode("* First\n pre *bold* /italic/ _underline_ =code= ~pre~ +del+ post\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2> pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post</section></body></html>")
      }
    }
    "inline xml" that {
      "typical" in {
        parse_orgmode("* First\n pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2> pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post</section></body></html>")
      }
    }
    "structure" that {
      "empty" in {
        parse_orgmode("",
            "<!DOCTYPE html><html><head/><body/></html>")
      }
      "simple" in {
        parse_orgmode("Hello SmartDox",
            "<!DOCTYPE html><html><head/><body>Hello SmartDox</body></html>")
      }
    }
    "hyperlink" that {
      "typical" in {
        parse_orgmode("[[http://www.yahoo.com/][Yahoo]]",
            """<!DOCTYPE html><html><head/><body><a href="http://www.yahoo.com/">Yahoo</a></body></html>""")
      }
      "simple" in {
        parse_orgmode("[[http://www.yahoo.com/]]",
            """<!DOCTYPE html><html><head/><body><a href="http://www.yahoo.com/">http://www.yahoo.com/</a></body></html>""")
      }
      "xml" in {
        parse_orgmode("""<a href="http://www.yahoo.com/">Yahoo</a>""",
            """<!DOCTYPE html><html><head/><body><a href="http://www.yahoo.com/">Yahoo</a></body></html>""")
      }
    }
  }
  "Table" should {
    "table" that {
      val tabletypical = """|------
| one | two | three |
|----"""
      val tablesimple = """| one | two | three |"""
      val tablesimple2 = """| one | two | three """
      val tablemulti = """|------
| one | two | three |
| four | five | six |        
|----"""
      val tableheadertypical = """|-----
|h1|h2|h3|
|----
|one | two |three |
|four| five | six|
|---"""
      val tableheadersimple = """|h1|h2|h3|
|----
|one | two |three |
|four| five | six|"""
      val tablefootertypical = """|-----
|h1|h2|h3|
|----
|one | two |three |
|four| five | six|
|---
| sum1 |sum2 | sum3|
|---"""
      val tablefootersimple = """|h1|h2|h3|
|----
|one | two |three |
|four| five | six|
|---
| sum1 |sum2 | sum3|"""
      val tablecaption = """#+CAPTION: Title
#+LABEL: title
#+ATTR_HTML: width=80%
#+ATTR_LATEX: width=15cm
|-----
|h1|h2|h3|
|----
|one | two |three |
|four| five | six|
|---
| sum1 |sum2 | sum3|
|---"""
      val result = "<table><tbody><tr><td>one</td><td>two</td><td>three</td></tr></tbody></table>"
      val resultmulti = "<table><tbody><tr><td>one</td><td>two</td><td>three</td></tr><tr><td>four</td><td>five</td><td>six</td></tr></tbody></table>"
      val resultheader = "<table><thead><tr><th>h1</th><th>h2</th><th>h3</th></tr></thead><tbody><tr><td>one</td><td>two</td><td>three</td></tr><tr><td>four</td><td>five</td><td>six</td></tr></tbody></table>"
      val resultfooter = "<table><thead><tr><th>h1</th><th>h2</th><th>h3</th></tr></thead><tbody><tr><td>one</td><td>two</td><td>three</td></tr><tr><td>four</td><td>five</td><td>six</td></tr></tbody><tfoot><tr><td>sum1</td><td>sum2</td><td>sum3</td></tr></tfoot></table>"
      val resultcaption = "<table><caption>Title</caption><thead><tr><th>h1</th><th>h2</th><th>h3</th></tr></thead><tbody><tr><td>one</td><td>two</td><td>three</td></tr><tr><td>four</td><td>five</td><td>six</td></tr></tbody><tfoot><tr><td>sum1</td><td>sum2</td><td>sum3</td></tr></tfoot></table>"
      "typical" in {
        parse_orgmode_simple(tabletypical, result)
      }
      "simple" in {
        parse_orgmode_simple(tablesimple, result)
      }
      "simple2" in {
        parse_orgmode_simple(tablesimple2, result)
      }
      "multi" in {
        parse_orgmode_simple(tablemulti, resultmulti)
      }
      "header" in {
        parse_orgmode_simple(tableheadertypical, resultheader)
      }
      "headersimple" in {
        parse_orgmode_simple(tableheadersimple, resultheader)
      }
      "footer" in {
        parse_orgmode_simple(tablefootertypical, resultfooter)
      }
      "footersimple" in {
        parse_orgmode_simple(tablefootersimple, resultfooter)
      }
      "caption" in {
        parse_orgmode_simple(tablecaption, resultcaption)
      }
    }
  }
  "Image" should {
    "img" that {
      "typical" in {
        parse_orgmode_simple("[[image/simple.png]]", """<img src="image/simple.png"/>""")
      }
    }
    "figure" that {
      val figure = """#+CAPTION: Figure
#+LABEL: fig
[[image/simple.png]]"""
      "typical" in {
        parse_orgmode_simple(figure, """<figure id="fig"><img src="image/simple.png"/><figcaption>Figure</figcaption></figure>""")
      }
    }
  }

  def parse_orgmode_simple(in: String, out: String) {
    parse_orgmode(in, 
        """<!DOCTYPE html><html><head/><body>%s</body></html>""".format(out))
  }

  def parse_orgmode(in: String, out: String) {
    val result = DoxParser.parseOrgmode(in)
    result should be ('successful)
    val dox = result.get
    dox.toString() should be (out)
  }

  def parse_orgmode_z(in: String, out: String) {
    val result = DoxParser.parseOrgmodeZ(in)
    result should success
    result match {
      case Success(dox) => dox.toString should be (out)
    }
  }
}