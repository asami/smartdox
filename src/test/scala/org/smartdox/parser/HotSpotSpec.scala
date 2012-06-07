package org.smartdox.parser

import scalaz._
import Scalaz._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.goldenport.scalatest.ScalazMatchers

/**
 * @since   Jan. 27, 2012
 * @version Jun.  5, 2012
 * @author  ASAMI, Tomoharu
 */
class HotSpotSpec extends WordSpec with ShouldMatchers with ScalazMatchers with UseDoxParser {
  // comment # and subtree
  // space in dl
  // file:abc.png
  // "/", "~", "<", ">"
  // <t>, <text>, <tt> 導入
  "0.2.5" should {
    "comment #" in {
      parse_orgmode_simple("abc\n#def\nghi",
            """<p>abc ghi</p>""")
    }
    "comment section" in {
      parse_orgmode_simple("* COMMENT abc\ndef\nghi\n* jkl",
            """<section><h2>jkl</h2></section>""")
    }
    "comment block" in {
      parse_orgmode_simple("abc\n#+BEGIN_COMMENT\ndef\n#+END_COMMENT\nghi\n",
            """<p>abc ghi</p>""")
    }
    "comment block lowercase" in {
      parse_orgmode_simple("abc\n#+begin_comment\ndef\n#+end_comment\nghi\n",
            """<p>abc ghi</p>""")
    }
    "file:abc.png" in {
      parse_orgmode_simple("file:abc.png",
            """<p><img src="abc.png"/></p>""")
    }
    "file:abc.doc" in {
      parse_orgmode_simple("file:abc.doc",
            """<p><a href="abc.doc">abc.doc</a></p>""")
    }
    "<tt>" in {
      parse_orgmode_simple("""<tt>abc</tt>""",
            """<p><tt>abc</tt></p>""")
    }
  }
  "0.2.4" should {
    "<< >>" in {
      parse_orgmode_simple("""<<*>>""",
            """<p>&lt;*&gt;</p>""")
    }
    "<: :>" in {
      parse_orgmode_simple("""<:<*>:>""",
            """<p>&lt;*&gt;</p>""")
    }
    "<: :> with /" in {
      parse_orgmode_simple("""<:/a/b/c:>""",
            """<p>/a/b/c</p>""")
    }
    "<t>" in {
      parse_orgmode_simple("""<t>*span*</t>""",
            """<p>*span*</p>""")
    }
    "<span>" in {
      parse_orgmode_simple("""<span>*span*</span>""",
            """<p><span><b>span</b></span></p>""")
    }
  }
/* Spec candidates
    "&lt;&amp;&gt;" in {
      parse_orgmode_simple("""&lt;&amp;&gt;""",
            """<p>&lt;&amp;&gt;</p>""")
    }
      "<t>" in {
        parse_orgmode_simple("""<t><*></t>""",
            """&lt;*&gt;""")
      }
      "<t> with /" in {
        parse_orgmode_simple("""<t>/a/b/c</t>""",
            """/a/b/c""")
      }
      "<tt>" in {
        parse_orgmode_simple("""<tt><*></tt>""",
            """<tt>&lt;*&gt;</tt>""")
      }
 */

/*
  "0.2.2" should {
      "missing closing / and ] in table" in {
        parse_orgmode_simple("""| [[http://example.com/][Some/None] |""",
            """<table><tbody><tr><td><a href="http://example.com/">Some/None</a></td></tr></tbody></table>""")
      }
      "= in code" in {
        parse_orgmode_simple("""<code>(b >= 0).option(b.toString)</code>""",
            """<p><code>(b &gt;= 0).option(b.toString)</code></p>""")
      }
      "not hyperlink left" in {
        parse_orgmode("""[""",
            """<!DOCTYPE html><html><head/><body><p>[</p></body></html>""")
      }
      "not hyperlink left word" in {
        parse_orgmode("""[not""",
            """<!DOCTYPE html><html><head/><body><p>[not</p></body></html>""")
      }
      "not hyperlink left word newline" in {
        parse_orgmode("[not\n",
            """<!DOCTYPE html><html><head/><body><p>[not</p></body></html>""")
      }
      "not hyperlink right" in {
        parse_orgmode("""]""",
            """<!DOCTYPE html><html><head/><body><p>]</p></body></html>""")
      }
      "not hyperlink" in {
        parse_orgmode("""[not link]""",
            """<!DOCTYPE html><html><head/><body><p>[not link]</p></body></html>""")
      }
      "implicit hyperlink" in {
        parse_orgmode("""http://www.yahoo.com/""",
            """<!DOCTYPE html><html><head/><body><p><a href="http://www.yahoo.com/">http://www.yahoo.com/</a></p></body></html>""")
      }
  }
  "0.2.1" should {
    "title" that {
      "auto title and body" in {
        parse_orgmode_auto_title("Hello\n\nSmartDox\n",
            "<!DOCTYPE html><html><head><title>Hello</title></head><body><p>SmartDox</p></body></html>")
      }
      "auto title" in {
        parse_orgmode_auto_title("Hello SmartDox",
            "<!DOCTYPE html><html><head><title>Hello SmartDox</title></head><body/></html>")
      }
      "multi line in top" in {
        parse_orgmode("Hello\nSmartDox\n",
            "<!DOCTYPE html><html><head/><body><p>Hello SmartDox</p></body></html>")
      }
      "first,contents/second,contents" in {
        parse_orgmode("* First\n1st contents.\n** Second\n2nd *contents*.\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2><p>1st contents.</p><section><h3>Second</h3><p>2nd <b>contents</b>.</p></section></section></body></html>")
      }
    }
    "inline" that {
      "typical" in {
        parse_orgmode("* First\n pre *bold* /italic/ _underline_ =code= ~pre~ +del+ post\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2><p> pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post</p></section></body></html>")
      }
    }
    "inline xml" that {
      "typical" in {
        parse_orgmode("* First\n pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post\n",
            "<!DOCTYPE html><html><head/><body><section><h2>First</h2><p> pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post</p></section></body></html>")
      }
    }
  }
*/
}

object HotSpot {
  def main(args: Array[String]) {
    org.scalatest.tools.Runner.main(Array("-s", "org.smartdox.parser.HotSpotSpec"))
  }
}
