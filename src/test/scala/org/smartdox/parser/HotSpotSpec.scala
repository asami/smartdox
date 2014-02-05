package org.smartdox.parser

import scalaz._
import Scalaz._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.goldenport.scalatest.ScalazMatchers

/*
 * @since   Jan. 27, 2012
 *  version Sep. 15, 2012
 *  version Oct. 19, 2012
 *  version Nov. 23, 2012
 * @version Feb.  5, 2014
 * @author  ASAMI, Tomoharu
 */
class HotSpotSpec extends WordSpec with Matchers with ScalazMatchers with UseDoxParser {
  "0.3.2" should {
    "includedoc" which {
      "includedoc" in {
        parse_orgmode_simple("""#+include: "abc.org"""",
                             """<includedoc filename="abc.org"/>""")
      }
    }
  }
  "0.3.1" should {
    "table" which {
      "* in table" in {
        parse_orgmode_simple("""| one | 10 * 20 | three |""",
                             """<table><tbody><tr><td>one</td><td>10 * 20</td><td>three</td></tr></tbody></table>""")
      }
/*
      "_ in table" in {
        parse_orgmode_simple("""| one | column_id | three |""",
                             """<table><tbody><tr><td>one</td><td>column<sub>id</sub></td><td>three</td></tr></tbody></table>""")
      }
    }
    "adornment" which {
      "sub" in {
        parse_orgmode_simple("""column_id""",
                             """column<sub>id</sub>""")
      }
      "sup" in {
        parse_orgmode_simple("""column_{id}""",
                             """column<sup>id</sup>""")
      }
    }
    "html literal" which {
      "not xml" in {
        parse_orgmode_simple("See under below.\n#+begin_html\n<ul><li>abc</ul>\n#+end_html\nShee above.\n",
                             """<ul><li>One<ul><li>Two Two-One</li></ul></li></ul>""")
      }
    }
    "div" which {
      "quote" in {
        parse_orgmode_simple("See under below.\n#+begin_quote\nThis is the definition\n#+end_quote\nShee above.\n",
                             """<ul><li>One<ul><li>Two Two-One</li></ul></li></ul>""")
      }
      "verse" in {
        parse_orgmode_simple("See under below.\n#+begin_verse\nThis is the definition\n#+end_verse\nShee above.\n",
                             """<ul><li>One<ul><li>Two Two-One</li></ul></li></ul>""")
      }
      "center" in {
        parse_orgmode_simple("See under below.\n#+begin_center\nThis is the definition\n#+end_center\nShee above.\n",
                             """<ul><li>One<ul><li>Two Two-One</li></ul></li></ul>""")
      }
*/
    }
  }
/*
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
    "<: :>" in {
      parse_orgmode_simple("""<[<*>]>""",
            """<p>&lt;*&gt;</p>""")
    }
    "<: :> with /" in {
      parse_orgmode_simple("""<[/a/b/c]>""",
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
*/
/* Spec candidates
    "<< >>" in {
      parse_orgmode_simple("""<<*>>""",
            """<p>&lt;*&gt;</p>""")
    }
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
}

object HotSpot {
  def main(args: Array[String]) {
    org.scalatest.tools.Runner.main(Array("-s", "org.smartdox.parser.HotSpotSpec"))
  }
}
