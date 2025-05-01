package org.smartdox.parser

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers

/*
 * @since   Oct. 14, 2018
 *  version Nov. 12, 2018
 *  version Dec. 31, 2018
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class Dox2ParserSpec extends WordSpec with Matchers with ScalazMatchers with UseDox2Parser {
  "Foundation" should {
    "simple" which {
    //   val in = "* OK"
    //   val out = "<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>"
    //   "plain" in {
    //     parse_orgmode(in, out)
    //   }
    //   // "scalaz" in {
    //   //   parse_orgmode_z(in, out)
    //   // }
    //   // "short" in {
    //   //   parse_orgmode("* OK", out)
    //   // }
    // }
    // "nest" which {
    //   "first/second" in {
    //     parse_orgmode("* First\n** Second\n",
    //         "<!DOCTYPE html><html><head/><body><section><h2>First</h2><section><h3>Second</h3></section></section></body></html>")
    //   }
    //   "first,contents/second,contents" in { // TODO bold option
    //     parse_orgmode("* First\n1st contents.\n** Second\n2nd *contents*.\n",
    //         "<!DOCTYPE html><html><head/><body><section><h2>First</h2><p>1st contents.</p><section><h3>Second</h3><p>2nd *contents*.</p></section></section></body></html>")
    //   }
    }
    "ul" which {
      "typical" in {
        // parse_orgmode("* First\n - first\n - second\n - third\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ul><li>first</li><li>second</li><li>third</li></ul></section></body></html>")
      }
      "nest" in {
        // parse_orgmode("* First\n - first\n  - first.first\n  - first.second\n - second\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ul><li>first<ul><li>first.first</li><li>first.second</li></ul></li><li>second</li></ul></section></body></html>")
      }
      "typical 2" in {
        parse_orgmode_simple_debug("- One\n- Two\n",
          """<ul><li>One</li><li>Two</li></ul>""")
      }
      "continue" in {
        parse_orgmode_simple_debug("- This is \n a pen.\n",
                             """<ul><li>This is a pen.</li></ul>""")
      }
      "continue 2" in {
        parse_orgmode_simple("- One\n - Two\n Two-One\n",
                             """<ul><li>One<ul><li>Two Two-One</li></ul></li></ul>""")
      }
      "continue 2 xx" in {
        parse_orgmode_simple_debug("abc\n\n- One\n - Two\n - Three\n\nxyz",
          """<p>abc</p><ul><li>One<ul><li>Two</li><li>Three</li></ul></li></ul><p>xyz</p>""")
      }
      "continue 2 x" in {
        parse_orgmode_simple_debug("- One\n - Two\n - Three\n",
          """<ul><li>One<ul><li>Two</li><li>Three</li></ul></li></ul>""")
      }
    }
    "ol" which {
      "typical" in {
        // parse_orgmode("* First\n 1. first\n 2. second\n 3. third\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ol><li>first</li><li>second</li><li>third</li></ol></section></body></html>")
      }
      "nest" in {
        // parse_orgmode("* First\n 1. first\n  1. first.first\n  2. first.second\n 2. second\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ol><li>first<ol><li>first.first</li><li>first.second</li></ol></li><li>second</li></ol></section></body></html>")
      }
    }
    "dl" which {
      "typical" in {
        // parse_orgmode("* First\n - first :: one\n - second :: two\n - third :: three\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><dl><dt>first</dt><dd>one</dd><dt>second</dt><dd>two</dd><dt>third</dt><dd>three</dd></dl></section></body></html>")
      }
    }
    "inline" which {
      "typical" in {
        // parse_orgmode_full("* First\n pre *bold* /italic/ _underline_ =code= ~pre~ +del+ post\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><p>pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post</p></section></body></html>")
      }
    }
    "inline xml" which {
      "typical" in {
        // parse_orgmode("* First\n pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><p>pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post</p></section></body></html>")
      }
      "= in code" in {
        // parse_orgmode_simple("""<code>(b >= 0).option(b.toString)</code>""",
        //     """<p><code>(b &gt;= 0).option(b.toString)</code></p>""")
      }
    }
//     "structure" which {
//       "empty" in {
//         parse_orgmode("",
//             "<!DOCTYPE html><html><head/><body/></html>")
//       }
//       "simple in top" in {
//         parse_orgmode("Hello SmartDox",
//             "<!DOCTYPE html><html><head/><body><p>Hello SmartDox</p></body></html>")
//       }
//       "multi line in top" in {
//         parse_orgmode("Hello\nSmartDox\n",
//             "<!DOCTYPE html><html><head/><body><p>Hello SmartDox</p></body></html>")
//       }
//       "auto title" in { FUTURE
//         parse_orgmode_auto_title("Hello SmartDox",
//             "<!DOCTYPE html><html><head><title>Hello SmartDox</title></head><body/></html>")
//       }
//       "auto title and body" in { FUTURE
//         // "Hello\nSmartDox\n"の場合の動きがorg-modeと異なるが、仕様としておく。
//         parse_orgmode_auto_title("Hello\n\nSmartDox\n",
//             "<!DOCTYPE html><html><head><title>Hello</title></head><body><p>SmartDox</p></body></html>")
//       }
//     }
//     "hyperlink" which {
//       "typical" in {
//         parse_orgmode("[[http://www.yahoo.com/][Yahoo]]",
//             """<!DOCTYPE html><html><head/><body><p><a href="http://www.yahoo.com/">Yahoo</a></p></body></html>""")
//       }
//       "simple" in {
//         parse_orgmode("[[http://www.yahoo.com/]]",
//             """<!DOCTYPE html><html><head/><body><p><a href="http://www.yahoo.com/">http://www.yahoo.com/</a></p></body></html>""")
//       }
//       "xml" in {
//         parse_orgmode("""<a href="http://www.yahoo.com/">Yahoo</a>""",
//             """<!DOCTYPE html><html><head/><body><p><a href="http://www.yahoo.com/">Yahoo</a></p></body></html>""")
//       }
//       "not hyperlink" in {
//         parse_orgmode("""[not link]""",
//             """<!DOCTYPE html><html><head/><body><p>[not link]</p></body></html>""")
//       }
//       "implicit hyperlink" in {
//         parse_orgmode("""http://www.yahoo.com/""",
//             """<!DOCTYPE html><html><head/><body><p><a href="http://www.yahoo.com/">http://www.yahoo.com/</a></p></body></html>""")
//       }
//     }
  }
  "Table" should {
    "table" which {
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
#+LABEL: tablelabel
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
      val resultcaption = """<table id="tablelabel"><caption>Title</caption><thead><tr><th>h1</th><th>h2</th><th>h3</th></tr></thead><tbody><tr><td>one</td><td>two</td><td>three</td></tr><tr><td>four</td><td>five</td><td>six</td></tr></tbody><tfoot><tr><td>sum1</td><td>sum2</td><td>sum3</td></tr></tfoot></table>"""
      // "typical" in {
      //   parse_orgmode_simple(tabletypical, result)
      // }
      // "simple" in {
      //   parse_orgmode_simple(tablesimple, result)
      // }
      // "simple2" in {
      //   parse_orgmode_simple(tablesimple2, result)
      // }
      // "multi" in {
      //   parse_orgmode_simple(tablemulti, resultmulti)
      // }
      // "header" in {
      //   parse_orgmode_simple(tableheadertypical, resultheader)
      // }
      // "headersimple" in {
      //   parse_orgmode_simple(tableheadersimple, resultheader)
      // }
      // "footer" in {
      //   parse_orgmode_simple(tablefootertypical, resultfooter)
      // }
      // "footersimple" in {
      //   parse_orgmode_simple(tablefootersimple, resultfooter)
      // }
      // "caption" in {
      //   parse_orgmode_simple(tablecaption, resultcaption)
      // }
//       "missing closing / and ] in table" in { future
//         parse_orgmode_simple("""| [[http://example.com/][Some/None] |""",
//             """<table><tbody><tr><td><a href="http://example.com/">Some/None</a></td></tr></tbody></table>""")
//       }
//       "external csv" in { future
//         parse_orgmode_simple("#+table: \"test.csv\" src\n",
//                              """<ttable uri="test.csv" src="true"/>""")
//       }
    }
  }
  val imgdot = "#+begin_dot image/simple.png\nDOT\n#+end_dot\n"
  "Image" should {
    "img" which {
      // "typical" in {
      //   parse_orgmode_simple("[[image/simple.png]]", """<p><img src="image/simple.png"/></p>""")
      // }
//       "embedded dot" in { future
//         parse_orgmode_simple(imgdot, """<p><img src="image/simple.png"/></p>""")
//       }
//       "embedded ditaa" in { future
//         parse_orgmode_simple("#+begin_ditaa image/simple.png\nDITAA\n#+end_ditaa\n", """<p><img src="image/simple.png"/></p>""")
//       }
//       "first,contents/second,contents" in { future
//         parse_orgmode_simple("* First\n1st contents.\n#+begin_dot image/simple.png\nDOT\n#+end_dot\n1st cont.\n** Second\n2nd *contents*.\n\ncont.\n* Next First\none\n\ntwo\n",
//             """<section><h2>First</h2><p>1st contents.<img src="image/simple.png"/>1st cont.</p><section><h3>Second</h3><p>2nd <b>contents</b>.</p><p>cont.</p></section></section><section><h2>Next First</h2><p>one</p><p>two</p></section>""")
//       }
    }
//     "figure" which {
//       val figure = """#+CAPTION: Figure
// #+LABEL: fig
// [[image/simple.png]]"""
//       val result = """<figure id="fig"><img src="image/simple.png"/><figcaption>Figure</figcaption></figure>"""
//       "typical" in {
//         parse_orgmode_simple(figure, result)
//       }
//       val figuredot = """#+CAPTION: Figure
// #+LABEL: fig
// """ + imgdot
//       "typical dot" in {
//         parse_orgmode_simple(figuredot, result)
//       }
//       "first,contents/second,contents" in {
//         parse_orgmode_simple("* First\n1st contents.\n#+CAPTION: Figure\n#+LABEL: fig\n#+begin_dot image/simple.png\nDOT\n#+end_dot\n1st cont.\n** Second\n2nd *contents*.\n\ncont.\n* Next First\none\n\ntwo\n",
//             """<section><h2>First</h2><p>1st contents.</p>%s<p>1st cont.</p><section><h3>Second</h3><p>2nd <b>contents</b>.</p><p>cont.</p></section></section><section><h2>Next First</h2><p>one</p><p>two</p></section>""".format(result))
//       }
//     }
  // }
  // "Paragraph" should {
  //   "prologue" which {
  //     "one paragraph" in {
  //       parse_orgmode_simple("First.\n",
  //           """<p>First.</p>""")
  //     }
  //     "three paragraphs" in {
  //       parse_orgmode_simple("First.\n\nSecond.\n\nThird.\n",
  //           """<p>First.</p><p>Second.</p><p>Third.</p>""")
  //     }
  //     "duplicate empty lines" in {
  //       parse_orgmode_simple("\nFirst.\n\n\nSecond.\n\n\nThird.\n\n",
  //           """<p>First.</p><p>Second.</p><p>Third.</p>""")
  //     }
  //   }
  //   "inside sections" which {
  //     "first,contents/second,contents" in {
  //       parse_orgmode_simple("* First\n1st contents.\n\ncont.\n** Second\n2nd *contents*.\n\ncont.\n* Next First\none\n\ntwo\n",
  //           """<section><h2>First</h2><p>1st contents.</p><p>cont.</p><section><h3>Second</h3><p>2nd <b>contents</b>.</p><p>cont.</p></section></section><section><h2>Next First</h2><p>one</p><p>two</p></section>""")
  //     }
  //   }
  // }
//   "Comment" should {
//     "comment #" in {
//       parse_orgmode_simple("abc\n#def\nghi",
//             """<p>abc ghi</p>""")
//     }
//     "comment section" in {
//       parse_orgmode_simple("* COMMENT abc\ndef\nghi\n* jkl",
//             """<section><h2>jkl</h2></section>""")
//     }
//     "comment block" in {
//       parse_orgmode_simple("abc\n#+BEGIN_COMMENT\ndef\n#+END_COMMENT\nghi\n",
//             """<p>abc ghi</p>""")
//     }
//     "comment block lowercase" in {
//       parse_orgmode_simple("abc\n#+begin_comment\ndef\n#+end_comment\nghi\n",
//             """<p>abc ghi</p>""")
//     }
//     "file:abc.png" in {
//       parse_orgmode_simple("file:abc.png",
//             """<p><img src="abc.png"/></p>""")
//     }
//     "file:abc.doc" in {
//       parse_orgmode_simple("file:abc.doc",
//             """<p><a href="abc.doc">abc.doc</a></p>""")
//     }
//     "<tt>" in {
//       parse_orgmode_simple("""<tt>abc</tt>""",
//             """<p><tt>abc</tt></p>""")
//     }
//   }
//   "Literal" should {
//     "<[ ]>" in {
//       parse_orgmode_simple("""<[<*>]>""",
//             """<p>&lt;*&gt;</p>""")
//     }
//     "<[ ]> with /" in {
//       parse_orgmode_simple("""<[/a/b/c]>""",
//             """<p>/a/b/c</p>""")
//     }
//     "<t>" in {
//       parse_orgmode_simple("""<t>*span*</t>""",
//             """<p>*span*</p>""")
//     }
//     "<span>" in {
//       parse_orgmode_simple("""<span>*span*</span>""",
//             """<p><span><b>span</b></span></p>""")
//     }
  }
//   "Terse" should {
//     "typical" which {
//       "underscoe" in {
//         parse_orgmode("register_provisional",
//           "<!DOCTYPE html><html><head/><body><p>register _provisional</p></body></html>"
//         )
//       }
//     }
//   }
}
