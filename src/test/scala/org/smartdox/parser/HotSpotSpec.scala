package org.smartdox.parser

import scalaz._
import Scalaz._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.goldenport.scalatest.ScalazMatchers

/**
 * @since   Jan. 27, 2012
 * @version Jan. 28, 2012
 * @author  ASAMI, Tomoharu
 */
class HotSpotSpec extends WordSpec with ShouldMatchers with ScalazMatchers with UseDoxParser {
  "0.2.2" should {
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
/*
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
