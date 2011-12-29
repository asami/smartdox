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
 * @version Dec. 29, 2011
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxParserSpec extends WordSpec with ShouldMatchers with ScalazMatchers {
  "Dox" should {
    "simple" that {
      val in = "* OK"
      val out = "<!DOCTYPE html><html><head></head><body><section><h2>OK</h2></section></body></html>"
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
            "<!DOCTYPE html><html><head></head><body><section><h2>First</h2><section><h3>Second</h3></section></section></body></html>")
      }
      "first,contents/second,contents" in {
        parse_orgmode("* First\n1st contents.\n** Second\n2nd *contents*.\n",
            "<!DOCTYPE html><html><head></head><body><section><h2>First</h2>1st contents.<section><h3>Second</h3>2nd <b>contents</b>.</section></section></body></html>")
      }
    }
    "ul" that {
      "typical" in {
        parse_orgmode("* First\n - first\n - second\n - third\n",
            "<!DOCTYPE html><html><head></head><body><section><h2>First</h2><ul><li>first</li><li>second</li><li>third</li></ul></section></body></html>")
      }
      "nest" in {
        parse_orgmode("* First\n - first\n  - first.first\n  - first.second\n - second\n",
            "<!DOCTYPE html><html><head></head><body><section><h2>First</h2><ul><li>first<ul><li>first.first</li><li>first.second</li></ul></li><li>second</li></ul></section></body></html>")
      }
    }
    "ol" that {
      "typical" in {
        parse_orgmode("* First\n 1. first\n 2. second\n 3. third\n",
            "<!DOCTYPE html><html><head></head><body><section><h2>First</h2><ol><li>first</li><li>second</li><li>third</li></ol></section></body></html>")
      }
      "nest" in {
        parse_orgmode("* First\n 1. first\n  1. first.first\n  2. first.second\n 2. second\n",
            "<!DOCTYPE html><html><head></head><body><section><h2>First</h2><ol><li>first<ol><li>first.first</li><li>first.second</li></ol></li><li>second</li></ol></section></body></html>")
      }
    }
    "inline" that {
      "typical" in {
        parse_orgmode("* First\n pre *bold* /italic/ _underline_ =code= ~pre~ post\n",
            "<!DOCTYPE html><html><head></head><body><section><h2>First</h2> pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> post</section></body></html>")
      }
    }
    "inline xml" that {
      "typical" in {
        parse_orgmode("* First\n pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> post\n",
            "<!DOCTYPE html><html><head></head><body><section><h2>First</h2> pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> post</section></body></html>")
      }
    }
    "structure" that {
      "empty" in {
        parse_orgmode("",
            "<!DOCTYPE html><html><head></head><body></body></html>")
      }
      "simple" in {
        parse_orgmode("Hello SmartDox",
            "<!DOCTYPE html><html><head></head><body><section><h2>First</h2><ul><li>first</li><li>second</li><li>third</li></ul></section></body></html>")
      }
    }
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