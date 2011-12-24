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
 * @version Dec. 24, 2011
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