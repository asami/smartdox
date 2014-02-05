package org.smartdox.parser

import scalaz._
import Scalaz._
import org.scalatest.Matchers
import org.goldenport.scalatest.ScalazMatchers

/**
 * @since   Jan. 27, 2012
 * @version Feb.  5, 2014
 * @author  ASAMI, Tomoharu
 */
trait UseDoxParser extends Matchers with ScalazMatchers {
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

  def parse_orgmode_auto_title(in: String, out: String) {
    val result = DoxParser.parseOrgmodeAutoTitle(in)
    result should be ('successful)
    val dox = result.get
    dox.toString() should be (out)
  }

  def parse_orgmode_z(in: String, out: String) {
    val result = DoxParser.parseOrgmodeZ(in)
    result should success
    result match {
      case Success(dox) => dox.toString should be (out)
      case Failure(_) => ???
    }
  }
}
