package org.smartdox.parser

import scalaz._, Scalaz._
import org.scalatest.Matchers
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._

/*
 * @since   Oct. 14, 2018
 *  version Nov. 11, 2018
 * @version Dec. 24, 2018
 * @author  ASAMI, Tomoharu
 */
trait UseDox2Parser extends Matchers with ScalazMatchers {
  protected def parse_orgmode_simple(in: String, out: String): Dox = {
    parse_orgmode(
      in,
      s"""<!DOCTYPE html><html><head/><body>$out</body></html>"""
    )
  }

  protected def parse_orgmode(in: String, out: String): Dox = {
    val result = Dox2Parser.parse(in)
    // result should be ('successful)
    val dox = result
    dox.toString() should be (out)
    dox
  }

  protected def parse_orgmode_full(in: String, out: String): Dox = {
    val c = Dox2Parser.Config.orgmodeInline
    val result = Dox2Parser.parse(c, in)
    val dox = result
    dox.toString() should be (out)
    dox
  }

  protected def parse_orgmode_auto_title(in: String, out: String): Dox = {
    ???
  }
}
