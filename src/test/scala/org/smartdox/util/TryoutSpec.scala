package org.smartdox.util

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox.parser.{DoxParser, UseDoxParser}
import org.smartdox.Dox

/*
 * @since   Feb.  4, 2019
 * @version Feb.  6, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with ScalazMatchers with UseDoxParser {
  // "TechDocTransformer" should {
  //   "top" in {
  //     val in = "_OK_"
  //     val out = """<!DOCTYPE html><html><head/><body><h1 class="title"/><p>_OK_</p></body></html>"""
  //     val a = parse_document_full(in)
  //     println(s"TechDocTransformerSpec in: $a")
  //     val b = TechDocTransformer().transform(a)
  //     println(s"TechDocTransformerSpec out: $b")
  //     b.toString() should be (out)
  //   }
  // }
}
