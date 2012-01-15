package org.smartdox

import scalaz._
import Scalaz._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalaz.ScalazMatchers
import org.smartdox.parser.DoxParser
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Reader

/*
 * @since   Jan. 12, 2012
 * @version Jan. 13, 2012
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxSpec extends WordSpec with ShouldMatchers with ScalazMatchers {
  "Dox" should {
    "tree" that {
      val in = "* OK"
      val out = "List(<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>, <head/>, <body><section><h2>OK</h2></section></body>, <section><h2>OK</h2></section>)"
      "plain" in {
        val result = DoxParser.parseOrgmode(in)
        val t = Dox.tree(result.get)
        println("tree = " + t.drawTree(showA))
      }
    }
  }
}
