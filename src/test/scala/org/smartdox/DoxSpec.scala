package org.smartdox

import scalaz._, Scalaz._, Show._, Tree._
import org.goldenport.Z._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox.parser.DoxParser
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Reader

/*
 * @since   Jan. 12, 2012
 * @version Feb.  5, 2014
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxSpec extends WordSpec with Matchers with ScalazMatchers {
  val in = "* OK"
  val in2 = "* Hello\nworld"
  val out = "List(<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>, <head/>, <body><section><h2>OK</h2></section></body>, <section><h2>OK</h2></section>)"
  "Dox" should {
    "provides scalaz tree" which {
      "plain" in {
        val d = DoxParser.parseOrgmode(in)
        val t = Dox.tree(d.get)
        println("tree = " + t.drawTree(showA))
        val d2 = Dox.untree(t)
        println("dox = " + d2)
      }
      "replaceShallow" in {
        println("replaceShallow")
        val d = DoxParser.parseOrgmode(in2)
        val t = Dox.tree(d.get)
        println("tree = " + t.drawTree(showA))
        val t2 = replaceShallow(t) {
          case (t: Text, _) => (Bold, Stream(leaf(t)))
        }
        println("tree2 = " + t2.drawTree(showA))
        val d2 = Dox.untree(t2)
        println("dox2 = " + d2)
      }
      "replace Section" in {
        println("replace Section")
        val d = DoxParser.parseOrgmode(in2)
        val t = Dox.tree(d.get)
        println("tree = " + t.drawTree(showA))
        val t2 = replaceShallow(t) {
          case (t: Section, cs) => (Div, cs)          
        }
        println("tree2 = " + t2.drawTree(showA))
        val d2 = Dox.untree(t2)
        println("dox2 = " + d2)
      }
    }
/*
    "provides scalaz lens" which {
      "plain" in {
        val d = DoxParser.parseOrgmode(in)
        val d2 = Dox.treeLens.mod(d.get, x => x)
        println("dox = " + d2)
      }
      "leaf" in {
        val d = DoxParser.parseOrgmode(in2)
        val d2 = Dox.treeLens.mod(d.get, replaceShallow(_) {
          case (t: Text, _) => (Bold, Stream(t.leaf))
        })
        println("dox = " + d2)
      }
    }
 */
  }
}
