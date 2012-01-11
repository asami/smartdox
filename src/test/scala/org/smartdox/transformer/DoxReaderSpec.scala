package org.smartdox.transformer

import scalaz._
import Scalaz._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalaz.ScalazMatchers
import org.smartdox.parser.DoxParser
import org.smartdox.Dox
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Reader

/*
 * @since   Jan. 11, 2012
 * @version Jan. 12, 2012
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxReaderSpec extends WordSpec with ShouldMatchers with ScalazMatchers {
  "DoxReader" should {
    "one section" that {
      val in = "* OK"
      val out = "List(<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>, <head/>, <body><section><h2>OK</h2></section></body>, <section><h2>OK</h2></section>)"
      "plain" in {
        val reader = parse_orgmode(in, out)
        val elements = reader_elements(reader)
        println("elements = " + elements)
        elements.toString should be (out)
      }
    }
    "two sections" that {
      val in = "* One\none\n* Two\ntwo\n"
      val out = "List(<!DOCTYPE html><html><head/><body><section><h2>One</h2><p>one</p></section><section><h2>Two</h2><p>two</p></section></body></html>, <head/>, <body><section><h2>One</h2><p>one</p></section><section><h2>Two</h2><p>two</p></section></body>, <section><h2>One</h2><p>one</p></section>, <p>one</p>, one, <section><h2>Two</h2><p>two</p></section>, <p>two</p>, two)"
      "plain" in {
        val reader = parse_orgmode(in, out)
        val elements = reader_elements(reader)
        println("elements = " + elements)
        elements.toString should be (out)
      }
    }
  }
  
  def parse_orgmode(in: String, out: String) = {
    val result = DoxParser.parseOrgmode(in)
    result should be ('successful)
    val dox = result.get
    new DoxReader(dox)
  }

  def reader_elements(reader: DoxReader): List[Dox] = {
    val buf = new ArrayBuffer[Dox]
    var r: Reader[Dox] = reader
    while (!r.atEnd) {
      buf += r.first
      r = r.rest
    }
    buf.toList
  }
}
