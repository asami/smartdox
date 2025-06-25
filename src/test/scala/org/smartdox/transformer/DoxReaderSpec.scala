package org.smartdox.transformer

import scalaz._
import Scalaz._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox.parser.UseDoxParser
import org.smartdox.Dox
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Reader

/*
 * @since   Jan. 11, 2012
 *  version Feb.  5, 2014
 * @version Jun. 17, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxReaderSpec extends WordSpec with Matchers with ScalazMatchers with UseDoxParser {
  "DoxReader" should {
    "one section" which {
      val in = "* OK"
      val out = "List(<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>, <head/>, <body><section><h2>OK</h2></section></body>, <section><h2>OK</h2></section>)"
      "plain" in {
        val reader = parse_orgmode_reader(in, out)
        val elements = reader_elements(reader)
        println("elements = " + elements)
        elements.toString should be (out)
      }
    }
    "two sections" which {
      val in = "* One\n\none\n\n* Two\n\ntwo\n"
      val out = "List(<!DOCTYPE html><html><head/><body><section><h2>One</h2><p>one</p></section><section><h2>Two</h2><p>two</p></section></body></html>, <head/>, <body><section><h2>One</h2><p>one</p></section><section><h2>Two</h2><p>two</p></section></body>, <section><h2>One</h2><p>one</p></section>, <p>one</p>, one, <section><h2>Two</h2><p>two</p></section>, <p>two</p>, two)"
      "plain" in {
        val reader = parse_orgmode_reader(in, out)
        val elements = reader_elements(reader)
        println("elements = " + elements)
        elements.toString should be (out)
      }
    }
  }
  
  def parse_orgmode_reader(in: String, out: String) = {
    val dox = parse_document(in)
    println(s"X: $dox")
    new DoxReader(dox)
  }

  // def parse_orgmode(in: String, out: String) = {
  //   val result = DoxParser.parseOrgmode(in)
  //   result should be ('successful)
  //   val dox = result.get
  //   new DoxReader(dox)
  // }

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
