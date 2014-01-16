package org.smartdox

import scalaz._
import Scalaz._
import org.goldenport.Z._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox.parser.DoxParser
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Reader

/*
 * @since   Jan. 16, 2014
 * @version Jan. 16, 2014
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxesSpec extends WordSpec with ShouldMatchers with ScalazMatchers with Doxes {
  "Doxes" should {
    "dox_desc" that {
      "empty" in {
        val desc = dox_desc()
        println("Doxes#doc_desc = " + desc)
        desc should not be (null)
      }
    }
  }
}
