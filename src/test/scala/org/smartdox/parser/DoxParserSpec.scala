package org.smartdox.parser

import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/*
 * @since   Dec. 24, 2011
 * @version Dec. 24, 2011
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxParserSpec extends WordSpec with ShouldMatchers {
  "Dox" should {
    "" that {
      "" in {
        val result = DoxParser.parseOrgmode("* OK")
        result should be ('successful)
        println(result.getClass)
      }
    }
  }
}