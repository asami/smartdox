package org.smartdox.parser

import scalaz._, Scalaz._
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._

/*
 * @since   Nov. 29, 2020
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxInlineParserSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDoxParser {
  "plain" should {
    "simple" in {
      val r = DoxInlineParser.parse("特性一覧")
      println(r)
    }
  }
}
