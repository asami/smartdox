package org.smartdox.converters

import scalaz._, Scalaz._
import java.io.File
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceMatchers
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.realm.Realm
import org.smartdox.parser.UseDox2Parser
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator._

/*
 * @since   Jul.  5, 2025
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class CategorySpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDox2Parser with ConsequenceMatchers {
  "Category" when {
    "Decode" should {
      "typical" in {
      }
    }
  }
}
