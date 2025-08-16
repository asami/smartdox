package org.smartdox.service.operations

import scalaz._, Scalaz._
import java.io.File
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.cli.Request
import org.goldenport.realm.Realm
import org.smartdox.parser.UseDoxParser
import org.smartdox.service.operations._

/*
 * @since   Mar.  9, 2025
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SiteOperationClassSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDoxParser {
  "DoxSiteGenerator" should {
    val env = Environment.createJaJp()
    "plain" which {
      "plain" in {
        val req = Request.create(SiteOperationClass.specification, Array("src/test/resources/site1"))
        val res = SiteOperationClass.apply(env, req)
        println(res)
      }
    }
  }
}
