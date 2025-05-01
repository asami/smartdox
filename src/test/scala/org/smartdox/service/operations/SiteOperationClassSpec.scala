package org.smartdox.service.operations

import scalaz._, Scalaz._
import java.io.File
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.cli.Request
import org.goldenport.realm.Realm
import org.smartdox.parser.UseDoxParser
import org.smartdox.service.operations._

/*
 * @since   Mar.  9, 2025
 * @version Mar.  9, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SiteOperationClassSpec extends WordSpec with Matchers with ScalazMatchers with UseDoxParser {
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
