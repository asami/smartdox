package org.smartdox.generators

import scalaz._, Scalaz._
import java.io.File
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.realm.Realm
import org.smartdox.parser.UseDoxParser
import org.smartdox.generator._

/*
 * @since   Mar.  2, 2025
 *  version Mar. 11, 2025
 * @version May.  2, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxSiteGeneratorSpec extends WordSpec with Matchers with ScalazMatchers with UseDoxParser {
  "DoxSiteGenerator" should {
    val env = Environment.createJaJp()
    val cliconfig = CliConfig.buildJaJp()
    val config = Config(cliconfig)
    val ctx = new Context(env, config, env.contextFoundation)
    "typical" which {
      "mini" ignore {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new DoxSiteGenerator(ctx)
        val r = g.generate(in)
        println(r.print)
      }
      "plain" ignore {
        val in = Realm.create(new File("src/test/resources/site1"))
        val g = new DoxSiteGenerator(ctx)
        val r = g.generate(in)
        println(r.print)
      }
      "dfn" ignore {
        val in = Realm.create(new File("src/test/resources/site-dfn"))
        val g = new DoxSiteGenerator(ctx)
        val r = g.generate(in)
        println(r.print)
      }
    }
  }
}
