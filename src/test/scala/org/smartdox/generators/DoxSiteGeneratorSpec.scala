package org.smartdox.generators

import scalaz._, Scalaz._
import java.io.File
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.realm.Realm
import org.smartdox.parser.UseDoxParser
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator._

/*
 * @since   Mar.  2, 2025
 *  version Mar. 11, 2025
 *  version May.  2, 2025
 *  version Jun.  8, 2025
 *  version Aug. 16, 2025
 * @version May. 13, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxSiteGeneratorSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDoxParser {
  "DoxSiteGenerator" should {
    val env = Environment.createJaJp()
    val cliconfig = CliConfig.buildJaJp()
    val config = Config(cliconfig)
    val ctx = new Context(env, config, env.contextFoundation)
    "typical" which {
      "mini" ignore {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default)
        val r = g.generate(in)
        println(r.print)
      }
      "plain" ignore {
        val in = Realm.create(new File("src/test/resources/site1"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default)
        val r = g.generate(in)
        println(r.print)
      }
      "dfn" ignore {
        val in = Realm.create(new File("src/test/resources/site-dfn"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default)
        val r = g.generate(in)
        println(r.print)
      }
    }
    "publish.d" which {
      val publish = Some(new File("src/test/resources/publish-fixture"))

      "copies raw metadata into doxsite root" in {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default, publish)
        val r = g.generate(in)
        r.get("doxsite.d/catalog/projects/textus-tutorial.json") should not be empty
        r.get("doxsite.d/samples/textus-tutorial/metadata.json") should not be empty
        r.get("doxsite.d/repository/artifacts/textus-core.json") should not be empty
      }

      "generates antora pages from publication paths and repository fallback" in {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new AntoraGenerator(ctx, DoxSite.Config.default, publish)
        val r = g.generate(in)
        r.get("antora.d/en/docs/samples/modules/textus/pages/tutorial/index.adoc") should not be empty
        r.get("antora.d/ja/docs/samples/modules/textus/pages/tutorial/source-manifest.adoc") should not be empty
        r.get("antora.d/en/docs/repository/modules/textus-core/pages/index.adoc") should not be empty
        r.get("antora.d/en/docs/repository/modules/textus-core/pages/releases.adoc") should not be empty
      }

      "fails on invalid metadata syntax" in {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new AntoraGenerator(ctx, DoxSite.Config.default, Some(new File("src/test/resources/publish-invalid-fixture")))
        intercept[IllegalArgumentException] {
          g.generate(in)
        }.getMessage should include ("broken.json")
      }

      "fails on invalid publication path" in {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new AntoraGenerator(ctx, DoxSite.Config.default, Some(new File("src/test/resources/publish-invalid-path-fixture")))
        intercept[IllegalArgumentException] {
          g.generate(in)
        }.getMessage should include ("publication.path")
      }
    }
  }
}
