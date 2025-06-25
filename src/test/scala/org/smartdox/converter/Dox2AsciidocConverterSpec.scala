package org.smartdox.converters

import scalaz._, Scalaz._
import java.io.File
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.context.test.ConsequenceMatchers
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.realm.Realm
import org.smartdox.parser.UseDox2Parser
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator._

/*
 * @since   Jun. 20, 2025
 * @version Jun. 20, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class Dox2AsciidocConverterSpec extends WordSpec with Matchers with ScalazMatchers with UseDox2Parser with ConsequenceMatchers {
  val context = Context.create()
  "Dox2AsciidocConverter" when {
    "Ul" should {
      "three" in {
        val c = new Dox2AsciidocConverter(context)
        val dox = parse_dox("""- X
- Y
- Z
""")
        val s = c.transform(dox)
        s should be_success("""* X
* Y
* Z
""")
      }
    }
  }
}
