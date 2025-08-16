package org.smartdox.converters

import scalaz._, Scalaz._
import java.io.File
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner
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
 * @since   Jun. 20, 2025
 *  version Jul.  1, 2025
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class Dox2AsciidocConverterSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDox2Parser with ConsequenceMatchers {
  val context = Context.create()

  protected def make_asciidoc(s: String): Consequence[String] = {
    val c = new Dox2AsciidocConverter(context)
    val dox = parse_dox(s)
    c.convert(dox)
  }

  "Dox2AsciidocConverter" when {
    "Ul" should {
      "three" in {
        val c = new Dox2AsciidocConverter(context)
        val dox = parse_dox("""- X
- Y
- Z
""")
        val s = c.convert(dox)
        s should be_success("""* X
* Y
* Z
""")
      }
    }
    "Section" should {
      "One" in {
        val s = make_asciidoc("""A

# X

B
""")
        s should be_success("""A

= X

B
""")
      }
      "Ul" in {
        val s = make_asciidoc("""- A
B
  - M

# X
""")
        s should be_success("""* A B
** M

= X
""")
      }
    }
    "Ol" should {
      "nest with Ul" in {
        val s = make_asciidoc("""1. A
B
  - X
  - Y
""")
        s should be_success(""". A B
** X
** Y
""")
      }
    }
  }
}
