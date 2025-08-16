package org.smartdox

import org.scalatest._
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.smartdox.parser.Dox2Parser

/*
 * @since   Jun.  9, 2025
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ParagraphSpec extends AnyWordSpec with Matchers {
  val hocon = """a.b=c
x.y=z
"""
  val phocon = Dox2Parser.parseOne(hocon)

  "A Paragraph" when {
    "toText" should {
      "output html" in {
        phocon.toText should be("a.b=c x.y=z")
      }
    }
    "toPlainText" should {
      "output logical line text" in {
        phocon.toPlainText should be("a.b=c x.y=z\n")
      }
    }
    "toData" should {
      "output physical line text" in {
        phocon.toData should be("""a.b=c
x.y=z
""")
      }
    }
  }
}
