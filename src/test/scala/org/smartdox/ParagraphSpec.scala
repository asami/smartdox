package org.smartdox

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.smartdox.parser.Dox2Parser

/*
 * @since   Jun.  9, 2025
 * @version Jun.  9, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ParagraphSpec extends WordSpec with Matchers {
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
