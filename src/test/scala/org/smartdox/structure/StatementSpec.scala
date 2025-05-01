package org.smartdox.structure

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._
import org.smartdox.parser.UseDox2Parser

/*
 * @since   Nov. 22, 2024
 *  version Nov. 24, 2024
 * @version Dec. 20, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class StatementSpec extends WordSpec with Matchers with ScalazMatchers with UseDox2Parser {
  "Statement" should {
//     "tryout" which {
//       "underscore" in {
//         val in = """# Vision

// O_P
// """
//         val dox = parse_model(in)
//         val s = Statement.Builder().build(dox)
//         s should be(
//           Statement(
//             Dox.makeSection(dox),
//             None,
//             Definition.TableDefinition.create("Definition One", "A:a\tB:b")
//           )
//         )
//       }
//     }
    "table" which {
      "plain" in {
        val in = """# Vision

FOR FFF WHO WWW THE TTT IS A AAA THAT TTT UNLIKE UUU OUR_PRODUCT OOO
"""
        val dox = parse_model(in)
        val s = Statement.Builder().build(dox)
        s should be(
          Statement(
            Dox.makeSection(dox),
            None,
            Definition.TextDefinition("Vision", "FOR FFF WHO WWW THE TTT IS A AAA THAT TTT UNLIKE UUU OUR_PRODUCT OOO")
          )
        )
      }
//       "formatted" in {
//         val in = """# Vision

// FOR FFF
// WHO WWW
// THE TTT
// IS-A AAA
// THAT TTT
// UNLIKE UUU
// OUR_PRODUCT OOO
// """
//         val dox = parse_model(in)
//         val s = Statement.Builder().build(dox)
//         s should be(
//           Statement(
//             Dox.makeSection(dox),
//             None,
//             Definition.TextDefinition("Vision", """FOR FFF
// WHO WWW
// THE TTT
// IS-A AAA
// THAT TTT
// UNLIKE UUU
// OUR_PRODUCT OOO
// """)
//           )
//         )
//       }
      "properties" in {
        val in = """# Vision

FOR=FFF
WHO=WWW
THE=TTT
IS-A=AAA
THAT=TTT
UNLIKE=UUU
OUR_PRODUCT=OOO
"""
        val dox = parse_model(in)
        val s = Statement.Builder().build(dox)
        s should be(
          Statement(
            Dox.makeSection(dox),
            None,
            Definition.PropertiesDefinition.createLxsv("Vision", "FOR:FFF\tWHO:WWW\tTHE:TTT\tIS-A:AAA\tTHAT:TTT\tUNLIKE:UUU\tOUR_PRODUCT:OOO")
          )
        )
      }
    }
  }
}
