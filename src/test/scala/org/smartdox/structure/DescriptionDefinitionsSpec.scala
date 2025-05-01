package org.smartdox.structure

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._
import org.smartdox.test.DoxMatchers
import org.smartdox.parser.UseDox2Parser

/*
 * @since   Nov.  9, 2024
 *  version Nov. 24, 2024
 * @version Dec. 22, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DescriptionDefinitionsSpec extends WordSpec with Matchers with ScalazMatchers with UseDox2Parser with DoxMatchers {
  "DescriptionDefinitions" should {
    "table" which {
      "plain" in {
        val in = """# OK

This is a description.

## Definition One

|A|B|
|-|-|
|a|b|

"""
        val dox = parse_markdown(in)
        val s = DescriptionDefinitions.Builder().build(dox)
        s should be(dox_description_definitions(
          dox,
          "OK",
          "This is a description.",
          Definition.TableDefinition.createLxsv("Definition One", "A:a\tB:b")
        ))
      }
    }
    "ul" which {
      "plain" in {
        val in = """# OK

This is a description.

## Definition One

- a
- b
- c

"""
        val dox = parse_markdown(in)
        val s = DescriptionDefinitions.Builder().build(dox)
        println(s)
      }
    }
    "ol" which {
      "plain" in {
        val in = """# OK

This is a description.

## Definition One

1. a
2. b
3. c

"""
        val dox = parse_markdown(in)
        val s = DescriptionDefinitions.Builder().build(dox)
        println(s)
      }
    }
    "dl" which {
      "plain" in {
        val in = """# OK

This is a description.

## Definition One

- A :: a
- B :: b
- C :: c

"""
        val dox = parse_markdown(in)
        val s = DescriptionDefinitions.Builder().build(dox)
        s should be(
          dox_description_definitions(
            dox,
            "OK",
            "This is a description.",
            Definition.DescriptionListDefinition(
              "Definition One",
              Vector(
                "A" -> "a",
                "B" -> "b",
                "C" -> "c"
              )
            )
          )
        )
      }
    }
  }
}
