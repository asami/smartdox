package org.smartdox.structure

import scalaz._, Scalaz._
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._
import org.smartdox.test.DoxMatchers
import org.smartdox.parser.Dox2Parser

/*
 * @since   Nov.  9, 2024
 *  version Nov. 24, 2024
 *  version Dec. 22, 2024
 *  version Aug. 16, 2025
 * @version Oct. 13, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DescriptionDefinitionsSpec extends AnyWordSpec with Matchers with ScalazMatchers with GivenWhenThen with DoxMatchers {
  "DescriptionDefinitions" should {
    "table" which {
      "plain" in {
        Given("an Enhanced Markdown section with a description and table definition")
        val in = """# OK

This is a description.

## Definition One

|A|B|
|-|-|
|a|b|

"""
        val dox = _parse_enhanced(in)
        When("the definition builder interprets the semantic AST without source locations")
        val s = DescriptionDefinitions.Builder().build(dox)
        Then("the section description and typed definition retain their exact contents")
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
        Given("an Enhanced Markdown section containing an ordinary unordered list")
        val in = """# OK

This is a description.

## Definition One

- a
- b
- c

"""
        val dox = _parse_enhanced(in)
        When("the definition builder applies its default admitted definition kinds")
        val s = DescriptionDefinitions.Builder().build(dox)
        Then("ordinary lists are not promoted to typed definitions")
        s.definitions shouldBe empty
      }
    }
    "ol" which {
      "plain" in {
        Given("an Enhanced Markdown section containing an ordinary ordered list")
        val in = """# OK

This is a description.

## Definition One

1. a
2. b
3. c

"""
        val dox = _parse_enhanced(in)
        When("the definition builder applies its default admitted definition kinds")
        val s = DescriptionDefinitions.Builder().build(dox)
        Then("ordinary lists are not promoted to typed definitions")
        s.definitions shouldBe empty
      }
    }
    "dl" which {
      "plain" in {
        Given("an Enhanced Markdown section with a description and definition-list extension")
        val in = """# OK

This is a description.

## Definition One

- A :: a
- B :: b
- C :: c

"""
        val dox = _parse_enhanced(in)
        When("the definition builder interprets the semantic AST without source locations")
        val s = DescriptionDefinitions.Builder().build(dox)
        Then("the section description and typed definition retain their exact contents")
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

  private def _parse_enhanced(source: String): Dox =
    Dox2Parser.parse(Dox2Parser.Config.enhancedMarkdown.copy(isLocation = false), source)
}
