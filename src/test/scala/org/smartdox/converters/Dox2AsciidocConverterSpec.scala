package org.smartdox.converters

import scalaz._, Scalaz._
import java.io.File
import org.goldenport.collection.VectorMap
import org.scalatest.GivenWhenThen
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
import org.smartdox.{Html5, Html5Inline, Text}
import org.smartdox.parser.UseDox2Parser
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator._
import org.smartdox.generators.AntoraGenerator

/*
 * @since   Jun. 20, 2025
 *  version Jul.  1, 2025
 *  version Aug. 16, 2025
 *  version Oct. 12, 2025
 *  version May. 14, 2026
 * @version Jul. 20, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class Dox2AsciidocConverterSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDox2Parser with ConsequenceMatchers with GivenWhenThen {
  val context = Dox2AsciidocConverter.Context(
    AntoraGenerator.Context(
      Context.create(),
      DoxSite.Config.default
    ),
    true
  )

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
    "Img" should {
      "emit Asciidoc inline image macro for a standalone image" in {
        val s = make_asciidoc("""[[images/why-reconstruct-software-development-methodology/summary-ja.png]]
""")
        s should be_success("""image:why-reconstruct-software-development-methodology:summary-ja.png[]

""")
      }
    }
    "Figure" should {
      "keep block image macro with caption attributes" in {
        val s = make_asciidoc("""#+CAPTION: Summary
[[images/why-reconstruct-software-development-methodology/summary-ja.png]]
""")
        s should be_success(""".Summary
image::why-reconstruct-software-development-methodology:summary-ja.png[role=img-figure,alt=Summary,title=Summary]

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
      "multiple top sections" in {
        val s = make_asciidoc("""# Title

# One

A

# Two

B
""")
        s should be_success("""= Title


== One

A

== Two

B
""")
      }
      "nested org title" in {
        val s = make_asciidoc("""# Title

## Constant

#+TITLE: Example
```
val A: Int = ???
```
""")
        s should be_success("""= Title

== Constant

=== Example


[source,text]
----
val A: Int = ???
----
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
    "Html5" should {
      "emit nested article media as a block passthrough" in {
        Given("a block Html5 article-media tree with nested raw HTML elements")
        val articlemedia = Html5(
          "div",
          VectorMap("class" -> "smartdox-article-media"),
          List(
            Html5(
              "div",
              VectorMap("class" -> "smartdox-article-media-pdf"),
              List(
                Html5("a", VectorMap("href" -> "/article.pdf"), List(Text("Article PDF"))),
                Html5("a", VectorMap("href" -> "/summary.pdf"), List(Text("Summary slides PDF")))
              )
            ),
            Html5(
              "div",
              VectorMap("class" -> "smartdox-article-media-video"),
              List(Html5("a", VectorMap("href" -> "https://example.test/watch"), List(Text("Watch"))))
            )
          )
        )

        When("the block tree is converted to AsciiDoc")
        val converted = new Dox2AsciidocConverter(context).convert(articlemedia).take

        Then("the complete tree is enclosed by standalone block passthrough delimiters")
        converted should startWith("++++\n<div class=\"smartdox-article-media\">")
        converted should endWith("</div>\n++++\n\n")
        converted should include ("<div class=\"smartdox-article-media-pdf\">")
        converted should include ("<a href=\"/article.pdf\">Article PDF</a>")
        converted should include ("<a href=\"/summary.pdf\">Summary slides PDF</a>")
        converted should include("<div class=\"smartdox-article-media-video\"><a href=\"https://example.test/watch\">Watch</a></div>")
        converted should not include ("pass:[")
      }

      "retain inline passthrough for Html5Inline" in {
        Given("an inline Html5Inline element")
        val inlinehtml = Html5Inline("span", VectorMap("class" -> "label"), List(Text("Inline")))

        When("the inline element is converted to AsciiDoc")
        val converted = new Dox2AsciidocConverter(context).convert(inlinehtml).take

        Then("the inline element remains wrapped by an inline passthrough macro")
        converted shouldBe "pass:[<span class=\"label\">Inline</span>]"
      }
    }
  }
}
