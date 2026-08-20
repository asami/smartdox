package org.smartdox.semanticweb

import java.util.Locale
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.smartdox.Document
import org.smartdox.generator.Context
import org.smartdox.parser.Dox2Parser
import org.smartdox.transformers.Dox2DomHtmlTransformer
import org.smartdox.transformers.Dox2HtmlTransformer

/*
 * @since   Aug. 20, 2026
 * @version Aug. 20, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RdfTermDisplaySpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "RdfTermDisplay" should {
    "project localized visible and spoken forms" which {
      "render an IRI-keyed Japanese first use with independent speech and stable metadata" in {
        Given("an explicit term resolved through a concept-keyed display registry")
        val document = _document(
          """Terminology: <term ref="ex:object-modeling">Object Modeling</term>
            |Terminology: <term ref="ex:object-modeling">Object Modeling</term>""".stripMargin
        )
        val registry = RdfTermDisplay.Registry(_object_modeling)

        When("the document is rendered for Japanese readers")
        val projection = RdfTermDisplay.project(document, Locale.JAPANESE, registry)
        val dom = new Dox2DomHtmlTransformer(
          Context.create().withTargetI18NContext(Locale.JAPANESE),
          Dox2DomHtmlTransformer.Rule(termRegistry = registry)
        ).documentOut(document)
        val html = _html(document, Locale.JAPANESE, registry)

        Then("the first visible form carries bilingual and abbreviation annotation")
        projection.references.map(_.visibleText) should contain ("オブジェクトモデリング（Object Modeling）（OM）")
        dom.getElementsByTagName("a").item(0).getTextContent shouldBe "オブジェクトモデリング（Object Modeling）（OM）"
        html should include ("オブジェクトモデリング（Object Modeling）（OM）")
        And("the later form uses the localized preferred label without first-use annotation")
        html should include ("<a ")
        html should include ("href=\"https://example.com/term/object-modeling\"")
        html should include ("aria-label=\"オブジェクトモデリング\"")
        html should include (">オブジェクトモデリング</a>")
        And("both occurrences retain the resolved identity rather than a label-derived identity")
        html.sliding("data-rdf-term-iri=\"https://example.com/term/object-modeling\"".length).count(_ == "data-rdf-term-iri=\"https://example.com/term/object-modeling\"") shouldBe 2
        html should include ("data-rdf-term-locale=\"ja\"")
        html should include ("data-rdf-term-kind=\"reference\"")
        html should include ("data-rdf-term-resolution=\"explicit\"")
        html should include ("data-rdf-term-form=\"canonical\"")
        html should include ("data-rdf-term-scope=\"software-modeling\"")
      }

      "render a resolved definition without conflating its local anchor and concept identity" in {
        Given("a definition with a local id and an RDF about reference")
        val document = _document("""Terminology: <dfn about="ex:object-modeling" id="object-modeling">Object Modeling</dfn>""")

        When("the document is rendered with display metadata")
        val html = _html(document, Locale.ENGLISH, RdfTermDisplay.Registry(_object_modeling))

        Then("the HTML definition retains the local anchor and separately records its resolved IRI")
        html should include ("<dfn ")
        html should include ("aria-label=\"Object Modeling\"")
        html should include ("id=\"object-modeling\"")
        html should include ("data-rdf-term-iri=\"https://example.com/term/object-modeling\"")
        html should include ("data-rdf-term-kind=\"definition\"")
      }

      "project a registry-backed authored bilingual form using its resolved IRI" in {
        Given("an authored bilingual term matched by the display registry")
        val bilingualdocument = _document(
          """Terminology: <term ref="ex:object-modeling" form="bilingual">Object Modeling｜オブジェクトモデリング</term>"""
        )
        val bilingualregistry = RdfTermDisplay.Registry(_object_modeling)

        When("the bilingual reference is projected for Japanese readers")
        val bilingualprojection = RdfTermDisplay.project(bilingualdocument, Locale.JAPANESE, bilingualregistry)
        val bilingualhtml = _html(bilingualdocument, Locale.JAPANESE, bilingualregistry)

        Then("the authored form resolves and receives registry-backed display metadata")
        bilingualprojection.references should have size 1
        bilingualprojection.references.head.iri shouldBe "https://example.com/term/object-modeling"
        bilingualprojection.references.head.visibleText shouldBe "オブジェクトモデリング（Object Modeling）"
        bilingualprojection.references.head.form shouldBe "bilingual"
        bilingualhtml should include ("data-rdf-term-iri=\"https://example.com/term/object-modeling\"")
        bilingualhtml should include ("aria-label=\"オブジェクトモデリング\"")
      }

      "annotate the first canonical occurrence after a short occurrence" in {
        Given("a short occurrence followed by a canonical occurrence for one concept")
        val shortcanonicaldocument = _document(
          """Terminology: <term ref="ex:object-modeling" form="short">Object Model</term>
            |Terminology: <term ref="ex:object-modeling">Object Modeling</term>""".stripMargin
        )
        val shortcanonicalregistry = RdfTermDisplay.Registry(_object_modeling)

        When("the occurrences are projected for Japanese readers")
        val shortcanonicalreferences = RdfTermDisplay.project(
          shortcanonicaldocument,
          Locale.JAPANESE,
          shortcanonicalregistry
        ).references

        Then("the short form does not consume first-use canonical annotation")
        shortcanonicalreferences.map(_.visibleText) shouldBe Vector(
          "オブジェクトモデル",
          "オブジェクトモデリング（Object Modeling）（OM）"
        )
      }
    }

    "enforce safe link policy" which {
      "require an explicit safe link policy before rendering a link" in {
        Given("a resolved term whose registry metadata does not opt into a link")
        val document = _document("""Terminology: <term ref="ex:object-modeling">Object Modeling</term>""")
        val metadata = _object_modeling.copy(linkPolicy = RdfTermDisplay.LinkPolicy.NoLink)

        When("the document is rendered")
        val html = _html(document, Locale.ENGLISH, RdfTermDisplay.Registry(metadata))

        Then("the resolved occurrence remains a semantic span")
        html should include ("<span ")
        html should include ("aria-label=\"Object Modeling\"")
        html should not include "href=\"https://example.com/term/object-modeling\""
      }

      "accept only absolute HTTP(S) destinations as an explicit link policy" in {
        Given("candidate destinations outside the safe absolute HTTP(S) boundary")
        val javascript = RdfTermDisplay.LinkPolicy.external("javascript:alert(1)")
        val relativeHttp = RdfTermDisplay.LinkPolicy.external("https:object-modeling")

        When("the display registry validates the link policy")
        val safe = RdfTermDisplay.LinkPolicy.external("https://example.com/term/object-modeling")

        Then("only the absolute HTTPS destination is accepted")
        javascript shouldBe None
        relativeHttp shouldBe None
        safe should not be empty
      }
    }
  }

  private val _object_modeling = RdfTermDisplay.Concept(
    iri = "https://example.com/term/object-modeling",
    preferredLabels = Map("en" -> "Object Modeling", "ja" -> "オブジェクトモデリング"),
    shortLabels = Map("en" -> "Object Model", "ja" -> "オブジェクトモデル"),
    aliases = Map("en" -> Vector("Object-Oriented Modeling")),
    abbreviations = Map("en" -> "OM", "ja" -> "OM"),
    scope = Some("software-modeling"),
    speechLabels = Map("en" -> "Object Modeling", "ja" -> "オブジェクトモデリング"),
    linkPolicy = RdfTermDisplay.LinkPolicy.external("https://example.com/term/object-modeling").get
  )

  private def _html(document: Document, locale: Locale, registry: RdfTermDisplay.Registry): String = {
    val context = Context.create().withTargetI18NContext(locale)
    Dox2HtmlTransformer(context, Dox2HtmlTransformer.Rule(isDefaultCss = false, termRegistry = registry)).transform(document).get.
      getOrElse(throw new IllegalStateException("SmartDox HTML rendering did not produce a document"))
  }

  private def _document(body: String): Document =
    Dox2Parser.parse(Dox2Parser.Config.smartdox,
      s"""RDF terminology
         |===============
         |
         |# HEAD
         |
         |term_namespaces {
         |  ex = "https://example.com/term/"
         |}
         |
         |# Terms
         |
         |$body
         |""".stripMargin
    ).asInstanceOf[Document]
}
