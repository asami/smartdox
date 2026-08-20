package org.smartdox.semanticweb

import java.util.Locale
import org.goldenport.parser.ParseLocation
import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.smartdox.Document
import org.smartdox.parser.Dox2Parser
import org.smartdox.semanticweb.Rdf.{Node, Triple}

/*
 * @since   Aug. 21, 2026
 * @version Aug. 21, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RdfTermProjectionSpec extends AnyWordSpec with Matchers with GivenWhenThen with ScalaCheckPropertyChecks {
  "RdfTermProjection" should {
    "project distinct term knowledge-graph resources and complete occurrence records" in {
      Given("a resolved definition and reference with explicit source locations")
      val result = _located_result("Entity", "Entity")

      When("the resolved terminology is projected with explicit source and public-page identities")
      val projection = RdfTermProjection.project(
        result,
        "https://example.com/documents/entity-source",
        "https://example.com/glossary/entity",
        "spec/entity.dox",
        Locale.ENGLISH
      )

      Then("the graph keeps concept, definition, source document, and public page as separate resources")
      val concept = Node.Uri(_concept_iri)
      val definition = Node.Uri("https://example.com/documents/entity-source#term-occurrence-definition-1")
      val source = Node.Uri("https://example.com/documents/entity-source")
      val page = Node.Uri("https://example.com/glossary/entity")
      Set(concept, definition, source, page).size shouldBe 4
      projection.graph.triples should contain (Triple(concept, Node.Uri(SmartDoxOntology.hasDefinitionOccurrence), definition))
      projection.graph.triples should contain (Triple(concept, Node.Uri(SmartDoxOntology.hasPublicGlossaryPage), page))
      projection.graph.triples should contain (Triple(definition, Node.Uri(SmartDoxOntology.inSourceDocument), source))
      projection.graph.triples should contain (Triple(definition, Node.Uri(SmartDoxOntology.denotesConcept), concept))
      projection.graph.triples should contain (Triple(definition, Node.Uri(SmartDoxOntology.sourceLocation), Node.Literal("[41, 1]")))
      And("every emitted occurrence contains the eight BoK-ready fields")
      projection.occurrences should have size 2
      projection.occurrences.foreach { occurrence =>
        occurrence.id should startWith ("https://example.com/documents/entity-source#term-occurrence-")
        occurrence.conceptIri shouldBe _concept_iri
        occurrence.surfaceForm shouldBe "Entity"
        occurrence.locale shouldBe "en"
        occurrence.kind.value should (be ("definition") or be ("reference"))
        occurrence.resolutionKind shouldBe RdfTermProjection.ResolutionKind.Explicit
        occurrence.sourcePath shouldBe "spec/entity.dox"
        occurrence.sourceLocation should not be null
      }
    }

    "project Locale.ROOT as the und BCP-47 locale" in {
      Given("a resolved definition and reference with Locale.ROOT")
      val result = _located_result("Entity", "Entity")

      When("the terminology is projected")
      val projection = RdfTermProjection.project(
        result,
        "https://example.com/documents/entity-source",
        "https://example.com/glossary/entity",
        "spec/entity.dox",
        Locale.ROOT
      )

      Then("each emitted occurrence uses the valid und BCP-47 language tag")
      projection.occurrences.map(_.locale) shouldBe Vector("und", "und")
    }

    "preserve the resolved canonical IRI in JSON-LD without a label-derived identity" in {
      Given("a resolved reference whose surface form is not an identifier")
      val result = _located_result("Entity", "An authored surface form")

      When("the projection renders JSON-LD through the RDF renderer")
      val jsonld = RdfTermProjection.project(
        result,
        "https://example.com/documents/entity-source",
        "https://example.com/glossary/entity",
        "spec/entity.dox",
        Locale.ENGLISH
      ).toJsonLd

      Then("the canonical IRI and semantic contexts are retained while the authored label is never used as an ID")
      jsonld should include (_concept_iri)
      jsonld should include ("\"dcterms\"")
      jsonld should include ("\"glossary\"")
      jsonld should include ("\"schema\"")
      jsonld should not include "@id\": \"An-authored-surface-form"
    }

    "omit occurrences without an actual source location" in {
      Given("a resolved definition and reference that have no source locations")
      val resolved = RdfTermResolver.resolve(_document("Entity", "Entity"))
      val result = resolved.copy(
        definitions = resolved.definitions.map(_.copy(location = None)),
        references = resolved.references.map(_.copy(location = None))
      )

      When("the projection is requested with explicit source identities")
      val projection = RdfTermProjection.project(
        result,
        "https://example.com/documents/entity-source",
        "https://example.com/glossary/entity",
        "spec/entity.dox",
        Locale.ENGLISH
      )

      Then("no incomplete BoK-ready occurrence record is emitted")
      projection.occurrences shouldBe Vector.empty
    }

    "retain canonical identity across generated surface-form variations" in {
      val surfacegenerator = Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString)
      forAll(surfacegenerator) { surface =>
        Given("a generated authored surface form for an explicitly resolved reference")
        val result = _located_result("Entity", surface, "verbatim")

        When("the resolved reference is projected")
        val projection = RdfTermProjection.project(
          result,
          "https://example.com/documents/entity-source",
          "https://example.com/glossary/entity",
          "spec/entity.dox",
          Locale.ENGLISH
        )

        Then("the surface variation retains the exact canonical concept IRI")
        projection.occurrences.find(_.kind == RdfTermProjection.OccurrenceKind.Reference).get.conceptIri shouldBe _concept_iri
        projection.toJsonLd should include (_concept_iri)
      }
    }
  }

  private val _concept_iri = "https://example.com/term/entity"

  private def _located_result(
    definitionsurface: String,
    referencesurface: String,
    form: String = "canonical"
  ): RdfTermResolver.Result = {
    val result = RdfTermResolver.resolve(_document(definitionsurface, referencesurface, form))
    result.copy(
      definitions = result.definitions.map(_.copy(location = Some(ParseLocation.create(41)))),
      references = result.references.map(_.copy(location = Some(ParseLocation.create(43))))
    )
  }

  private def _document(
    definitionsurface: String,
    referencesurface: String,
    form: String = "canonical"
  ): Document =
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
         |Terminology: <dfn about="ex:entity">$definitionsurface</dfn>
         |Terminology: <term ref="ex:entity" form="$form">$referencesurface</term>
         |""".stripMargin
    ).asInstanceOf[Document]
}
