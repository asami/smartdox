package org.smartdox.semanticweb

import java.util.Locale
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.smartdox.{Dfn, Document, Dox, Span, Term}
import org.smartdox.generator.Context
import org.smartdox.doxsite.LinkCollector
import org.smartdox.parser.Dox2Parser
import org.smartdox.semanticweb.Rdf.{Node, Triple}
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.transformers.Dox2HtmlTransformer

/*
 * @since   Aug. 21, 2026
 * @version Aug. 21, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SimpleModelingRdfTermAcceptanceSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "the SimpleModeling.org RDF terminology fixture" should {
    "traverse through the ordinary site scanner without treating terms as links" in {
      Given("the mirrored external SmartDox fixture")
      val document = _fixture_document

      When("the ordinary site scanner traverses the fixture")
      val result = scala.util.Try {
        document.traverse(new LinkCollector.SiteScanner.Scanner())
      }

      Then("traversal completes without an exception")
      result.isSuccess shouldBe true
    }

    "resolve only its explicit authoritative glossary references" in {
      Given("the mirrored external SmartDox fixture and its concept-keyed registry")
      val document = _fixture_document
      val registry = _registry

      When("the fixture is parsed and resolved against the authoritative concept identities")
      val resolved = RdfTermResolver.resolve(document, registry.knownConcepts)
      val terms = _nodes(document).collect { case term: Term => term }
      val dfns = _nodes(document).collect { case dfn: Dfn => dfn }
      val stablespans = _nodes(document).collect {
        case span: Span if span.attribute("strategy").contains("stable") => span
      }

      Then("every explicit reference resolves without diagnostics and ordinary words stay unreferenced")
      resolved.diagnostics shouldBe Vector.empty
      resolved.references should have size 5
      resolved.references.map(_.concept.iri) shouldBe Vector(
        _structural_ownership_iri,
        _structural_ownership_iri,
        _composition_iri,
        _realization_iri,
        _cml_iri
      )
      terms should have size 5
      dfns shouldBe Vector.empty
      stablespans.map(_.toText) shouldBe Vector("所有", "合成", "実現")
    }

    "render Japanese-first display with independent speech and stable ordinary text" in {
      Given("the resolved fixture document and the Japanese display registry")
      val document = _fixture_document
      val registry = _registry

      When("the document is transformed to Japanese HTML")
      val display = RdfTermDisplay.project(document, Locale.JAPANESE, registry)
      val html = Dox2HtmlTransformer(
        Context.create().withTargetI18NContext(Locale.JAPANESE),
        Dox2HtmlTransformer.Rule(isDefaultCss = false, termRegistry = registry)
      ).transform(document).get.getOrElse(throw new IllegalStateException("HTML transformation did not produce a document"))

      Then("canonical and short references display Japanese first while ordinary words remain authored")
      display.references.map(_.visibleText) should contain ("構造上の所有（Structural Ownership）")
      display.references.find(_.form == "short").map(_.visibleText) shouldBe Some("所有")
      display.references.find(_.iri == _cml_iri).map(_.speechText) shouldBe Some("CML")
      html should include ("構造上の所有（Structural Ownership）")
      html should include ("所有")
      html should include ("CML（Cozy Modeling Language）")
      html should include ("aria-label=\"構造上の所有\"")
      html should include ("aria-label=\"CML\"")
      html should not include "aria-label=\"CML（Cozy Modeling Language）\""
      html should include ("<span strategy=\"stable\">所有</span>")
      html should include ("<span strategy=\"stable\">合成</span>")
      html should include ("<span strategy=\"stable\">実現</span>")
      html.sliding("data-rdf-term-iri=\"".length).count(_ == "data-rdf-term-iri=\"") shouldBe 5
    }

    "project resolved identities into RDF and JSON-LD node evidence" in {
      Given("the resolved fixture terminology with parser source locations")
      val document = _fixture_document
      val registry = _registry
      val resolved = RdfTermResolver.resolve(document, registry.knownConcepts)

      When("the terminology is projected with its source document and public page identities")
      val projection = RdfTermProjection.project(
        resolved,
        _source_document_iri,
        _public_fixture_page_iri,
        _source_path,
        Locale.JAPANESE,
        registry.knownConcepts
      )
      val source = Node.Uri(_source_document_iri)
      val page = Node.Uri(_public_fixture_page_iri)
      val occurrence = projection.occurrences.head
      val occurrencenode = Node.Uri(occurrence.id)

      Then("the graph contains source-document, occurrence, and denotesConcept evidence for resolved IRIs")
      projection.occurrences should have size 5
      projection.occurrences.forall(_.sourcePath == _source_path) shouldBe true
      projection.graph.triples should contain (Triple(source, RdfType, Node.Uri(SmartDoxOntology.sourceDocument)))
      projection.graph.triples should contain (Triple(page, RdfType, Node.Uri(SmartDoxOntology.publicGlossaryPage)))
      projection.graph.triples should contain (Triple(occurrencenode, Node.Uri(SmartDoxOntology.inSourceDocument), source))
      projection.graph.triples should contain (Triple(occurrencenode, Node.Uri(SmartDoxOntology.denotesConcept), Node.Uri(occurrence.conceptIri)))
      projection.toJsonLd should include (_source_document_iri)
      projection.toJsonLd should include (_public_fixture_page_iri)
      projection.toJsonLd should include (_structural_ownership_iri)
      projection.toJsonLd should include (_composition_iri)
      projection.toJsonLd should include (_realization_iri)
      projection.toJsonLd should include (_cml_iri)
    }
  }

  private val _source_path = "overview/rdf-term-acceptance.dox"
  private val _source_document_iri = "https://www.simplemodeling.org/overview/rdf-term-acceptance"
  private val _public_fixture_page_iri = "https://www.simplemodeling.org/overview/rdf-term-acceptance.html"
  private val _structural_ownership_iri = "https://www.simplemodeling.org/glossary/object-foundation/structural-ownership"
  private val _composition_iri = "https://www.simplemodeling.org/glossary/object-foundation/composition"
  private val _realization_iri = "https://www.simplemodeling.org/glossary/object-foundation/realization"
  private val _cml_iri = "https://www.simplemodeling.org/glossary/literate-modeling/cml"

  private val _structural_ownership = RdfTermDisplay.Concept(
    iri = _structural_ownership_iri,
    preferredLabels = Map("en" -> "Structural Ownership", "ja" -> "構造上の所有"),
    shortLabels = Map("en" -> "Ownership", "ja" -> "所有"),
    scope = Some("Object Foundation / whole-part structure"),
    speechLabels = Map("en" -> "Structural Ownership", "ja" -> "構造上の所有")
  )
  private val _composition = RdfTermDisplay.Concept(
    iri = _composition_iri,
    preferredLabels = Map("en" -> "Composition", "ja" -> "合成関係"),
    shortLabels = Map("en" -> "Composition", "ja" -> "合成"),
    scope = Some("Object Foundation / whole-part relationship"),
    speechLabels = Map("en" -> "Composition", "ja" -> "合成関係")
  )
  private val _realization = RdfTermDisplay.Concept(
    iri = _realization_iri,
    preferredLabels = Map("en" -> "Realization", "ja" -> "実現関係"),
    shortLabels = Map("en" -> "Realization", "ja" -> "実現"),
    scope = Some("Object Foundation / model correspondence"),
    speechLabels = Map("en" -> "Realization", "ja" -> "実現関係")
  )
  private val _cml = RdfTermDisplay.Concept(
    iri = _cml_iri,
    preferredLabels = Map("en" -> "Cozy Modeling Language", "ja" -> "CML"),
    shortLabels = Map("en" -> "CML", "ja" -> "CML"),
    scope = Some("Literate Modeling / formal modeling language"),
    speechLabels = Map("en" -> "Cozy Modeling Language", "ja" -> "CML")
  )

  private val _registry = RdfTermDisplay.Registry(
    _structural_ownership,
    _composition,
    _realization,
    _cml
  )

  private def _fixture_document: Document = {
    val stream = Option(getClass.getResourceAsStream("/simplemodeling-org/phase-6-rdf-term-acceptance.dox")).getOrElse(
      throw new IllegalStateException("RDF terminology acceptance fixture is unavailable")
    )
    val source = scala.io.Source.fromInputStream(stream)(scala.io.Codec.UTF8)
    try {
      Dox2Parser.parse(Dox2Parser.Config.smartdox, source.mkString).asInstanceOf[Document]
    } finally {
      source.close()
    }
  }

  private def _nodes(dox: Dox): Vector[Dox] =
    dox +: dox.elements.toVector.flatMap(_nodes)
}
