package org.smartdox.semanticweb

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.smartdox._
import org.smartdox.parser.Dox2Parser

/*
 * @since   Aug. 18, 2026
 * @version Aug. 19, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RdfTermResolverSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "RdfTermResolver" should {
    "preserve term AST distinctions and dfn identity attributes" in {
      Given("a parsed SmartDox document with RDF terminology inline tags")
      val document = _document(
        """Terminology: <dfn about="ex:entity" id="entity-anchor">Entity</dfn>
          |Terminology: <term ref="ex:entity">Entity</term>
          |Terminology: <noterm>literal Entity</noterm>""".stripMargin
      )

      When("the inline AST is traversed")
      val nodes = _nodes(document)
      val definition = nodes.collectFirst { case m: Dfn => m }.get

      Then("the semantic tags remain distinct AST values")
      nodes.count(_.isInstanceOf[Term]) shouldBe 1
      nodes.count(_.isInstanceOf[NoTerm]) shouldBe 1
      definition.attribute("about") shouldBe Some("ex:entity")
      definition.attribute("id") shouldBe Some("entity-anchor")
    }

    "resolve CURIE and absolute-IRI definitions and references" in {
      Given("a document HEAD namespace map and explicit definitions")
      val document = _document(
        """Terminology: <dfn about="ex:entity" id="entity-anchor">Entity</dfn>
          |Terminology: <dfn about="https://example.com/term/attribute" id="attribute-anchor">Attribute</dfn>
          |Terminology: <term ref="ex:entity">Entity</term>
          |Terminology: <term ref="https://example.com/term/attribute">Attribute</term>
          |Terminology: <noterm>Entity</noterm>""".stripMargin
      )

      When("explicit references are resolved")
      val result = RdfTermResolver.resolve(document)

      Then("only exact expanded identities are resolved")
      result.namespaces shouldBe Map("ex" -> "https://example.com/term/")
      result.definitions.map(_.concept.iri) shouldBe Vector(
        "https://example.com/term/entity",
        "https://example.com/term/attribute"
      )
      result.references.map(_.concept.iri) shouldBe Vector(
        "https://example.com/term/entity",
        "https://example.com/term/attribute"
      )
      result.diagnostics shouldBe Vector.empty
    }

    "support short, bilingual, and verbatim authored forms from supplied concepts" in {
      Given("a parsed document with explicit references and a read-only known concept")
      val document = _document(
        """Terminology: <term ref="ex:entity" form="short">Ent.</term>
          |Terminology: <term ref="ex:entity" form="bilingual">Entity｜実体</term>
          |Terminology: <term ref="ex:entity" form="verbatim">the entity named by the author</term>""".stripMargin
      )
      val known = Vector(RdfTermResolver.KnownConcept(
        "https://example.com/term/entity",
        "Entity",
        short = Some("Ent."),
        bilingual = Some("Entity｜実体")
      ))

      When("the forms are resolved")
      val result = RdfTermResolver.resolve(document, known)

      Then("their compatible visible text is retained without mutating the supplied concept")
      result.references.map(_.form) shouldBe Vector(
        RdfTermResolver.TermForm.Short,
        RdfTermResolver.TermForm.Bilingual,
        RdfTermResolver.TermForm.Verbatim
      )
      result.references.map(_.visibleText) shouldBe Vector(
        "Ent.",
        "Entity｜実体",
        "the entity named by the author"
      )
      known.head.short shouldBe Some("Ent.")
      result.diagnostics shouldBe Vector.empty
    }

    "resolve only explicit references without bare-label fallback" in {
      Given("a document containing a matching bare label and one explicit term")
      val document = _document(
        """Terminology: <dfn about="ex:entity">Entity</dfn> is defined here.
          |
          |A bare Entity is not an RDF reference.
          |
          |Terminology: <term ref="ex:entity">Entity</term> is explicit.""".stripMargin
      )

      When("the document is resolved")
      val result = RdfTermResolver.resolve(document)

      Then("only the explicit CURIE reference is present")
      result.references should have size 1
      result.references.head.concept.iri shouldBe "https://example.com/term/entity"
      result.diagnostics shouldBe Vector.empty
    }

    "retain unsupported-form diagnostics when a term reference is also missing" in {
      Given("a term with empty visible text, an unsupported form, and no explicit reference")
      val document = _document(
        """Terminology: <term form="unsupported"></term>"""
      )

      When("the document is resolved")
      val result = RdfTermResolver.resolve(document)

      Then("all independent authoring diagnostics are retained in deterministic order")
      result.references shouldBe Vector.empty
      result.diagnostics.map(_.code) shouldBe Vector(
        RdfTermResolver.DiagnosticCode.EmptyVisibleText,
        RdfTermResolver.DiagnosticCode.UnsupportedForm,
        RdfTermResolver.DiagnosticCode.MissingTermReference
      )
    }

    "report every frozen authoring error deterministically" in {
      Given("a parsed document containing each invalid RDF terminology condition")
      val document = _document(
        """Terminology: <dfn about="ex:entity">Entity</dfn>
          |Terminology: <dfn about="ex:empty">Empty</dfn>
          |Terminology: <dfn about="ex:duplicate">Duplicate</dfn>
          |Terminology: <dfn about="ex:duplicate">Duplicate</dfn>
          |Terminology: <dfn about="ex:label-a">Ambiguous</dfn>
          |Terminology: <dfn about="ex:label-b">Ambiguous</dfn>
          |Terminology: <term ref=":entity">Entity</term>
          |Terminology: <term ref="unknown:entity">Entity</term>
          |Terminology: <term ref="relative-entity">Entity</term>
          |Terminology: <term ref="https:entity">Entity</term>
          |Terminology: <term>Entity</term>
          |Terminology: <term ref="ex:empty"></term>
          |Terminology: <term ref="ex:entity" form="unsupported">Entity</term>
          |Terminology: <term ref="ex:missing">Missing</term>
          |Terminology: <term ref="ex:entity" form="short">Entity</term>
          |Terminology: <noterm><term ref="ex:entity">Entity</term></noterm>
          |Terminology: <noterm><dfn about="ex:nested">Nested</dfn></noterm>
          |Terminology: <noterm><noterm>Nested suppression</noterm></noterm>""".stripMargin
      )

      When("the document is resolved")
      val diagnostics = RdfTermResolver.resolve(document).diagnostics.map(_.code)

      Then("each rejected condition has a deterministic diagnostic code")
      diagnostics should contain (RdfTermResolver.DiagnosticCode.MissingPrefix)
      diagnostics should contain (RdfTermResolver.DiagnosticCode.UnknownPrefix)
      diagnostics should contain (RdfTermResolver.DiagnosticCode.RelativeOrMalformedIri)
      diagnostics should contain (RdfTermResolver.DiagnosticCode.MissingTermReference)
      diagnostics should contain (RdfTermResolver.DiagnosticCode.EmptyVisibleText)
      diagnostics should contain (RdfTermResolver.DiagnosticCode.UnsupportedForm)
      diagnostics should contain (RdfTermResolver.DiagnosticCode.UnresolvedExplicitReference)
      diagnostics should contain (RdfTermResolver.DiagnosticCode.DuplicateConceptIdentity)
      diagnostics should contain (RdfTermResolver.DiagnosticCode.ConflictingCanonicalLabel)
      diagnostics should contain (RdfTermResolver.DiagnosticCode.IncompatibleVisibleForm)
      diagnostics.count(_ == RdfTermResolver.DiagnosticCode.NoTermNesting) shouldBe 3
    }
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

  private def _nodes(p: Dox): Vector[Dox] =
    p +: p.elements.toVector.flatMap(_nodes)
}
