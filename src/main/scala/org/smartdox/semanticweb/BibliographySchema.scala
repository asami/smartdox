package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Rdf.Node
import org.smartdox.semanticweb.Rdf.Node._
import org.smartdox.semanticweb.Rdf.Triple
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/**
 * BibliographySchema
 * ----------------------------------------------------------------------
 * ABox builder for Bibliography and Term instances.
 * Extracted from the original unified ontology to keep OntologyModel
 * purely TBox/RBox and move instance builders here.
 *
 * @since   Nov. 27, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object BibliographySchema extends SchemaModel {
  val prefix = "bibliography"
  val namespace = "https://www.simplemodeling.org/bibliography/schema/0.1-SNAPSHOT#"

  // ------------------------------------------------------------------
  // JSON-LD Context (reuse ontology IRIs)
  // ------------------------------------------------------------------
  override lazy val jsonldContext: Map[String, Any] = Map(
    "Bibliography" -> BibliographyOntology.Bibliography,
    "Term" -> BibliographyOntology.Term,
    "hasTerm" -> BibliographyOntology.hasTerm,
    "hasDefinition" -> BibliographyOntology.hasDefinition,
    "synonymOf" -> BibliographyOntology.synonymOf,
    "relatedTo" -> BibliographyOntology.relatedTo,
    "language" -> BibliographyOntology.language,
    "example" -> BibliographyOntology.example,
    "category" -> BibliographyOntology.category
  )

  // ------------------------------------------------------------------
  // ABox Builders
  // ------------------------------------------------------------------
  def termNode(termId: String): Node.Uri =
    Node.Uri(BibliographyOntology.namespace + termId)

  def bibliographyTriples(bibliographyId: String, termIds: Seq[String]): Seq[Triple] = {
    val subject = Node.Uri(BibliographyOntology.namespace + bibliographyId)
    val termTriples = termIds.map { tid =>
      Triple(subject, Node.Uri(BibliographyOntology.hasTerm), termNode(tid))
    }
    Triple(subject, RdfType, Node.Uri(BibliographyOntology.Bibliography)) +: termTriples
  }

  def termTriples(
    termId: String,
    definition: String,
    lang: Option[String],
    synonyms: Seq[String],
    related: Seq[String]
  ): Seq[Triple] = {
    val subject = termNode(termId)
    val base = Seq(
      Triple(subject, RdfType, Node.Uri(BibliographyOntology.Term)),
      Triple(subject, Node.Uri(BibliographyOntology.hasDefinition), Node.Literal(definition))
    )

    val langTriples =
      lang.map(l => Triple(subject, Node.Uri(BibliographyOntology.language), Node.Literal(l))).toSeq

    val synonymTriples =
      synonyms.map { s =>
        Triple(subject, Node.Uri(BibliographyOntology.synonymOf), Node.Literal(s))
      }

    val relatedTriples =
      related.map { r =>
        Triple(subject, Node.Uri(BibliographyOntology.relatedTo), Node.Literal(r))
      }

    base ++ langTriples ++ synonymTriples ++ relatedTriples
  }
}
