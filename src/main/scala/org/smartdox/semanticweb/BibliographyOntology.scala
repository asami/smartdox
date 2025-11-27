package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * Bibliography Ontology
 * ----------------------------------------------------------------------
 * Defines vocabulary and schema for bibliography terms used in
 * the SimpleModeling.org knowledge ecosystem.
 *
 * This unified ontology includes:
 *  - RDF/OWL vocabulary definitions for terms
 *  - Utility methods to build RDF triples for each term instance
 *
 * @since   Nov. 20, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object BibliographyOntology extends OntologyModel {
  val prefix = "bibliography"
  val namespace = "https://www.simplemodeling.org/bibliography/ontology/0.1-SNAPSHOT#"

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val Bibliography = uri("Bibliography")
  val Term     = uri("Term")

  // ------------------------------------------------------------------
  // Properties
  // ------------------------------------------------------------------
  val hasTerm        = uri("hasTerm")
  val hasDefinition  = uri("hasDefinition")
  val synonymOf      = uri("synonymOf")
  val relatedTo      = uri("relatedTo")
  val language       = uri("language")
  val example        = uri("example")
  val category       = uri("category")

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  override lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "Bibliography" -> Bibliography,
    "Term" -> Term,
    "hasTerm" -> hasTerm,
    "hasDefinition" -> hasDefinition,
    "synonymOf" -> synonymOf,
    "relatedTo" -> relatedTo
  )

  // Base schema triples for this ontology (can be extended later)
  lazy val toTriples: Seq[Triple] = Seq.empty

  override def jsonldProfile: RdfRenderer.JsonLDProfile =
    RdfRenderer.JsonLDProfile.SmartDox
}
