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
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
object BibliographyOntology extends KnowledgeModel {
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
  lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "Bibliography" -> Bibliography,
    "Term" -> Term,
    "hasTerm" -> hasTerm,
    "hasDefinition" -> hasDefinition,
    "synonymOf" -> synonymOf,
    "relatedTo" -> relatedTo
  )

  // ------------------------------------------------------------------
  // RDF Triple Builders (統合された Schema 部分)
  // ------------------------------------------------------------------
  def termNode(termId: String): Node.Uri = Node.Uri(s"${namespace}term/$termId")

  def bibliographyTriples(bibliographyId: String, termIds: Seq[String]): Seq[Triple] = {
    val subject = Node.Uri(s"${namespace}bibliography/$bibliographyId")
    val termTriples = termIds.map(tid => Triple(subject, Node.Uri(hasTerm), termNode(tid)))
    Triple(subject, RdfType, Node.Uri(Bibliography)) +: termTriples
  }

  def termTriples(
    termId: String,
    definition: String,
    lang: Option[String] = None,
    synonyms: Seq[String] = Seq.empty,
    related: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = termNode(termId)
    val synTriples = synonyms.map(s => Triple(subject, Node.Uri(synonymOf), termNode(s)))
    val relTriples = related.map(r => Triple(subject, Node.Uri(relatedTo), termNode(r)))
    val defTriple = Triple(subject, Node.Uri(hasDefinition), Node.Literal(definition, None, lang))
    Seq(Triple(subject, RdfType, Node.Uri(Term))) ++ synTriples ++ relTriples :+ defTriple
  }

  // Base schema triples for this ontology (can be extended later)
  lazy val triples: Seq[Triple] = Seq.empty

  def jsonldProfile: RdfRenderer.JsonLDProfile =
    RdfRenderer.JsonLDProfile.SmartDox

  def toGraph: Rdf.Graph = Rdf.Graph(triples.toVector)
}
