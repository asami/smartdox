package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * Glossary Ontology
 * ----------------------------------------------------------------------
 * Defines vocabulary and schema for glossary terms used in
 * the SimpleModeling.org knowledge ecosystem.
 *
 * This unified ontology includes:
 *  - RDF/OWL vocabulary definitions for terms
 *  - Utility methods to build RDF triples for each term instance
 *
 * @since   Nov. 13, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object GlossaryOntology {
  val prefix = "glossary"
  val namespace = "https://www.simplemodeling.org/glossary/ontology/1.0#"
  def uri(local: String) = namespace + local

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val Glossary = uri("Glossary")
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
    "Glossary" -> Glossary,
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

  def glossaryTriples(glossaryId: String, termIds: Seq[String]): Seq[Triple] = {
    val subject = Node.Uri(s"${namespace}glossary/$glossaryId")
    val termTriples = termIds.map(tid => Triple(subject, Node.Uri(hasTerm), termNode(tid)))
    Triple(subject, RdfType, Node.Uri(Glossary)) +: termTriples
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
}
