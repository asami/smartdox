package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.semanticweb.SimpleModelingOrgOntology._

/*
 * SimpleModeling.org Schema
 * ----------------------------------------------------------------------
 * Provides RDF graph-generation functions for representing
 * the relationships among all ontologies and schemas
 * within the SimpleModeling.org ecosystem.
 *
 * This schema acts as the meta-linker between ontologies:
 *  - SmartDoxOntology
 *  - BoKOntology
 *  - CategoryOntology
 *  - ProjectOntology
 *  - SimpleModelingOntology
 *  - GlossaryOntology
 *
 * @since   Nov. 13, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object SimpleModelingOrgSchema {

  // ------------------------------------------------------------
  // URI helpers
  // ------------------------------------------------------------
  def ontologyNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def schemaNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def siteNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def kbNode(uriStr: String): Node.Uri = Node.Uri(uriStr)

  // ------------------------------------------------------------
  // Ontology linkage graph
  // ------------------------------------------------------------
  def ontologyTriples(
    ontologyId: String,
    version: Option[String],
    definesSchemas: Seq[String] = Seq.empty,
    governsSites: Seq[String] = Seq.empty,
    aligns: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = ontologyNode(ontologyId)
    val schemaTriples = definesSchemas.map(sid => Triple(subject, Node.Uri(definesSchema), schemaNode(sid)))
    val siteTriples = governsSites.map(sid => Triple(subject, Node.Uri(governsSite), siteNode(sid)))
    val alignTriples = aligns.map(aid => Triple(subject, Node.Uri(alignsWith), ontologyNode(aid)))
    val versionTriple = version.map(v => Triple(subject, Node.Uri(hasVersion), Node.Literal(v)))
    Seq(Triple(subject, RdfType, Node.Uri(Ontology))) ++ schemaTriples ++ siteTriples ++ alignTriples ++ versionTriple
  }

  // ------------------------------------------------------------
  // Knowledge Base graph (top-level)
  // ------------------------------------------------------------
  def knowledgeBaseTriples(
    kbId: String,
    version: String,
    includesOntologies: Seq[String],
    includesSchemas: Seq[String],
    includesSites: Seq[String]
  ): Seq[Triple] = {
    val subject = kbNode(kbId)
    val ontTriples = includesOntologies.map(o => Triple(subject, Node.Uri(includesOntology), ontologyNode(o)))
    val schemaTriples = includesSchemas.map(s => Triple(subject, Node.Uri(includesSchema), schemaNode(s)))
    val siteTriples = includesSites.map(s => Triple(subject, Node.Uri(includesSite), siteNode(s)))
    Seq(
      Triple(subject, RdfType, Node.Uri(KnowledgeBase)),
      Triple(subject, Node.Uri(hasVersion), Node.Literal(version))
    ) ++ ontTriples ++ schemaTriples ++ siteTriples
  }

  // ------------------------------------------------------------
  // Graph assembler
  // ------------------------------------------------------------
  def toGraph(
    kbId: String,
    version: String,
    ontologyDefs: Seq[(String, Option[String], Seq[String], Seq[String], Seq[String])],
    schemas: Seq[String],
    sites: Seq[String]
  ): Graph = {
    val kbTriples = knowledgeBaseTriples(kbId, version, ontologyDefs.map(_._1), schemas, sites)
    val ontTriples = ontologyDefs.flatMap {
      case (oid, ver, schs, sts, aligns) => ontologyTriples(oid, ver, schs, sts, aligns)
    }
    Graph(kbTriples ++ ontTriples)
  }
}
