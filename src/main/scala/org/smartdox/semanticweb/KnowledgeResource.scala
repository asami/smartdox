package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import scala.collection.mutable

/*
 * Knowledge Resource Schema
 * ----------------------------------------------------------------------
 * Represents metadata of a SmartDox/HTML document
 * as a unified semantic resource within the SimpleModeling.org ecosystem.
 *
 * Each SmartDox page or HTML document corresponds to one KnowledgeResource instance,
 * which accumulates semantic information (SmartDox structure, BoK links,
 * glossary terms, SimpleModel references, etc.) during HTML generation.
 *
 * @since   Nov. 15, 2025
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
case class KnowledgeResource(
  id: String,
  title: Option[String] = None,
  description: Option[String] = None,
  language: Option[String] = None,
  author: Option[String] = None,
  created: Option[String] = None,
  modified: Option[String] = None,
  categoryUri: Option[String] = None,
  projectUri: Option[String] = None,
  glossaryTerms: Seq[String] = Seq.empty,
  modelRefs: Seq[String] = Seq.empty
) {

  // ------------------------------------------------------------------
  // Builder-like update methods (immutable and chainable)
  // ------------------------------------------------------------------

  def withTitle(value: String): KnowledgeResource = copy(title = Some(value))
  def withDescription(value: String): KnowledgeResource = copy(description = Some(value))
  def withLanguage(value: String): KnowledgeResource = copy(language = Some(value))
  def withAuthor(value: String): KnowledgeResource = copy(author = Some(value))
  def withCreated(value: String): KnowledgeResource = copy(created = Some(value))
  def withModified(value: String): KnowledgeResource = copy(modified = Some(value))
  def withCategory(uri: String): KnowledgeResource = copy(categoryUri = Some(uri))
  def withProject(uri: String): KnowledgeResource = copy(projectUri = Some(uri))
  def addGlossary(termUri: String): KnowledgeResource = copy(glossaryTerms = glossaryTerms :+ termUri)
  def addModelRef(modelUri: String): KnowledgeResource = copy(modelRefs = modelRefs :+ modelUri)

  // ------------------------------------------------------------------
  // Conversion to RDF Graph
  // ------------------------------------------------------------------
  def toGraph: Graph = {
    val triples = mutable.ArrayBuffer[Triple]()
    val subject = Node.Uri(id)

    // --- SmartDox core
    triples += Triple(subject, Vocabulary.Rdf.node.`type`, Node.Uri(SmartDoxOntology.Document))
    title.foreach(t => triples += Triple(subject, Node.Uri(SmartDoxOntology.title), Node.Literal(t, None, language)))
    description.foreach(d => triples += Triple(subject, Node.Uri(SmartDoxOntology.description), Node.Literal(d, None, language)))
    created.foreach(d => triples += Triple(subject, Node.Uri(SmartDoxOntology.created), Node.Literal(d)))
    modified.foreach(d => triples += Triple(subject, Node.Uri(SmartDoxOntology.modified), Node.Literal(d)))
    author.foreach(a => triples += Triple(subject, Node.Uri(Dcterms.creator), Node.Literal(a)))

    // --- Knowledge positioning (BoK, Category, Project)
    categoryUri.foreach(c => triples += Triple(subject, Node.Uri(SimpleModelingOrgOntology.governsSite), Node.Uri(c)))
    projectUri.foreach(p => triples += Triple(subject, Node.Uri(SimpleModelingOrgOntology.includesModule), Node.Uri(p)))

    // --- Glossary / SimpleModel relations
    glossaryTerms.foreach(term =>
      triples += Triple(subject, Node.Uri(SimpleModelingOrgOntology.definesVocabulary), Node.Uri(term))
    )
    modelRefs.foreach(m =>
      triples += Triple(subject, Node.Uri(SimpleModelingOrgOntology.includesModule), Node.Uri(m))
    )

    Graph(triples)
  }

  // ------------------------------------------------------------------
  // JSON-LD export (no json4s dependency)
  // ------------------------------------------------------------------
  def toJsonLD: String = {
    val context =
      s"""
         |{
         |  "sd": "${SmartDoxOntology.namespace}",
         |  "smorg": "${SimpleModelingOrgOntology.namespace}",
         |  "dcterms": "${Dcterms.namespace}",
         |  "glossary": "https://www.simplemodeling.org/glossary/ontology/0.1-SNAPSHOT#",
         |  "project": "https://www.simplemodeling.org/project/ontology/0.1-SNAPSHOT#",
         |  "category": "https://www.simplemodeling.org/category/ontology/0.1-SNAPSHOT#"
         |}
         |""".stripMargin.trim

    val triplesJson = toGraph.triples.map { t =>
      val subj = t.subject.toString
      val pred = t.predicate.toString
      val obj = t.obj match {
        case Node.Uri(u) => s""""$u""""
        case Node.Literal(v, _, Some(lang)) => s"""{"@value": "$v", "@language": "$lang"}"""
        case Node.Literal(v, Some(dt), _)   => s"""{"@value": "$v", "@type": "$dt"}"""
        case Node.Literal(v, _, _)          => s""""$v""""
        case Node.Blank(id)                 => s"""{"@id": "_:$id"}""" // handle blank node safely
      }
      s"""{"@id": "$subj", "predicate": "$pred", "object": $obj}"""
    }.mkString("[", ",", "]")

    s"""{
       |  "@context": $context,
       |  "@graph": $triplesJson
       |}""".stripMargin
  }
}

