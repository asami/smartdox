package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * Bok Schema (Body of Knowledge Schema)
 *
 * Defines RDF-level representation for integrated knowledge
 * across SimpleModeling and SmartDox ecosystems.
 *
 * It combines conceptual, documentary, and operational knowledge
 * from projects, entities, rules, and site-level documents
 * into a unified semantic graph.
 *
 * @since   Nov. 12, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object BokSchema extends SchemaModel {
  //
  // Schema-level namespace (separate from BokOntology)
  //
  override val prefix: String = Vocabulary.BokSchema.prefix
  override val namespace: String = Vocabulary.BokSchema.namespace

  //
  // JSON-LD context for schema export
  //
  override lazy val jsonldContext: Map[String, Any] = Map(
    // Core vocabularies
    "rdf"     -> Vocabulary.Rdf.namespace,
    "rdfs"    -> Vocabulary.Rdfs.namespace,
    "dcterms" -> Vocabulary.Dcterms.namespace,
    "sm"      -> SimpleModelOntology.namespace,
    // Schema namespace
    prefix    -> namespace,
    // Logical schema terms (Concept, KnowledgeUnit, Relation)
    "Concept"        -> uri("Concept"),
    "KnowledgeUnit"  -> uri("KnowledgeUnit"),
    "Relation"       -> uri("Relation")
  )

  /** Represents a conceptual element (domain concept, entity, or value). */
  case class Concept(
    id: String,
    label: String,
    kind: String,
    description: Option[String] = None,
    related: Seq[String] = Seq.empty
  ) {
    def toTriples: Seq[Triple] = {
      val s = Node.Uri(id)
      val base = Seq(
        Triple(s, RdfType, SimpleModelOntology.node.Entity),
        Triple(s, Rdfs.node.label, Node.Literal(label)),
        Triple(s, Dcterms.node.type_, Node.Literal(kind))
      )
      val desc = description.map(v => Triple(s, Rdfs.node.comment, Node.Literal(v)))
      val rels = related.map(r => Triple(s, Dcterms.node.relation, Node.Uri(r)))
      base ++ desc ++ rels
    }
  }

  /** Represents a knowledge unit (SmartDox document, model, or resource). */
  case class KnowledgeUnit(
    id: String,
    title: String,
    category: Option[String] = None,
    language: Option[String] = None,
    tags: Seq[String] = Seq.empty,
    modelRefs: Seq[String] = Seq.empty,
    relatedConcepts: Seq[String] = Seq.empty
  ) {
    def toTriples: Seq[Triple] = {
      val s = Node.Uri(id)
      val base = Seq(
        Triple(s, RdfType, Dcterms.node.BibliographicResource),
        Triple(s, Dcterms.node.title, Node.Literal(title))
      )
      val opt = Seq(
        category.map(v => Triple(s, Dcterms.node.subject, Node.Literal(v))),
        language.map(v => Triple(s, Dcterms.node.language, Node.Literal(v)))
      ).flatten
      val tagsTriples = tags.map(t => Triple(s, SimpleModelOntology.node.tag, Node.Literal(t)))
      val models = modelRefs.map(m => Triple(s, Dcterms.node.relation, Node.Uri(m)))
      val conceptLinks = relatedConcepts.map(c => Triple(s, Dcterms.node.subject, Node.Uri(c)))
      base ++ opt ++ tagsTriples ++ models ++ conceptLinks
    }
  }

  /** Represents a relation between knowledge entities (Bok-specific edges). */
  case class Relation(
    subject: String,
    predicate: String,
    obj: String
  ) {
    def toTriple: Triple =
      Triple(Node.Uri(subject), Node.Uri(predicate), Node.Uri(obj))
  }

  /** Represents the full Body of Knowledge model as an RDF Graph. */
  case class BokModel(
    concepts: Seq[Concept] = Seq.empty,
    knowledgeUnits: Seq[KnowledgeUnit] = Seq.empty,
    relations: Seq[Relation] = Seq.empty
  ) {
    def toGraph: Graph = {
      val triples =
        concepts.flatMap(_.toTriples) ++
        knowledgeUnits.flatMap(_.toTriples) ++
        relations.map(_.toTriple)
      Graph(triples)
    }
  }

  //
  // Static schema export (for DoxSite)
  //
  // For now, we export an empty BokModel with schema context.
  // Later you can change this to include schema-level instances
  // (e.g. predefined Concept/KnowledgeUnit templates).
  //

  private def emptyModel: BokModel = BokModel()
}
