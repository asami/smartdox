package org.smartdox.semanticweb

/*
 * @since   Nov. 12, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object BokOntology extends OntologyModel {
  override val prefix: String = Vocabulary.Bok.prefix
  override val namespace: String = Vocabulary.Bok.namespace

  // Classes
  val Concept = uri("Concept")
  val KnowledgeUnit = uri("KnowledgeUnit")
  val Relation = uri("Relation")

  // Properties
  val relatesTo = uri("relatesTo")
  val references = uri("references")
  val representsDocument = uri("representsDocument")

  /** JSON-LD context */
  override lazy val jsonldContext = Map(
    prefix -> namespace,
    "Concept" -> Concept,
    "KnowledgeUnit" -> KnowledgeUnit,
    "Relation" -> Relation,
    "relatesTo" -> relatesTo,
    "references" -> references,
    "representsDocument" -> representsDocument
  )

  //
  // RDF Graph generation
  //
  def toTriples: Seq[Rdf.Triple] = Seq(
    // Labels
    Rdf.Triple(Rdf.Node.Uri(Concept), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("Concept")),
    Rdf.Triple(Rdf.Node.Uri(KnowledgeUnit), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("KnowledgeUnit")),
    Rdf.Triple(Rdf.Node.Uri(Relation), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("Relation")),

    // Class comment (example)
    Rdf.Triple(
      Rdf.Node.Uri(Relation),
      Rdf.Node.Uri(Vocabulary.Rdfs.comment),
      Rdf.Node.Literal("A semantic relation between knowledge units.")
    ),

    // Properties (labels)
    Rdf.Triple(Rdf.Node.Uri(relatesTo), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("relatesTo")),
    Rdf.Triple(Rdf.Node.Uri(references), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("references")),
    Rdf.Triple(Rdf.Node.Uri(representsDocument), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("representsDocument"))
  )
}
