package org.smartdox.semanticweb

/*
 * DocumentModelOntology
 *
 * Defines the ontology for the Document Model layer:
 *   - Concept
 *   - KnowledgeUnit
 *   - Relation
 *   - Category (document classification)
 *
 * This ontology corresponds to SmartDox-based document modeling and knowledge documentation:
 * glossary terms, articles, relations, and category structures.
 *
 * @since   Nov. 28, 2025
 * @version Nov. 28, 2025
 * @author  ASAMI, Tomoharu
 */
object DocumentModelOntology extends OntologyModel {
  override val prefix: String = Vocabulary.DocumentModel.prefix
  override val namespace: String = Vocabulary.DocumentModel.namespace

  // ------------------------------------------------------------
  // Classes
  // ------------------------------------------------------------
  val Concept = uri("Concept")
  val KnowledgeUnit = uri("KnowledgeUnit")
  val Relation = uri("Relation")
  val Category = uri("Category")
  val DocumentModelRoot = uri("DocumentModelRoot")

  // ------------------------------------------------------------
  // Properties
  // ------------------------------------------------------------
  val relatesTo = uri("relatesTo")
  val references = uri("references")
  val representsDocument = uri("representsDocument")

  // Category relationships
  val hasCategory = uri("hasCategory")
  val inCategory = uri("inCategory")

  /** JSON-LD context */
  override lazy val jsonldContext = Map(
    prefix -> namespace,
    "owl" -> Vocabulary.Owl.namespace,
    // Classes
    "Concept" -> Concept,
    "KnowledgeUnit" -> KnowledgeUnit,
    "Relation" -> Relation,
    "Category" -> Category,
    "DocumentModelRoot" -> DocumentModelRoot,
    // Properties
    "relatesTo" -> relatesTo,
    "references" -> references,
    "representsDocument" -> representsDocument,
    "hasCategory" -> hasCategory,
    "inCategory" -> inCategory
  )

  // ------------------------------------------------------------
  // RDF triples (labels, comments, OWL typing)
  // ------------------------------------------------------------
  def toTriples: Seq[Rdf.Triple] = Seq(
    // Labels
    Rdf.Triple(Rdf.Node.Uri(Concept), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("Concept")),
    Rdf.Triple(Rdf.Node.Uri(KnowledgeUnit), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("KnowledgeUnit")),
    Rdf.Triple(Rdf.Node.Uri(Relation), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("Relation")),
    Rdf.Triple(Rdf.Node.Uri(Category), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("Category")),

    // Comments
    Rdf.Triple(
      Rdf.Node.Uri(Relation),
      Rdf.Node.Uri(Vocabulary.Rdfs.comment),
      Rdf.Node.Literal("A semantic relationship between knowledge units or concepts.")
    ),

    // Class declarations
    Rdf.Triple(Rdf.Node.Uri(Concept), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.Class)),
    Rdf.Triple(Rdf.Node.Uri(KnowledgeUnit), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.Class)),
    Rdf.Triple(Rdf.Node.Uri(Relation), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.Class)),
    Rdf.Triple(Rdf.Node.Uri(Category), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.Class)),
    Rdf.Triple(Rdf.Node.Uri(DocumentModelRoot), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("DocumentModelRoot")),
    Rdf.Triple(Rdf.Node.Uri(DocumentModelRoot), Rdf.Node.Uri(Vocabulary.Rdfs.comment), Rdf.Node.Literal("Root class for the SmartDox Document Model.")),
    Rdf.Triple(Rdf.Node.Uri(DocumentModelRoot), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.Class)),

    // Properties (labels + OWL.ObjectProperty)
    Rdf.Triple(Rdf.Node.Uri(relatesTo), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("relatesTo")),
    Rdf.Triple(Rdf.Node.Uri(relatesTo), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    Rdf.Triple(Rdf.Node.Uri(references), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("references")),
    Rdf.Triple(Rdf.Node.Uri(references), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    Rdf.Triple(Rdf.Node.Uri(representsDocument), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("representsDocument")),
    Rdf.Triple(Rdf.Node.Uri(representsDocument), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    // Category properties
    Rdf.Triple(Rdf.Node.Uri(hasCategory), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("hasCategory")),
    Rdf.Triple(Rdf.Node.Uri(hasCategory), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    Rdf.Triple(Rdf.Node.Uri(inCategory), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("inCategory")),
    Rdf.Triple(Rdf.Node.Uri(inCategory), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty))
  )
}
