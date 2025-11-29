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
 * @version Nov. 29, 2025
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
  val mentions = uri("mentions")
  val mentionedIn = uri("mentionedIn")

  val aboutTerm = uri("aboutTerm")
  val aboutTermOf = uri("aboutTermOf")
  val definesTerm = uri("definesTerm")
  val definedIn = uri("definedIn")
  val referencesTerm = uri("referencesTerm")
  val usesTerm = uri("usesTerm")
  val usesTermOf = uri("usesTermOf")

  object node {
    val Concept = Rdf.Node.Uri(DocumentModelOntology.Concept)
    val KnowledgeUnit = Rdf.Node.Uri(DocumentModelOntology.KnowledgeUnit)
    val Relation = Rdf.Node.Uri(DocumentModelOntology.Relation)
    val Category = Rdf.Node.Uri(DocumentModelOntology.Category)
    val DocumentModelRoot = Rdf.Node.Uri(DocumentModelOntology.DocumentModelRoot)

    val relatesTo = Rdf.Node.Uri(DocumentModelOntology.relatesTo)
    val references = Rdf.Node.Uri(DocumentModelOntology.references)
    val representsDocument = Rdf.Node.Uri(DocumentModelOntology.representsDocument)
    val hasCategory = Rdf.Node.Uri(DocumentModelOntology.hasCategory)
    val inCategory = Rdf.Node.Uri(DocumentModelOntology.inCategory)
    val mentions = Rdf.Node.Uri(DocumentModelOntology.mentions)
    val mentionedIn = Rdf.Node.Uri(DocumentModelOntology.mentionedIn)

    val aboutTerm = Rdf.Node.Uri(DocumentModelOntology.aboutTerm)
    val aboutTermOf = Rdf.Node.Uri(DocumentModelOntology.aboutTermOf)
    val definesTerm = Rdf.Node.Uri(DocumentModelOntology.definesTerm)
    val definedIn = Rdf.Node.Uri(DocumentModelOntology.definedIn)
    val referencesTerm = Rdf.Node.Uri(DocumentModelOntology.referencesTerm)
    val usesTerm = Rdf.Node.Uri(DocumentModelOntology.usesTerm)
    val usesTermOf = Rdf.Node.Uri(DocumentModelOntology.usesTermOf)
  }

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
    "inCategory" -> inCategory,
    "mentions" -> mentions,
    "mentionedIn" -> mentionedIn,
    "aboutTerm" -> aboutTerm,
    "aboutTermOf" -> aboutTermOf,
    "definesTerm" -> definesTerm,
    "definedIn" -> definedIn,
    "referencesTerm" -> referencesTerm,
    "usesTerm" -> usesTerm,
    "usesTermOf" -> usesTermOf
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
    Rdf.Triple(Rdf.Node.Uri(inCategory), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    Rdf.Triple(Rdf.Node.Uri(mentions), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("mentions")),
    Rdf.Triple(Rdf.Node.Uri(mentions), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    Rdf.Triple(Rdf.Node.Uri(mentionedIn), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("mentionedIn")),
    Rdf.Triple(Rdf.Node.Uri(mentionedIn), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    Rdf.Triple(Rdf.Node.Uri(aboutTerm), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("aboutTerm")),
    Rdf.Triple(Rdf.Node.Uri(aboutTerm), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Rdf.Triple(
      Rdf.Node.Uri(aboutTerm),
      Rdf.Node.Uri(Vocabulary.Owl.inverseOf),
      Rdf.Node.Uri(aboutTermOf)
    ),

    Rdf.Triple(Rdf.Node.Uri(aboutTermOf), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("aboutTermOf")),
    Rdf.Triple(Rdf.Node.Uri(aboutTermOf), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    Rdf.Triple(Rdf.Node.Uri(definesTerm), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("definesTerm")),
    Rdf.Triple(Rdf.Node.Uri(definesTerm), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Rdf.Triple(
      Rdf.Node.Uri(definesTerm),
      Rdf.Node.Uri(Vocabulary.Owl.inverseOf),
      Rdf.Node.Uri(definedIn)
    ),

    Rdf.Triple(Rdf.Node.Uri(definedIn), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("definedIn")),
    Rdf.Triple(Rdf.Node.Uri(definedIn), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    Rdf.Triple(Rdf.Node.Uri(referencesTerm), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("referencesTerm")),
    Rdf.Triple(Rdf.Node.Uri(referencesTerm), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),

    Rdf.Triple(Rdf.Node.Uri(usesTerm), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("usesTerm")),
    Rdf.Triple(Rdf.Node.Uri(usesTerm), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Rdf.Triple(Rdf.Node.Uri(usesTerm), Rdf.Node.Uri(Vocabulary.Owl.inverseOf), Rdf.Node.Uri(usesTermOf)),

    Rdf.Triple(Rdf.Node.Uri(usesTermOf), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("usesTermOf")),
    Rdf.Triple(Rdf.Node.Uri(usesTermOf), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),
  )
}
