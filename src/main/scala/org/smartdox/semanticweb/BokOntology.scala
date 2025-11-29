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
  val BoK = uri("BoK")
  val ComponentRepository = uri("ComponentRepository")

  // Properties
  val includesBoK = uri("includesBoK")
  val includesDocumentModel = uri("includesDocumentModel")
  val includesSimpleModel = uri("includesSimpleModel")
  val includesComponentRepository = uri("includesComponentRepository")
  val documents = uri("documents")

  /** JSON-LD context */
  override lazy val jsonldContext = Map(
    prefix -> namespace,
    "owl" -> Vocabulary.Owl.namespace,
    "BoK" -> BoK,
    "ComponentRepository" -> ComponentRepository,
    "includesBoK" -> includesBoK,
    "includesDocumentModel" -> includesDocumentModel,
    "includesSimpleModel" -> includesSimpleModel,
    "includesComponentRepository" -> includesComponentRepository,
    "documents" -> documents,
  )

  //
  // RDF Graph generation
  //
  def toTriples: Seq[Rdf.Triple] = Seq(
    // Labels
    Rdf.Triple(Rdf.Node.Uri(BoK), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("BoK")),
    Rdf.Triple(
      Rdf.Node.Uri(BoK),
      Rdf.Node.Uri(Vocabulary.Rdfs.comment),
      Rdf.Node.Literal("The root class representing the SimpleModeling Body of Knowledge.")
    ),

    Rdf.Triple(Rdf.Node.Uri(BoK), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.Class)),

    Rdf.Triple(Rdf.Node.Uri(ComponentRepository), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("ComponentRepository")),
    Rdf.Triple(Rdf.Node.Uri(ComponentRepository), Rdf.Node.Uri(Vocabulary.Rdfs.comment), Rdf.Node.Literal("Repository of reusable model and software components.")),
    Rdf.Triple(Rdf.Node.Uri(ComponentRepository), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.Class)),

    // Properties (labels)
    Rdf.Triple(Rdf.Node.Uri(includesDocumentModel), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("includesDocumentModel")),
    Rdf.Triple(Rdf.Node.Uri(includesDocumentModel), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Rdf.Triple(Rdf.Node.Uri(includesSimpleModel), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("includesSimpleModel")),
    Rdf.Triple(Rdf.Node.Uri(includesSimpleModel), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Rdf.Triple(Rdf.Node.Uri(includesComponentRepository), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("includesComponentRepository")),
    Rdf.Triple(Rdf.Node.Uri(includesComponentRepository), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Rdf.Triple(Rdf.Node.Uri(documents), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal("documents")),
    Rdf.Triple(Rdf.Node.Uri(documents), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty))
  )
}
