package org.smartdox.semanticweb

/*
 * ComponentRepositoryOntology
 *
 * Ontology for describing reusable software/model components and their metadata.
 *
 * This ontology covers:
 *  - Components
 *  - Versions
 *  - Artifacts (Maven/NPM/etc.)
 *  - Capabilities (provided/required)
 *  - Dependencies
 *  - Download URLs and checksum digests
 *
 * @since   Nov. 28, 2025
 * @version Nov. 28, 2025
 * @author  ASAMI, Tomoharu
 */
object ComponentRepositoryOntology extends OntologyModel {

  override val prefix: String = Vocabulary.ComponentRepository.prefix
  override val namespace: String = Vocabulary.ComponentRepository.namespace

  // ------------------------------------------------------------
  // Classes
  // ------------------------------------------------------------
  val Component = uri("Component")
  val ComponentVersion = uri("ComponentVersion")
  val Artifact = uri("Artifact")
  val Capability = uri("Capability")

  // ------------------------------------------------------------
  // Properties
  // ------------------------------------------------------------
  val hasVersion = uri("hasVersion")
  val hasArtifact = uri("hasArtifact")
  val provides = uri("provides")
  val requires = uri("requires")
  val dependsOn = uri("dependsOn")

  // Artifact metadata
  val downloadUrl = uri("downloadUrl")
  val digest = uri("digest")

  /** JSON-LD context */
  override lazy val jsonldContext = Map(
    prefix -> namespace,
    "owl" -> Vocabulary.Owl.namespace,

    // Classes
    "Component" -> Component,
    "ComponentVersion" -> ComponentVersion,
    "Artifact" -> Artifact,
    "Capability" -> Capability,

    // Properties
    "hasVersion" -> hasVersion,
    "hasArtifact" -> hasArtifact,
    "provides" -> provides,
    "requires" -> requires,
    "dependsOn" -> dependsOn,
    "downloadUrl" -> downloadUrl,
    "digest" -> digest
  )

  // ------------------------------------------------------------
  // RDF triples (labels, types)
  // ------------------------------------------------------------
  def toTriples: Seq[Rdf.Triple] =
    Seq(
      // ----- Classes -----
      tripleClass(Component, "Component", "A reusable software or model component."),
      tripleClass(ComponentVersion, "ComponentVersion", "A specific version of a component."),
      tripleClass(Artifact, "Artifact", "A software artifact such as a Maven or NPM package."),
      tripleClass(Capability, "Capability", "A semantic capability provided or required by a component."),

      // ----- Properties -----
      tripleProperty(hasVersion, "hasVersion"),
      tripleProperty(hasArtifact, "hasArtifact"),
      tripleProperty(provides, "provides"),
      tripleProperty(requires, "requires"),
      tripleProperty(dependsOn, "dependsOn"),
      tripleProperty(downloadUrl, "downloadUrl"),
      tripleProperty(digest, "digest")
    ).flatten

  // ------------------------------------------------------------
  // Helpers
  // ------------------------------------------------------------
  private def tripleClass(uri: String, label: String, comment: String): Seq[Rdf.Triple] =
    Seq(
      Rdf.Triple(Rdf.Node.Uri(uri), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal(label)),
      Rdf.Triple(Rdf.Node.Uri(uri), Rdf.Node.Uri(Vocabulary.Rdfs.comment), Rdf.Node.Literal(comment)),
      Rdf.Triple(Rdf.Node.Uri(uri), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.Class))
    )

  private def tripleProperty(uri: String, label: String): Seq[Rdf.Triple] =
    Seq(
      Rdf.Triple(Rdf.Node.Uri(uri), Rdf.Node.Uri(Vocabulary.Rdfs.label), Rdf.Node.Literal(label)),
      Rdf.Triple(Rdf.Node.Uri(uri), Rdf.Node.Uri(Vocabulary.Rdf.`type`), Rdf.Node.Uri(Vocabulary.Owl.ObjectProperty))
    )
  object node {
    val Component         = Rdf.Node.Uri(ComponentRepositoryOntology.Component)
    val ComponentVersion  = Rdf.Node.Uri(ComponentRepositoryOntology.ComponentVersion)
    val Artifact          = Rdf.Node.Uri(ComponentRepositoryOntology.Artifact)
    val Capability        = Rdf.Node.Uri(ComponentRepositoryOntology.Capability)

    val hasVersion  = Rdf.Node.Uri(ComponentRepositoryOntology.hasVersion)
    val hasArtifact = Rdf.Node.Uri(ComponentRepositoryOntology.hasArtifact)
    val provides    = Rdf.Node.Uri(ComponentRepositoryOntology.provides)
    val requires    = Rdf.Node.Uri(ComponentRepositoryOntology.requires)
    val dependsOn   = Rdf.Node.Uri(ComponentRepositoryOntology.dependsOn)

    val downloadUrl = Rdf.Node.Uri(ComponentRepositoryOntology.downloadUrl)
    val digest      = Rdf.Node.Uri(ComponentRepositoryOntology.digest)
  }
}
