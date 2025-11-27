package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._

/*
 * SimpleModeling Ontology
 * ----------------------------------------------------------------------
 * Defines the conceptual framework of the SimpleModeling methodology:
 * Literate Model-Driven Development (LMDD), Object-Functional Modeling,
 * and Component-Based Software Development (CBSD).
 *
 * Namespace: https://www.simplemodeling.org/simplemodeling/ontology/0.1-SNAPSHOT#
 * Prefix: smont
 *
 * @since   Nov. 12, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object SimpleModelingOntology extends OntologyModel {
  override val prefix: String = Vocabulary.SimpleModeling.prefix
  override val namespace: String = Vocabulary.SimpleModeling.namespace

  // ------------------------------------------------------------------
  // Core Concepts of Methodology
  // ------------------------------------------------------------------
  val Methodology     = uri("Methodology")
  val Process         = uri("Process")
  val Phase           = uri("Phase")
  val Artifact        = uri("Artifact")
  val Model           = uri("Model")
  val MetaModel       = uri("MetaModel")
  val KnowledgeFlow   = uri("KnowledgeFlow")

  // ------------------------------------------------------------------
  // Relationships
  // ------------------------------------------------------------------
  val definesModel    = uri("definesModel")     // Methodology → Model
  val usesMetaModel   = uri("usesMetaModel")    // Model → MetaModel
  val hasPhase        = uri("hasPhase")         // Process → Phase
  val producesArtifact= uri("producesArtifact") // Process → Artifact
  val governsProcess  = uri("governsProcess")   // Methodology → Process

  // ------------------------------------------------------------------
  // Integration with SimpleModel (Object-Functional Model)
  // ------------------------------------------------------------------
  val SimpleModelOntology = "https://www.simplemodeling.org/simplemodeling/ontology/0.1-SNAPSHOT#"

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  override lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "Methodology" -> Methodology,
    "Process" -> Process,
    "Phase" -> Phase,
    "Artifact" -> Artifact,
    "Model" -> Model,
    "MetaModel" -> MetaModel,
    "definesModel" -> definesModel,
    "usesMetaModel" -> usesMetaModel,
    "governsProcess" -> governsProcess
  )
  override def toTriples: Seq[Triple] = Seq.empty
}
