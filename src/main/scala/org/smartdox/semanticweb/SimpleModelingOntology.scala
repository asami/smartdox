package org.smartdox.semanticweb

/*
 * SimpleModeling Ontology
 * ----------------------------------------------------------------------
 * Defines the conceptual framework of the SimpleModeling methodology:
 * Literate Model-Driven Development (LMDD), Object-Functional Modeling,
 * and Component-Based Software Development (CBSD).
 *
 * Namespace: https://www.simplemodeling.org/simplemodeling/ontology/1.0#
 * Prefix: smont
 *
 * @since   Nov. 12, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object SimpleModelingOntology {
  val prefix = "smont"
  val namespace = "https://www.simplemodeling.org/simplemodeling/ontology/1.0#"
  def uri(local: String) = namespace + local

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
  val SimpleModelOntology = "https://www.simplemodeling.org/simplemodel/ontology/1.0#"

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  lazy val jsonldContext: Map[String, Any] = Map(
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
}
