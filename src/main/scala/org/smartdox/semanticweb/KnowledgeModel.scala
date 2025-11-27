package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._

/**
 * Common trait for Ontology-like and Schema-like models.
 *
 * A KnowledgeModel is a top-level semantic asset consisting of:
 *   - namespace (IRI)
 *   - prefix (CURIE prefix)
 *   - RDF graph describing the model
 *   - JSON-LD context for export
 *   - JSON-LD / Turtle renderers
 *
 * @since   Nov. 20, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
trait KnowledgeModel {

  /** CURIE prefix (e.g., "bok", "sm", "smorg") */
  def prefix: String

  /** Namespace (IRI ending with # or /) */
  def namespace: String

  /** The root node of the model (e.g., Ontology IRI) */
  def root: Node.Uri = Node.Uri(namespace)

  /** Build full IRI from local name */
  def uri(local: String): String = namespace + local

  /** RDF triples representing this model (TBox/RBox for ontology, ABox for schema) */
  def toTriples: Seq[Triple]

  /** Graph is derived from triples */
  final def toGraph: Graph = Graph(toTriples.toVector)

  /** JSON-LD profile (default BoK) */
  def jsonldProfile: RdfRenderer.JsonLDProfile = RdfRenderer.JsonLDProfile.BoK

  /** JSON-LD @context (default empty, override in SchemaModel) */
  def jsonldContext: Map[String, Any] = Map.empty

  /** Export this model as JSON-LD */
  def asJsonLD: String =
    RdfRenderer.toJsonLD(
      toGraph,
      jsonldProfile,
      userContext = jsonldContext
    )

  /** Export this model as Turtle */
  def asTurtle: String =
    RdfRenderer.toTurtle(
      toGraph,
      jsonldProfile
    )
}
