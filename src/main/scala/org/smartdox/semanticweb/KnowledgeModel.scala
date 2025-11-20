package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.RdfRenderer.JsonLDProfile
import org.smartdox.semanticweb.RdfRenderer.Policy

/**
 * Common trait for Ontology-like and Schema-like models.
 *
 * A KnowledgeModel is a top-level semantic asset consisting of:
 *   - namespace (IRI)
 *   - prefix (CURIE prefix)
 *   - RDF graph describing the model
 *   - JSON-LD context for export
 *   - JSON-LD / Turtle renderers
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

  /** RDF graph representing this model */
  def toGraph: Graph

  /** JSON-LD @context (prefix → namespace) */
  def jsonldContext: Map[String, Any]

  /** Default JSON-LD rendering policy */
  def defaultPolicy: Policy = RdfRenderer.Policy.default

  def jsonldProfile: RdfRenderer.JsonLDProfile

  /** JSON-LD export */
  def asJsonLD: String =
    RdfRenderer.toJsonLD(
      toGraph,
      jsonldProfile,
      userContext = jsonldContext,
      policy = defaultPolicy
    )

  /** Turtle export */
  def asTurtle: String =
    RdfRenderer.toTurtle(
      toGraph,
      jsonldProfile
    )
}
