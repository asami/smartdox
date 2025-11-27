package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._

/**
 * OntologyModel
 * --------------------------------------------------------------------
 * Represents a TBox/RBox semantic model (classes, properties, axioms).
 * Ontologies define *meaning*, not physical JSON-LD shapes.
 *
 * Differences from SchemaModel:
 *  - Pure OWL/RDFS semantics only
 *
 * @since   Nov. 27, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
trait OntologyModel extends KnowledgeModel {
  /** Ontology must supply TBox/RBox triples */
  override def toTriples: Seq[Triple]

  /** Ontology does not provide JSON-LD context */
  override def jsonldContext: Map[String, Any] = Map.empty
}
