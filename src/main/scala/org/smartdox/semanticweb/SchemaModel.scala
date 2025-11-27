package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._

/**
 * SchemaModel
 * --------------------------------------------------------------------
 * ABox shape / JSON-LD context / Turtle generation rules.
 * Schemas define how instances (ABox) should be serialized.
 *
 * Responsibilities:
 *  - JSON-LD @context
 *  - Node URI builders (termNode, categoryNode, etc.)
 *  - Triple builders for ABox
 *
 * @since   Nov. 27, 2025
 * @version Nov. 27, 2025
 */
trait SchemaModel extends KnowledgeModel {

  /**
   * ABox triples for this schema.
   * Default: empty ABox (Schema has no instance-level data by itself).
   *
   * Each schema (GlossarySchema, CategorySchema, BibliographySchema, …)
   * overrides this to generate its own ABox triple set.
   */
  override def toTriples: Seq[Triple] = Seq.empty

  /**
   * Schemas typically provide JSON-LD @context for instances.
   * Override in each concrete schema as needed.
   */
  override def jsonldContext: Map[String, Any] = Map.empty
}
