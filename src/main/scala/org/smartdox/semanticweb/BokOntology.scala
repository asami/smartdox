package org.smartdox.semanticweb

/*
 * @since   Nov. 12, 2025
 * @version Nov. 12, 2025
 * @author  ASAMI, Tomoharu
 */
object BokOntology {
  val prefix = "bok"
  val namespace = "https://www.simplemodeling.org/bok/ontology/1.0#"
  def uri(local: String) = namespace + local

  // Classes
  val Concept = uri("Concept")
  val KnowledgeUnit = uri("KnowledgeUnit")
  val Relation = uri("Relation")

  // Properties
  val relatesTo = uri("relatesTo")
  val references = uri("references")
  val representsDocument = uri("representsDocument")

  lazy val jsonldContext = Map(
    prefix -> namespace,
    "Concept" -> Concept,
    "KnowledgeUnit" -> KnowledgeUnit,
    "Relation" -> Relation,
    "relatesTo" -> relatesTo,
    "references" -> references,
    "representsDocument" -> representsDocument
  )
}
