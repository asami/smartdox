package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/**
 * Glossary Schema
 * ------------------------------------------------------------------
 * Defines the *physical representation* (JSON-LD & Turtle shape)
 * of Glossary-related ABox data. This schema translates SmartDox
 * glossary information into RDF triples according to the conceptual
 * model defined in GlossaryOntology.
 *
 * Responsibilities:
 *  - JSON-LD context definition
 *  - Node URI generation (term/glossary)
 *  - RDF triple generation for Glossary/Term ABox
 *
 * @since   Nov. 27, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI
 */
object GlossarySchema extends SchemaModel {

  // ------------------------------------------------------------------
  // Prefix & Namespace (via Vocabulary)
  // ------------------------------------------------------------------
  val prefix: String    = Vocabulary.Glossary.prefix
  val namespace: String = Vocabulary.Glossary.namespace

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  override lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,

    // Classes
    "Glossary"     -> Vocabulary.Glossary.uri("Glossary"),
    "Term"         -> Vocabulary.Glossary.uri("Term"),

    // Properties
    "hasTerm"        -> Vocabulary.Glossary.uri("hasTerm"),
    "hasDefinition"  -> Vocabulary.Glossary.uri("hasDefinition"),
    "synonymOf"      -> Vocabulary.Glossary.uri("synonymOf"),
    "relatedTo"      -> Vocabulary.Glossary.uri("relatedTo"),
    "language"       -> Vocabulary.Glossary.uri("language"),
    "example"        -> Vocabulary.Glossary.uri("example"),
    "category"       -> Vocabulary.Glossary.uri("category")
  )

  // ------------------------------------------------------------------
  // Node URI Generators
  // ------------------------------------------------------------------

  /** Glossary node URI: glossary/<id> */
  def glossaryNode(glossaryId: String): Node.Uri =
    Node.Uri(s"${namespace}glossary/$glossaryId")

  /** Term node URI: term/<id> */
  def termNode(termId: String): Node.Uri =
    Node.Uri(s"${namespace}term/$termId")

  // ------------------------------------------------------------------
  // RDF Triple Builders (ABox)
  // ------------------------------------------------------------------

  /** Glossary → Term links */
  def glossaryTriples(glossaryId: String, termIds: Seq[String]): Seq[Triple] = {
    val s = glossaryNode(glossaryId)
    val header = Triple(s, RdfType, Node.Uri(Vocabulary.Glossary.uri("Glossary")))
    val links = termIds.map(tid =>
      Triple(s, Node.Uri(Vocabulary.Glossary.uri("hasTerm")), termNode(tid))
    )
    header +: links
  }

  /** Term → definition, synonym, related, etc. */
  def termTriples(
    termId: String,
    definition: String,
    lang: Option[String] = None,
    synonyms: Seq[String] = Seq.empty,
    related: Seq[String] = Seq.empty,
    exampleText: Option[String] = None,
    categoryId: Option[String] = None
  ): Seq[Triple] = {

    val s = termNode(termId)

    val tType = Triple(s, RdfType, Node.Uri(Vocabulary.Glossary.uri("Term")))
    val tDef  = Triple(s,
      Node.Uri(Vocabulary.Glossary.uri("hasDefinition")),
      Node.Literal(definition, None, lang)
    )

    val tSyn = synonyms.map(id =>
      Triple(s, Node.Uri(Vocabulary.Glossary.uri("synonymOf")), termNode(id))
    )

    val tRel = related.map(id =>
      Triple(s, Node.Uri(Vocabulary.Glossary.uri("relatedTo")), termNode(id))
    )

    val tEx  = exampleText.toSeq.map(ex =>
      Triple(s, Node.Uri(Vocabulary.Glossary.uri("example")), Node.Literal(ex))
    )

    val tCat = categoryId.toSeq.map(id =>
      Triple(s, Node.Uri(Vocabulary.Glossary.uri("category")), Node.Uri(id))
    )

    Seq(tType, tDef) ++ tSyn ++ tRel ++ tEx ++ tCat
  }

  // ------------------------------------------------------------------
  // Profile / Graph (no TBox content here)
  // ------------------------------------------------------------------
}
