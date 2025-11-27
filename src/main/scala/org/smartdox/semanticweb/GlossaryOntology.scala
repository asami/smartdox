package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * Glossary Ontology (TBox)
 * ----------------------------------------------------------------------
 * Defines RDF/OWL vocabulary for glossary terms used in the
 * SimpleModeling.org knowledge ecosystem.
 *
 * This ontology provides:
 *  - RDF/OWL class definitions (Glossary, Term)
 *  - RDF/OWL property definitions (hasTerm, hasDefinition, synonymOf, etc.)
 *
 * NOTE:
 *  - Instance (ABox) construction logic is intentionally NOT included here.
 *    It has been moved to GlossarySchema to maintain a clean TBox/ABox separation.
 *
 * @since   Nov. 13, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object GlossaryOntology extends OntologyModel {
  val prefix = Vocabulary.Glossary.prefix
  val namespace = Vocabulary.Glossary.namespace

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val Glossary     = Vocabulary.Glossary.uri("Glossary")
  val Term         = Vocabulary.Glossary.uri("Term")

  // ------------------------------------------------------------------
  // Properties
  // ------------------------------------------------------------------
  val hasTerm        = Vocabulary.Glossary.uri("hasTerm")
  val hasDefinition  = Vocabulary.Glossary.uri("hasDefinition")
  val synonymOf      = Vocabulary.Glossary.uri("synonymOf")
  val relatedTo      = Vocabulary.Glossary.uri("relatedTo")
  val language       = Vocabulary.Glossary.uri("language")
  val example        = Vocabulary.Glossary.uri("example")
  val category       = Vocabulary.Glossary.uri("category")

  override lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "Glossary" -> Glossary,
    "Term" -> Term,
    "hasTerm" -> hasTerm,
    "hasDefinition" -> hasDefinition,
    "synonymOf" -> synonymOf,
    "relatedTo" -> relatedTo,
    "language" -> language,
    "example" -> example,
    "category" -> category
  )

  // Base schema triples for this ontology (can be extended later)
  lazy val toTriples: Seq[Triple] = Seq(
    // Class definitions
    Triple(Node.Uri(Glossary), RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Term),     RdfType, Node.Uri(Owl.Class)),

    // Property definitions
    Triple(Node.Uri(hasTerm),       RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasDefinition), RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(synonymOf),     RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(relatedTo),     RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(language),      RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(example),       RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(category),      RdfType, Node.Uri(Owl.ObjectProperty))
  )
}
