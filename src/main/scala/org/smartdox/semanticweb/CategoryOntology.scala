package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * Category Ontology
 * ----------------------------------------------------------------------
 * Defines the RDF/OWL vocabulary for BoK categories
 * (e.g., Domain Modeling, Software Architecture) within the
 * SimpleModeling knowledge system.
 *
 * @since   Nov. 13, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object CategoryOntology extends OntologyModel {
  override val prefix = Vocabulary.Category.prefix
  override val namespace = Vocabulary.Category.namespace

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val Category     = Vocabulary.Category.uri("Category")     // Top-level category
  val Subcategory  = Vocabulary.Category.uri("Subcategory")  // Nested category
  val Article      = Vocabulary.Category.uri("Article")      // Article belonging to a category
  val Topic        = Vocabulary.Category.uri("Topic")        // Conceptual topic
  val Tag          = Vocabulary.Category.uri("Tag")          // Classification tag

  // ------------------------------------------------------------------
  // Properties
  // ------------------------------------------------------------------
  val hasSubcategory = Vocabulary.Category.uri("hasSubcategory")
  val hasArticle     = Vocabulary.Category.uri("hasArticle")
  val hasTopic       = Vocabulary.Category.uri("hasTopic")
  val hasTag         = Vocabulary.Category.uri("hasTag")
  val relatedTo      = Vocabulary.Category.uri("relatedTo")
  val title          = Vocabulary.Category.uri("title")
  val description    = Vocabulary.Category.uri("description")
  val order          = Vocabulary.Category.uri("order")
  val parentCategory = Vocabulary.Category.uri("parentCategory")

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  override lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "Category" -> Category,
    "Subcategory" -> Subcategory,
    "Article" -> Article,
    "Topic" -> Topic,
    "Tag" -> Tag,
    "hasSubcategory" -> hasSubcategory,
    "hasArticle" -> hasArticle,
    "hasTopic" -> hasTopic,
    "hasTag" -> hasTag,
    "relatedTo" -> relatedTo,
    "title" -> title,
    "description" -> description,
    "order" -> order,
    "parentCategory" -> parentCategory
  )

  // ------------------------------------------------------------------
  // TBox: Core class/property declarations
  // ------------------------------------------------------------------
  lazy val toTriples: Seq[Triple] = Seq(
    // Classes
    Triple(Node.Uri(Category),        RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Subcategory),     RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Article),         RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Topic),           RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Tag),             RdfType, Node.Uri(Owl.Class)),

    // Object Properties
    Triple(Node.Uri(hasSubcategory),  RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasArticle),      RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasTopic),        RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasTag),          RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(relatedTo),       RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(parentCategory),  RdfType, Node.Uri(Owl.ObjectProperty)),

    // Datatype Properties
    Triple(Node.Uri(title),           RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(description),     RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(order),           RdfType, Node.Uri(Owl.DatatypeProperty))
  )
}
