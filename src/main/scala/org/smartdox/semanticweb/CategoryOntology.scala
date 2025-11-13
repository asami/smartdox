package org.smartdox.semanticweb

/*
 * Category Ontology
 * ----------------------------------------------------------------------
 * Defines the RDF/OWL vocabulary for BoK categories
 * (e.g., Domain Modeling, Software Architecture) within the
 * SimpleModeling knowledge system.
 *
 * @since   Nov. 13, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object CategoryOntology {
  val prefix = "category"
  val namespace = "https://www.simplemodeling.org/category/ontology/1.0#"
  def uri(local: String) = namespace + local

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val Category     = uri("Category")     // A major BoK category
  val Subcategory  = uri("Subcategory")  // A subcategory within a Category
  val Article      = uri("Article")      // An article belonging to a Category
  val Topic        = uri("Topic")        // A conceptual topic
  val Tag          = uri("Tag")          // Classification or search tag

  // ------------------------------------------------------------------
  // Properties
  // ------------------------------------------------------------------
  val hasSubcategory = uri("hasSubcategory") // Category → Subcategory
  val hasArticle     = uri("hasArticle")     // Category → Article
  val hasTopic       = uri("hasTopic")       // Category → Topic
  val hasTag         = uri("hasTag")         // Category → Tag
  val relatedTo      = uri("relatedTo")      // Category → Category
  val title          = uri("title")          // Human-readable title
  val description    = uri("description")    // Category description
  val order          = uri("order")          // Display order
  val parentCategory = uri("parentCategory") // Subcategory → Category

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  lazy val jsonldContext: Map[String, Any] = Map(
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
}
