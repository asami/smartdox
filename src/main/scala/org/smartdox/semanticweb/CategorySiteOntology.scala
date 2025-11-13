package org.smartdox.semanticweb

/*
 * Category Site Ontology
 * ----------------------------------------------------------------------
 * Defines the RDF/OWL vocabulary for the category site layer
 * in the SimpleModeling BoK publication.
 *
 * @since   Nov. 13, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object CategorySiteOntology {
  val prefix = "categorysite"
  val namespace = "https://www.simplemodeling.org/category-site/ontology/1.0#"
  def uri(local: String) = namespace + local

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val CategorySite = uri("CategorySite")   // カテゴリサイト全体
  val CategoryPage = uri("CategoryPage")   // 各カテゴリページ
  val CategoryItem = uri("CategoryItem")   // 個別の記事またはBoK項目

  // ------------------------------------------------------------------
  // Properties
  // ------------------------------------------------------------------
  val hasCategory   = uri("hasCategory")   // CategorySite → CategoryPage
  val hasItem       = uri("hasItem")       // CategoryPage → CategoryItem
  val represents    = uri("represents")    // CategoryItem → BoK Concept
  val relatedTo     = uri("relatedTo")     // CategoryItem → CategoryItem
  val taggedWith    = uri("taggedWith")    // CategoryItem → Tag (Glossaryなど)
  val order         = uri("order")         // 表示順序
  val title         = uri("title")         // 表題
  val description   = uri("description")   // 説明文

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "CategorySite" -> CategorySite,
    "CategoryPage" -> CategoryPage,
    "CategoryItem" -> CategoryItem,
    "hasCategory"  -> hasCategory,
    "hasItem"      -> hasItem,
    "represents"   -> represents,
    "relatedTo"    -> relatedTo,
    "taggedWith"   -> taggedWith,
    "order"        -> order,
    "title"        -> title,
    "description"  -> description
  )
}
