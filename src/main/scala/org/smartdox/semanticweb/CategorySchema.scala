package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.semanticweb.CategoryOntology._

/*
 * Category Schema
 * ----------------------------------------------------------------------
 * Defines the RDF/JSON-LD structural schema for BoK categories.
 * Used by SmartDox and SimpleModeling to generate structured
 * JSON-LD or Turtle representations of each category page.
 *
 * @since   Nov. 13, 2025
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
object CategorySchema extends KnowledgeModel {
  override val prefix: String = "categorySchema"
  override val namespace: String = "https://www.simplemodeling.org/category/schema/0.1-SNAPSHOT#"

  override lazy val jsonldContext: Map[String, Any] =
    CategoryOntology.jsonldContext

  override def jsonldProfile: RdfRenderer.JsonLDProfile =
    RdfRenderer.JsonLDProfile.BoK

  // Base triples for this schema (will be populated via toGraph(category…))
  lazy val triples: Seq[Triple] = Seq.empty

  override def toGraph: Rdf.Graph = Rdf.Graph(triples.toVector)
  // ------------------------------------------------------------------
  // Node Builders
  // ------------------------------------------------------------------
  def categoryNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def articleNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def topicNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def tagNode(uriStr: String): Node.Uri = Node.Uri(uriStr)

  // ------------------------------------------------------------------
  // Category Triples
  // ------------------------------------------------------------------
  def categoryTriples(
    categoryId: String,
    title: String,
    description: String,
    order: Option[Int] = None,
    subcategories: Seq[String] = Seq.empty,
    articles: Seq[String] = Seq.empty,
    topics: Seq[String] = Seq.empty,
    tags: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = categoryNode(categoryId)

    val orderTriples = order.map(o =>
      Triple(subject, Node.Uri(CategoryOntology.order), Node.Literal(o.toString))
    ).toSeq

    val subTriples = subcategories.map(sid =>
      Triple(subject, Node.Uri(hasSubcategory), categoryNode(sid))
    )

    val articleTriples = articles.map(aid =>
      Triple(subject, Node.Uri(hasArticle), articleNode(aid))
    )

    val topicTriples = topics.map(tid =>
      Triple(subject, Node.Uri(hasTopic), topicNode(tid))
    )

    val tagTriples = tags.map(tid =>
      Triple(subject, Node.Uri(hasTag), tagNode(tid))
    )

    Seq(
      Triple(subject, RdfType, Node.Uri(Category)),
      Triple(subject, Node.Uri(CategoryOntology.title), Node.Literal(title)),
      Triple(subject, Node.Uri(CategoryOntology.description), Node.Literal(description))
    ) ++ orderTriples ++ subTriples ++ articleTriples ++ topicTriples ++ tagTriples
  }

  // ------------------------------------------------------------------
  // Subcategory Triples
  // ------------------------------------------------------------------
  def subcategoryTriples(
    subId: String,
    title: String,
    parentId: String
  ): Seq[Triple] = {
    val subject = categoryNode(subId)
    Seq(
      Triple(subject, RdfType, Node.Uri(Subcategory)),
      Triple(subject, Node.Uri(CategoryOntology.title), Node.Literal(title)),
      Triple(subject, Node.Uri(parentCategory), categoryNode(parentId))
    )
  }

  // ------------------------------------------------------------------
  // Article Triples
  // ------------------------------------------------------------------
  def articleTriples(
    articleId: String,
    title: String,
    related: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = articleNode(articleId)
    val relatedTriples = related.map(rid =>
      Triple(subject, Node.Uri(relatedTo), articleNode(rid))
    )
    Seq(
      Triple(subject, RdfType, Node.Uri(Article)),
      Triple(subject, Node.Uri(CategoryOntology.title), Node.Literal(title))
    ) ++ relatedTriples
  }

  // ------------------------------------------------------------------
  // Graph Builder
  // ------------------------------------------------------------------
  def toGraph(
    categoryId: String,
    title: String,
    description: String,
    articles: Seq[(String, String)],
    subcategories: Seq[(String, String)] = Seq.empty
  ): Graph = {
    val catTriples =
      categoryTriples(categoryId, title, description, articles = articles.map(_._1), subcategories = subcategories.map(_._1))
    val subTriples =
      subcategories.flatMap { case (sid, stitle) => subcategoryTriples(sid, stitle, categoryId) }
    val artTriples =
      articles.flatMap { case (aid, atitle) => articleTriples(aid, atitle) }
    Graph(catTriples ++ subTriples ++ artTriples)
  }
}
