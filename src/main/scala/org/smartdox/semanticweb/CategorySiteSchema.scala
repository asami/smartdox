package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.semanticweb.CategorySiteOntology._

/*
 * Category Site Schema
 * ----------------------------------------------------------------------
 * Defines the structural schema for category-site JSON-LD generation
 * within the BoK publication layer of SimpleModeling.org.
 *
 * @since   Nov. 13, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object CategorySiteSchema {
  // ------------------------------------------------------------------
  // RDF Node Builders
  // ------------------------------------------------------------------
  def siteNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def categoryNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def itemNode(uriStr: String): Node.Uri = Node.Uri(uriStr)

  // ------------------------------------------------------------------
  // CategorySite Triples
  // ------------------------------------------------------------------
  def siteTriples(siteId: String, categories: Seq[String]): Seq[Triple] = {
    val subject = siteNode(siteId)
    val categoryTriples = categories.map(cid =>
      Triple(subject, Node.Uri(hasCategory), categoryNode(cid))
    )
    Seq(
      Triple(subject, RdfType, Node.Uri(CategorySite)),
      Triple(subject, Rdfs.node.label, Node.Literal("Category Site"))
    ) ++ categoryTriples
  }

  // ------------------------------------------------------------------
  // CategoryPage Triples
  // ------------------------------------------------------------------
  def categoryTriples(
    categoryId: String,
    title: String,
    description: String,
    items: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = categoryNode(categoryId)
    val itemTriples = items.map(iid =>
      Triple(subject, Node.Uri(hasItem), itemNode(iid))
    )
    Seq(
      Triple(subject, RdfType, Node.Uri(CategoryPage)),
      Triple(subject, Node.Uri(title), Node.Literal(title)),
      Triple(subject, Node.Uri(description), Node.Literal(description))
    ) ++ itemTriples
  }

  // ------------------------------------------------------------------
  // CategoryItem Triples
  // ------------------------------------------------------------------
  def itemTriples(
    itemId: String,
    title: String,
    order: Option[Int] = None,
    related: Seq[String] = Seq.empty,
    tags: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = itemNode(itemId)
    val orderTriples = order.map(o => Triple(subject, Node.Uri(CategorySiteOntology.order), Node.Literal(o.toString))).toSeq
    val relatedTriples = related.map(r =>
      Triple(subject, Node.Uri(relatedTo), itemNode(r))
    )
    val tagTriples = tags.map(t =>
      Triple(subject, Node.Uri(taggedWith), Node.Uri(t))
    )
    Seq(
      Triple(subject, RdfType, Node.Uri(CategoryItem)),
      Triple(subject, Node.Uri(CategorySiteOntology.title), Node.Literal(title))
    ) ++ orderTriples ++ relatedTriples ++ tagTriples
  }

  // ------------------------------------------------------------------
  // Graph builder
  // ------------------------------------------------------------------
  def toGraph(
    siteId: String,
    categories: Seq[(String, String, String, Seq[(String, String)])]
  ): Graph = {
    val site = siteTriples(siteId, categories.map(_._1))
    val pages = categories.flatMap { case (id, title, desc, items) =>
      categoryTriples(id, title, desc, items.map(_._1))
    }
    val itemTriplesAll = categories.flatMap { case (_, _, _, items) =>
      items.flatMap { case (id, title) => itemTriples(id, title) }
    }
    Graph(site ++ pages ++ itemTriplesAll)
  }
}
