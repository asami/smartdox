package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * @since   Nov. 12, 2025
 * @version Nov. 12, 2025
 * @author  ASAMI, Tomoharu
 */
object BokSiteOntology {
  val prefix = "boksite"
  val namespace = "https://www.simplemodeling.org/bok-site/ontology/1.0#"
  def uri(local: String) = namespace + local

  // Core Classes
  val BoKSite = uri("BoKSite")
  val Article = uri("Article")
  val Category = uri("Category")
  val Term = uri("Term")

  // Properties
  val hasArticle = uri("hasArticle")
  val hasCategory = uri("hasCategory")
  val hasTerm = uri("hasTerm")
  val relatedTo = uri("relatedTo")
  val taggedWith = uri("taggedWith")

  lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "BoKSite" -> BoKSite,
    "Article" -> Article,
    "Category" -> Category,
    "Term" -> Term,
    "hasArticle" -> hasArticle,
    "hasCategory" -> hasCategory,
    "hasTerm" -> hasTerm,
    "relatedTo" -> relatedTo,
    "taggedWith" -> taggedWith
  )
}
