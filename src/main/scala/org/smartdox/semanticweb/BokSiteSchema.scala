package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * @since   Nov. 12, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object BoxSiteSchema {
  /** Represents a Box Article or Page */
  case class Article(
    id: String,
    title: String,
    description: Option[String] = None,
    language: Option[String] = None,
    tags: Seq[String] = Seq.empty,
    category: Option[String] = None,
    related: Seq[String] = Seq.empty
  ) {
    def toTriples: Seq[Triple] = {
      val subject = Node.Uri(id)
      val base = Seq(
        Triple(subject, RdfType, Rdfs.node.Resource),
        Triple(subject, Dcterms.node.title, Node.Literal(title)),
      )
      val opt = Seq(
        description.map(v => Triple(subject, Dcterms.node.description, Node.Literal(v))),
        language.map(v => Triple(subject, Dcterms.node.language, Node.Literal(v))),
        category.map(v => Triple(subject, Dcterms.node.subject, Node.Literal(v)))
      ).flatten
      val tagTriples = tags.map(tag => Triple(subject, SimpleModelOntology.node.tag, Node.Literal(tag)))
      val relTriples = related.map(rel => Triple(subject, Dcterms.node.relation, Node.Uri(rel)))
      base ++ opt ++ tagTriples ++ relTriples
    }
  }

  /** Represents a site-wide Category or Section */
  case class Category(
    id: String,
    name: String,
    parent: Option[String] = None
  ) {
    def toTriples: Seq[Triple] = {
      val subject = Node.Uri(id)
      val base = Seq(
        Triple(subject, RdfType, Rdfs.node.Class),
        Triple(subject, Rdfs.node.label, Node.Literal(name))
      )
      val parentTriple = parent.map(p => Triple(subject, Rdfs.node.subClassOf, Node.Uri(p)))
      base ++ parentTriple
    }
  }

  /** Represents a glossary term (linked to /glossary) */
  case class Term(
    id: String,
    label: String,
    definition: Option[String] = None,
    alias: Seq[String] = Seq.empty,
    seeAlso: Seq[String] = Seq.empty
  ) {
    def toTriples: Seq[Triple] = {
      val subject = Node.Uri(id)
      val base = Seq(
        Triple(subject, RdfType, Rdfs.node.Class),
        Triple(subject, Rdfs.node.label, Node.Literal(label))
      )
      val opt = definition.map(v => Triple(subject, Rdfs.node.comment, Node.Literal(v))).toSeq
      val aliasTriples = alias.map(a => Triple(subject, Dcterms.node.alternative, Node.Literal(a)))
      val seeAlsoTriples = seeAlso.map(s => Triple(subject, Rdfs.node.seeAlso, Node.Uri(s)))
      base ++ opt ++ aliasTriples ++ seeAlsoTriples
    }
  }

  /** Represents the whole Box site as an RDF Graph */
  case class BoxSite(
    articles: Seq[Article] = Seq.empty,
    categories: Seq[Category] = Seq.empty,
    glossary: Seq[Term] = Seq.empty
  ) {
    def toGraph: Graph = {
      val triples = articles.flatMap(_.toTriples) ++
        categories.flatMap(_.toTriples) ++
        glossary.flatMap(_.toTriples)
      Graph(triples)
    }
  }
}
