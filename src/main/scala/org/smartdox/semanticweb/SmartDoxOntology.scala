package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * SmartDox Ontology (Unified)
 * ----------------------------------------------------------------------
 * Defines both the vocabulary (ontology) and the RDF/JSON-LD schema
 * for SmartDox document structures.
 *
 * This unified model covers:
 *  - RDF/OWL class and property definitions
 *  - Schema-level builders to generate RDF triples
 *    representing SmartDox documents and their internal structures.
 *
 * @since   Nov. 13, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object SmartDoxOntology extends OntologyModel {
  override val prefix: String = Vocabulary.SmartDox.prefix
  override val namespace: String = Vocabulary.SmartDox.namespace

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val Document    = uri("Document")
  val Meta        = uri("Meta")
  val Section     = uri("Section")
  val Paragraph   = uri("Paragraph")
  val List        = uri("List")
  val Item        = uri("Item")
  val Table       = uri("Table")
  val Figure      = uri("Figure")
  val CodeBlock   = uri("CodeBlock")
  val Note        = uri("Note")
  val Link        = uri("Link")

  // ------------------------------------------------------------------
  // Datatype and Annotation Properties
  // ------------------------------------------------------------------
  val title        = uri("title")
  val description  = uri("description")
  val created      = uri("created")
  val modified     = uri("modified")
  val language     = uri("language")
  val keyword      = uri("keyword")
  val tag          = uri("tag")
  val text         = uri("text")
  val src          = uri("src")
  val caption      = uri("caption")
  val codeText     = uri("codeText")
  val codeLanguage = uri("codeLanguage")

  // ------------------------------------------------------------------
  // Object Properties
  // ------------------------------------------------------------------
  val hasMeta       = uri("hasMeta")
  val hasSection    = uri("hasSection")
  val hasParagraph  = uri("hasParagraph")
  val hasList       = uri("hasList")
  val hasItem       = uri("hasItem")
  val hasTable      = uri("hasTable")
  val hasFigure     = uri("hasFigure")
  val hasCodeBlock  = uri("hasCodeBlock")
  val hasLink       = uri("hasLink")

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  override lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "Document" -> Document,
    "Section" -> Section,
    "Paragraph" -> Paragraph,
    "Figure" -> Figure,
    "Table" -> Table,
    "CodeBlock" -> CodeBlock,
    "Note" -> Note,
    "title" -> title,
    "description" -> description,
    "language" -> language,
    "keyword" -> keyword,
    "tag" -> tag
  )

  // ------------------------------------------------------------------
  // RDF Triple Builders (Schema functions)
  // ------------------------------------------------------------------

  def documentTriples(
    docUri: String,
    titleStr: String,
    desc: Option[String] = None,
    lang: Option[String] = None,
    createdAt: Option[String] = None,
    modifiedAt: Option[String] = None,
    keywords: Seq[String] = Seq.empty,
    tags: Seq[String] = Seq.empty,
    sections: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = Node.Uri(docUri)
    val baseTriples = Seq(
      Triple(subject, RdfType, Node.Uri(Document)),
      Triple(subject, Node.Uri(title), Node.Literal(titleStr))
    )

    val optTriples = Seq(
      desc.map(d => Triple(subject, Node.Uri(description), Node.Literal(d))),
      lang.map(l => Triple(subject, Node.Uri(language), Node.Literal(l))),
      createdAt.map(c => Triple(subject, Node.Uri(created), Node.Literal(c))),
      modifiedAt.map(m => Triple(subject, Node.Uri(modified), Node.Literal(m)))
    ).flatten

    val keywordTriples = keywords.map(k => Triple(subject, Node.Uri(keyword), Node.Literal(k)))
    val tagTriples     = tags.map(t => Triple(subject, Node.Uri(tag), Node.Literal(t)))
    val sectionTriples = sections.map(s => Triple(subject, Node.Uri(hasSection), Node.Uri(s)))

    baseTriples ++ optTriples ++ keywordTriples ++ tagTriples ++ sectionTriples
  }

  def sectionTriples(
    uriStr: String,
    titleStr: String,
    paragraphs: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = Node.Uri(uriStr)
    val base = Seq(
      Triple(subject, RdfType, Node.Uri(Section)),
      Triple(subject, Node.Uri(title), Node.Literal(titleStr))
    )
    val paraTriples = paragraphs.map(p => Triple(subject, Node.Uri(hasParagraph), Node.Uri(p)))
    base ++ paraTriples
  }

  def paragraphTriples(uriStr: String, textStr: String): Seq[Triple] = {
    val subject = Node.Uri(uriStr)
    Seq(
      Triple(subject, RdfType, Node.Uri(Paragraph)),
      Triple(subject, Node.Uri(text), Node.Literal(textStr))
    )
  }

  def figureTriples(uriStr: String, srcStr: String, captionStr: Option[String]): Seq[Triple] = {
    val subject = Node.Uri(uriStr)
    val base = Seq(
      Triple(subject, RdfType, Node.Uri(Figure)),
      Triple(subject, Node.Uri(src), Node.Literal(srcStr))
    )
    captionStr.map(c => base :+ Triple(subject, Node.Uri(caption), Node.Literal(c))).getOrElse(base)
  }

  def codeBlockTriples(uriStr: String, code: String, lang: Option[String]): Seq[Triple] = {
    val subject = Node.Uri(uriStr)
    val base = Seq(
      Triple(subject, RdfType, Node.Uri(CodeBlock)),
      Triple(subject, Node.Uri(codeText), Node.Literal(code))
    )
    val langT = lang.map(l => Triple(subject, Node.Uri(codeLanguage), Node.Literal(l)))
    base ++ langT.toSeq
  }

  override lazy val toTriples: Seq[Triple] = Seq.empty
}
