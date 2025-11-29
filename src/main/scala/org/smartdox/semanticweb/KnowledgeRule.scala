package org.smartdox.semanticweb

import java.net.URI
import org.goldenport.RAISE
import org.goldenport.values.PathName
import org.goldenport.util.StringUtils
import org.smartdox.doxsite.LinkEnabler.LinkEmbedder.Link
import org.smartdox.semanticweb.Rdf._

/*
 * @since   Nov. 29, 2025
 * @version Nov. 29, 2025
 * @author  ASAMI, Tomoharu
 */
case class KnowledgeRule(
) {
  type IriId = String

  def resourceIri(source: Node, link: Link): IriId =
    link.pathname match {
      case Some(s) => resourceIri(source, s, link.href)
      case None => resourceIriRelative(source, link.href.toString)
    }

  def resourceIri(source: Node, base: PathName, path: URI): IriId =
    resourceIri(source.value, base.v, path.toString)

  /**
   * Resolves a resource IRI using:
   *  - sourceRoot: e.g. https://www.simplemodeling.org/domain-modeling/analysis-design-entity
   *  - basePath:  a .dox file path such as "development-process/meta-development-system-framework.dox"
   *  - rel:       a relative glossary link such as "../glossary/literate-modeling/cml.html"
   *
   * Produces: https://www.simplemodeling.org/glossary/literate-modeling/cml
   */
  def resourceIri(sourceRoot: String, basePath: String, rel: String): String = {
    val rootUri = new java.net.URI(sourceRoot)

    // Construct a base document URI without extension:
    // e.g. sourceRoot "https://.../domain-modeling/entity"
    //  + basePath "development-process/meta-development-system-framework.dox"
    //  -> "https://.../development-process/meta-development-system-framework"
    val baseDoc =
      if (basePath.endsWith(".dox"))
        basePath.substring(0, basePath.length - 4)
      else
        basePath

    val siteRoot =
      rootUri.resolve("/").toString // ensure ending slash

    val baseDocUri = new java.net.URI(siteRoot + baseDoc)

    // Resolve relative glossary path
    val resolved = baseDocUri.resolve(rel).toString

    // Strip trailing ".html"
    StringUtils.toPathnameBody(resolved)
  }




  def resourceIriRelative(source: Node, path: String): IriId = {
    // Resolve a relative link such as "../glossary/literate-modeling/cml.html"
    // against an article IRI like "https://www.simplemodeling.org/domain-modeling/analysis-design-entity"
    // producing "https://www.simplemodeling.org/glossary/literate-modeling/cml"
    val base = new java.net.URI(source.value)
    val resolved = base.resolve(path) // assuming Link.path holds the relative path string
    val s = resolved.toString
    StringUtils.toPathnameBody(s)
  }

  def toArticleNode(source: Node, article: Link): Node =
    Node.Uri(resourceIri(source, article))

  def toGlossaryNode(source: Node, glossary: Link): Node =
    Node.Uri(resourceIri(source, glossary))

  def articleToArticle(source: Node, sink: IriId): Vector[Triple] =
    articleToArticle(source, Node.Uri(sink))

  def articleToArticle(source: Node, sink: Link): Vector[Triple] =
    articleToArticle(source, toArticleNode(source, sink))

  def articleToArticle(source: Node, sink: Node): Vector[Triple] = {
    // Weak HTML-level reference
    val m = mentions(source, sink)

    // External vocabularies for general reference between articles
    val s_mentions = schemaMentions(source, sink)
    val d_refs = dctermsReferences(source, sink)
    val related = schemaRelated(source, sink)

    m ++ s_mentions ++ d_refs ++ related
  }

  def articleToGlossaryUse(article: Node, glossary: IriId): Vector[Triple] =
    articleToGlossaryUse(article, Node.Uri(glossary))

  def articleToGlossaryUse(article: Node, glossary: Link): Vector[Triple] =
    articleToGlossaryUse(article, toGlossaryNode(article, glossary))

  def articleToGlossaryUse(article: Node, glossary: Node): Vector[Triple] = {
    // Weak HTML link
    val m = mentions(article, glossary)

    // Medium-strength semantic relations for "use" (not definition)
    val refs = referencesTerm(article, glossary)

    // External vocabularies (medium strength)
    val s_mentions = schemaMentions(article, glossary)
    val d_refs = dctermsReferences(article, glossary)
    val skos_ex = skosExample(article, glossary)

    m ++ refs ++ s_mentions ++ d_refs ++ skos_ex
  }

  def articleToGlossaryDefine(article: Node, glossary: Node): Vector[Triple] = {
    // Core weak relation (HTML link)
    val m = mentions(article, glossary)

    // Stronger semantic relations (DocumentModelOntology)
    val about = aboutTerm(article, glossary)
    val defines = definesTerm(article, glossary)
    val refs = referencesTerm(article, glossary)

    // Combine all; callers may filter as needed
    m ++ about ++ defines ++ refs ++
    schemaAbout(article, glossary) ++
    schemaMentions(article, glossary) ++
    dctermsSubject(article, glossary) ++
    dctermsReferences(article, glossary) ++
    skosExample(article, glossary)
  }

  def mentions(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(
        source,
        DocumentModelOntology.node.mentions,
        sink
      )// ,
      // Triple(
      //   sink,
      //   Node.Uri(DocumentModelOntology.mentionedIn),
      //   source
      // )
    )

  def aboutTerm(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(
        source,
        DocumentModelOntology.node.aboutTerm,
        sink
      )// ,
      // Triple(
      //   sink,
      //   DocumentModelOntology.node.aboutTermOf,
      //   source
      // )
    )

  def definesTerm(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(
        source,
        DocumentModelOntology.node.definesTerm,
        sink
      )// ,
      // Triple(
      //   sink,
      //   DocumentModelOntology.node.definedIn,
      //   source
      // )
    )

  def referencesTerm(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(
        source,
        DocumentModelOntology.node.referencesTerm,
        sink
      )
    )

  def schemaAbout(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(source, Vocabulary.Schema.node.about, sink)
    )

  def schemaMentions(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(source, Vocabulary.Schema.node.mentions, sink)
    )

  def dctermsSubject(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(source, Vocabulary.Dcterms.node.subject, sink)
    )

  def dctermsReferences(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(source, Vocabulary.Dcterms.node.references, sink)
    )

  def skosExample(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(source, Vocabulary.Skos.node.example, sink)
    )

  def schemaRelated(source: Node, sink: Node): Vector[Triple] =
    Vector(
      Triple(source, Vocabulary.Schema.node.relatedLink, sink)
    )
}
