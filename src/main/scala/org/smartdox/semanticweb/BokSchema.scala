package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * Bok Schema (Body of Knowledge Schema)
 *
 * Defines RDF-level representation for integrated knowledge
 * across SimpleModeling and SmartDox ecosystems.
 *
 * It combines conceptual, documentary, and operational knowledge
 * from projects, entities, rules, and site-level documents
 * into a unified semantic graph.
 *
 * @since   Nov. 12, 2025
 * @version Nov. 28, 2025
 * @author  ASAMI, Tomoharu
 */
object BokSchema extends SchemaModel {

  override val prefix: String = Vocabulary.Bok.prefix
  override val namespace: String = Vocabulary.Bok.namespace

  override lazy val jsonldContext: Map[String, Any] = BokOntology.jsonldContext

  val BoKRoot = Node.Uri("https://www.simplemodeling.org/bok")
  val DocumentModelRoot = Node.Uri("https://www.simplemodeling.org/bok/document-model")
  val SimpleModelRoot = Node.Uri("https://www.simplemodeling.org/bok/simple-model")
  val ComponentRepositoryRoot = Node.Uri("https://www.simplemodeling.org/bok/component-repository")

  def build(site: Seq[Site.SiteResource], simpleModelGraph: Graph, componentRepositoryGraph: Graph): Graph = {

    val docModel = DocumentModelSchema.fromSiteResources(site)
    val docGraph = docModel.toGraph

    // Extract documentedBy links from SimpleModel graph
    val documentedByLinks: Seq[Triple] =
      simpleModelGraph.triples.filter(t => t.predicate == Node.Uri(SimpleModelOntology.documentedBy))

    val docModelRootTriples = Seq(
      Triple(DocumentModelRoot, RdfType, Node.Uri(DocumentModelOntology.uri("DocumentModelRoot"))),
      Triple(DocumentModelRoot, Rdfs.node.label, Node.Literal("Document Model"))
    )

    val simpleModelRootTriples = Seq(
      Triple(SimpleModelRoot, RdfType, Node.Uri(SimpleModelOntology.uri("SimpleModel"))),
      Triple(SimpleModelRoot, Rdfs.node.label, Node.Literal("Simple Model"))
    )

    val componentRepositoryRootTriples = Seq(
      Triple(ComponentRepositoryRoot, RdfType, Node.Uri(BokOntology.ComponentRepository)),
      Triple(ComponentRepositoryRoot, Rdfs.node.label, Node.Literal("Component Repository"))
    )

    val bokRootTriples = Seq(
      Triple(BoKRoot, RdfType, Node.Uri(BokOntology.BoK)),
      Triple(BoKRoot, Rdfs.node.label, Node.Literal("SimpleModeling Body of Knowledge")),
      Triple(BoKRoot, Node.Uri(BokOntology.includesDocumentModel), DocumentModelRoot),
      Triple(BoKRoot, Node.Uri(BokOntology.includesSimpleModel), SimpleModelRoot)
      , Triple(BoKRoot, Node.Uri(BokOntology.includesComponentRepository), ComponentRepositoryRoot)
    )

    Graph(
      bokRootTriples ++
      docModelRootTriples ++
      simpleModelRootTriples ++
      componentRepositoryRootTriples ++
      docGraph.triples ++
      simpleModelGraph.triples ++
      componentRepositoryGraph.triples ++
      documentedByLinks
    )
  }
}
