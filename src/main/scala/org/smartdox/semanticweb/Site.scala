package org.smartdox.semanticweb

import java.util.Locale

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.metadata.DocumentMetaData

/**
 * Site (SimpleModeling.org)
 *
 * This class generates a full RDF Site Graph that represents
 * the running structure of SimpleModeling.org:
 *
 *  - Site root (@id = smorg:SiteRoot)
 *  - Relations to Ontology / Schema / Vocabulary (URIs are passed in)
 *  - Relations to SmartDox pages (SiteResource)
 *  - Metadata of each document (title, created, updated, author)
 *
 * It produces:
 *   - site.jsonld
 *   - site.ttl
 *
 * @since   Nov. 20, 2025
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
object Site {

  // ------------------------------------------------------------
  // Namespace
  // ------------------------------------------------------------

  /** Prefix for SimpleModeling.org meta-ontology (smorg:) */
  val prefix: String = "smorg"

  /** Namespace of SimpleModeling.org meta-ontology */
  val namespace: String = SimpleModelingOrgOntology.namespace

  /** Builds a full IRI under the smorg namespace */
  def uri(local: String): String = namespace + local

  /** Root identifier of this site */
  val SiteRoot: String = uri("SiteRoot")

  // ------------------------------------------------------------
  // Data Model
  // ------------------------------------------------------------

  /**
   * Represents one page or document inside the site.
   * Derived from SmartDox DocumentMetaData (HEAD section).
   */
  case class SiteResource(
    id: String,                  // absolute IRI of the page
    meta: DocumentMetaData       // SmartDox document metadata
  ) {

    /**
     * Convert this SiteResource into RDF triples.
     *
     * - rdf:type dcterms:BibliographicResource
     * - rdfs:label with language-tagged literals (ja / en) if available
     * - dcterms:date / dcterms:modified from published/modified
     * - dcterms:creator with language-tagged literals (ja / en) if available
     */
    def toTriples: Seq[Triple] = {
      val s = Node.Uri(id)

      // Type: treat each page as a bibliographic resource.
      val typeTriple =
        Triple(s, RdfType, Dcterms.node.BibliographicResource)

      // Title as language-tagged labels (language-map style).
      val titleJa = meta.getTitleString(Locale.JAPANESE)
      val titleEn = meta.getTitleString(Locale.ENGLISH)

      val titleTriples: Seq[Triple] =
        Seq(
          titleJa.map(t =>
            Triple(s, Rdfs.node.label, Node.Literal(t, None, Some("ja")))
          ),
          titleEn.map(t =>
            Triple(s, Rdfs.node.label, Node.Literal(t, None, Some("en")))
          )
        ).flatten

      // Dates: published / modified -> dcterms:date / dcterms:modified
      val published = meta.getPublishedString(Locale.JAPANESE).map { d =>
        Triple(s, Dcterms.node.date, Node.Literal(d))
      }

      val modified = meta.getModifiedString(Locale.JAPANESE).map { d =>
        Triple(s, Dcterms.node.modified, Node.Literal(d))
      }

      // Author as language-tagged creator
      val authorJa = meta.getAuthorString(Locale.JAPANESE)
      val authorEn = meta.getAuthorString(Locale.ENGLISH)

      val authorTriples: Seq[Triple] =
        Seq(
          authorJa.map(a =>
            Triple(s, Dcterms.node.creator, Node.Literal(a, None, Some("ja")))
          ),
          authorEn.map(a =>
            Triple(s, Dcterms.node.creator, Node.Literal(a, None, Some("en")))
          )
        ).flatten

      Seq(typeTriple) ++ titleTriples ++ published ++ modified ++ authorTriples
    }
  }

  /**
   * Site model: root + resources + links to Ontology/Schema/Vocabulary.
   *
   * @param resources  list of site pages (SmartDox documents)
   * @param ontology   URI of governing ontology (e.g. smorg: or SimpleModeling ontology)
   * @param schema     URI of schema graph (if any)
   * @param vocabulary URI of vocabulary graph (if any)
   */
  case class SiteModel(
    resources: Seq[SiteResource] = Seq.empty,
    ontology: Option[String] = None,
    schema: Option[String] = None,
    vocabulary: Option[String] = None
  ) {

    /**
     * Build full RDF graph of the site:
     *
     * - smorg:SiteRoot a smorg:Site ;
     *     rdfs:label "SimpleModeling.org" ;
     *     smorg:includesOntology <...> ;
     *     smorg:includesSchema   <...> ;
     *     smorg:definesVocabulary <...> .
     * - plus all SiteResource triples.
     */
    def toGraph: Graph = {
      val root = Node.Uri(SiteRoot)

      val rootBase: Seq[Triple] = Seq(
        Triple(root, RdfType, Node.Uri(uri("Site"))),
        Triple(root, Rdfs.node.label, Node.Literal("SimpleModeling.org"))
      )

      val ontoTriples: Seq[Triple] =
        ontology.map(o =>
          Triple(root, Node.Uri(SimpleModelingOrgOntology.includesOntology), Node.Uri(o))
        ).toSeq

      val schemaTriples: Seq[Triple] =
        schema.map(s =>
          Triple(root, Node.Uri(SimpleModelingOrgOntology.includesSchema), Node.Uri(s))
        ).toSeq

      // There is no includesVocabulary in SimpleModelingOrgOntology yet,
      // so we use definesVocabulary to relate the site to its vocabulary.
      val vocabTriples: Seq[Triple] =
        vocabulary.map(v =>
          Triple(root, Node.Uri(SimpleModelingOrgOntology.definesVocabulary), Node.Uri(v))
        ).toSeq

      val resourceTriples: Seq[Triple] =
        resources.flatMap(_.toTriples)

      Graph(rootBase ++ ontoTriples ++ schemaTriples ++ vocabTriples ++ resourceTriples)
    }
  }

  // ------------------------------------------------------------
  // JSON-LD / Turtle Rendering
  // ------------------------------------------------------------

  /**
   * Default JSON-LD @context for site.jsonld
   *
   * Note:
   *  - RDF / RDFS / OWL / DCTERMS namespaces
   *  - smorg: SimpleModeling.org meta-ontology namespace
   */
  lazy val jsonldContext: Map[String, Any] = Map(
    "rdf"      -> Vocabulary.Rdf.namespace,
    "rdfs"     -> Vocabulary.Rdfs.namespace,
    "owl"      -> Vocabulary.Owl.namespace,
    "dcterms"  -> Vocabulary.Dcterms.namespace,
    "smorg"    -> namespace
  )

  /** Default rendering policy for JSON-LD. */
  private def defaultPolicy: RdfRenderer.Policy =
    RdfRenderer.Policy(
      prettyJson    = true,
      prettyContext = true,
      jsonIndent    = 2
    )

  /**
   * Generate JSON-LD string (site.jsonld).
   * Uses BoK profile so that ontology / schema / site nodes are
   * ordered in a stable, model-centric way.
   */
  def toJsonLD(model: SiteModel): String =
    RdfRenderer.toJsonLD(
      model.toGraph,
      RdfRenderer.JsonLDProfile.BoK,   // BoK profile fits site structural data
      userContext = jsonldContext,
      policy = defaultPolicy
    )

  /**
   * Generate Turtle string (site.ttl).
   * Uses the same BoK profile for subject ordering and prefix handling.
   */
  def toTurtle(model: SiteModel): String =
    RdfRenderer.toTurtle(
      model.toGraph,
      RdfRenderer.JsonLDProfile.BoK
    )
}
