package org.smartdox.semanticweb

import java.util.Locale
import java.net.URI
import org.goldenport.collection.VectorMap
import org.goldenport.values.PathName
import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.metadata.MetaData
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.metadata.Glossary
import org.smartdox.doxsite.LinkCollection.DoxLinks

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
 * @version Nov. 29, 2025
 * @author  ASAMI, Tomoharu
 */
object Site {
  val rule = new KnowledgeRule()

  // ------------------------------------------------------------
  // Namespace
  // ------------------------------------------------------------

  /** Prefix for SimpleModeling.org meta-ontology (smorg:) */
  val prefix: String = "smorg"

  /** Namespace of SimpleModeling.org meta-ontology */
  val namespace: String = SimpleModelingOrgOntology.namespace

  /** Builds a full IRI under the smorg namespace */
  def uri(local: String): String = namespace + local

  /** Builds a full IRI under the Schema.org namespace */
  def schemaUri(local: String): String = Vocabulary.Schema.uri(local)

  /** Root identifier of this site */
  val SiteRoot: String = "https://www.simplemodeling.org/site/simplemodelingorg"

  // ------------------------------------------------------------
  // Data Model
  // ------------------------------------------------------------

  /**
   * Represents one page or document inside the site.
   * Derived from SmartDox DocumentMetaData (HEAD section).
   */
  sealed trait SiteResource {
    def id: String             // canonical article IRI
    def meta: DocumentMetaData // SmartDox document metadata

    def typeTriple: Triple

    /**
     * Convert this SiteResource into RDF triples.
     *
     * - rdf:type smorg:Article
     * - rdfs:label with language-tagged literals (ja / en) if available
     * - dcterms:date / dcterms:modified from published/modified
     * - dcterms:creator with language-tagged literals (ja / en) if available
     */
    def toTriples: Seq[Triple] = {
      val s = Node.Uri(id)

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

      val schemaAuthorTriples: Seq[Triple] =
        Seq(
          authorJa.map(a =>
            Triple(s, Node.Uri(schemaUri("author")), Node.Literal(a, None, Some("ja")))
          ),
          authorEn.map(a =>
            Triple(s, Node.Uri(schemaUri("author")), Node.Literal(a, None, Some("en")))
          ),
          authorJa.map(a =>
            Triple(s, Node.Uri(schemaUri("creator")), Node.Literal(a, None, Some("ja")))
          ),
          authorEn.map(a =>
            Triple(s, Node.Uri(schemaUri("creator")), Node.Literal(a, None, Some("en")))
          )
        ).flatten

      val descJa = meta.getEffectiveSummaryString(Locale.JAPANESE)
      val descEn = meta.getEffectiveSummaryString(Locale.ENGLISH)

      val descriptionTriples: Seq[Triple] =
        Seq(
          descJa.map(d =>
            Triple(s, Node.Uri(schemaUri("description")), Node.Literal(d, None, Some("ja")))
          ),
          descEn.map(d =>
            Triple(s, Node.Uri(schemaUri("description")), Node.Literal(d, None, Some("en")))
          ),
          descJa.map(d =>
            Triple(s, Node.Uri(uri("summary")), Node.Literal(d, None, Some("ja")))
          ),
          descEn.map(d =>
            Triple(s, Node.Uri(uri("summary")), Node.Literal(d, None, Some("en")))
          )
        ).flatten

      val headlineJa = meta.getEffectiveHeadlineString(Locale.JAPANESE)
      val headlineEn = meta.getEffectiveHeadlineString(Locale.ENGLISH)
      val briefJa = meta.getEffectiveBriefString(Locale.JAPANESE)
      val briefEn = meta.getEffectiveBriefString(Locale.ENGLISH)

      val headlineTriples: Seq[Triple] =
        Seq(
          headlineJa.map(h =>
            Triple(s, Node.Uri(uri("headline")), Node.Literal(h, None, Some("ja")))
          ),
          headlineEn.map(h =>
            Triple(s, Node.Uri(uri("headline")), Node.Literal(h, None, Some("en")))
          )
        ).flatten

      val briefTriples: Seq[Triple] =
        Seq(
          briefJa.map(b =>
            Triple(s, Node.Uri(uri("brief")), Node.Literal(b, None, Some("ja")))
          ),
          briefEn.map(b =>
            Triple(s, Node.Uri(uri("brief")), Node.Literal(b, None, Some("en")))
          )
        ).flatten

      val glossarytriples = Vector() // meta.glossary.definitions

      val referencetriples = Vector() // meta.references

      val categorietriples = Vector() // meta.categories

      val keywordtriples = Vector() // meta.keywords

      val tagtriples = Vector() // meta.tags

      Seq(typeTriple) ++ titleTriples ++ published ++ modified ++ authorTriples ++
      schemaAuthorTriples ++ descriptionTriples ++ headlineTriples ++ briefTriples ++
      glossarytriples ++ referencetriples ++ categorietriples ++ keywordtriples ++ tagtriples
    }
  }
  object SiteResource {
    case class Article(
      id: String,            // canonical article IRI
      meta: DocumentMetaData // SmartDox document metadata
    ) extends SiteResource {
      // Type: article node
      val typeTriple =
        Triple(Node.Uri(id), RdfType, Node.Uri(uri("Article")))
    }
    object Article {
      def create(path: URI, meta: DocumentMetaData): Article = {
        val cid = _create_canonical_id(path)
        Article(cid, meta)
      }
    }

    case class Glossary(
      id: String,            // canonical article IRI
      meta: DocumentMetaData // SmartDox document metadata
    ) extends SiteResource {
      val typeTriple =
        Triple(Node.Uri(id), RdfType, Dcterms.node.BibliographicResource)
    }
    object Glossary {
      def create(path: URI, meta: DocumentMetaData): Glossary = {
        val cid = _create_canonical_id(path)
        Glossary(cid, meta)
      }
    }

    case class Bibliography(
      id: String,            // canonical article IRI
      meta: DocumentMetaData // SmartDox document metadata
    ) extends SiteResource {
      val typeTriple =
        Triple(Node.Uri(id), RdfType, Dcterms.node.BibliographicResource)
    }
    object Bibliography {
      def create(path: URI, meta: DocumentMetaData): Bibliography = {
        val cid = _create_canonical_id(path)
        Bibliography(cid, meta)
      }
    }

    private def _create_canonical_id(path: URI): String = {
      val raw = path.toString.stripPrefix("/")
      val base =
        if (raw.endsWith(".html")) raw.dropRight(5)
        else raw
      s"https://www.simplemodeling.org/$base"
    }
  }

  /**
   * Site model: root + resources + links to Ontology/Schema/Vocabulary.
   *
   * @param resources  list of site pages (SmartDox documents)
   * @param glossary   glossary data for the site
   * @param locales    list of supported locales
   * @param ontology   URI of governing ontology (e.g. smorg: or SimpleModeling ontology)
   * @param schema     URI of schema graph (if any)
   * @param vocabulary URI of vocabulary graph (if any)
   */
  case class SiteModel(
    metadata: MetaData = MetaData.empty,
    resources: Seq[SiteResource] = Seq.empty,
    locales: Seq[String] = Seq("ja", "en"),
    ontology: Option[String] = None,
    schema: Option[String] = None,
    vocabulary: Option[String] = None
  ) {
    def glossary: Glossary = metadata.glossary

    /**
     * Build full RDF graph of the site:
     *
     * - smorg:SiteRoot a smorg:Site ;
     *     rdfs:label "SimpleModeling.org" ;
     *     smorg:includesOntology <...> ;
     *     smorg:includesSchema   <...> ;
     *     smorg:definesVocabulary <...> .
     * - plus all SiteResource triples.
     * - plus locale-specific page nodes with schema:hasPart, schema:inLanguage, and rdf:type dcterms:BibliographicResource
     */
    def toGraph: Graph = {
      val root = Node.Uri(SiteRoot)

      val rootBase: Seq[Triple] = Seq(
        Triple(root, RdfType, Node.Uri(uri("Site"))),
        Triple(root, Rdfs.node.label, Node.Literal("SimpleModeling.org"))
      )

      val siteClassTriples: Seq[Triple] = Seq(
        Triple(Node.Uri(uri("Site")), RdfType, Owl.node.Class),
        Triple(Node.Uri(uri("Site")), Rdfs.node.label, Node.Literal("Site"))
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
        vocabulary.map(_ =>
          Triple(root, Node.Uri(SimpleModelingOrgOntology.definesVocabulary), Node.Uri(uri("Ontology")))
        ).toSeq

      // ------------------------------------------------------------
      // BoK root triple
      // ------------------------------------------------------------
      val bokRoot = Node.Uri("https://www.simplemodeling.org/bok")
      val includesBokTriple =
        Triple(root, Node.Uri(BokOntology.includesBoK), bokRoot)

      // Generate locale-specific page nodes for each canonical resource
      val localePageTriples: Seq[Triple] = resources.flatMap { r =>
        val canonicalNode = Node.Uri(r.id)
        val canonicalLocal =
          r.id.stripPrefix("https://www.simplemodeling.org/")
            .stripPrefix("/")
        locales.flatMap { loc =>
          val pageUri = s"https://www.simplemodeling.org/$loc/$canonicalLocal.html"
          val pageNode = Node.Uri(pageUri)
          Seq(
            Triple(canonicalNode, Node.Uri(schemaUri("hasPart")), pageNode),
            Triple(pageNode, Node.Uri(schemaUri("inLanguage")), Node.Literal(loc)),
            Triple(pageNode, RdfType, Dcterms.node.BibliographicResource)
          )
        }
      }

      // ------------------------------------------------------------
      // schema:relatedLink (cross-article weak relatedness)
      // ------------------------------------------------------------
      // val relatedTriples: Seq[Triple] = {
      //   val lcOpt = metadata.linkCollection

      //   lcOpt.toSeq.flatMap { lc =>
      //     // Loop over all canonical SiteResources
      //     resources.flatMap { sourceR =>
      //       val pathname = _smorg_iri_to_dox_path(sourceR.id)
      //       val sourceNode = Node.Uri(sourceR.id)

      //       // Get DoxLinks for this pathname
      //       lc.get(pathname).toSeq.flatMap { doxLinks =>
      //         doxLinks.internalLinks.links.flatMap { link =>
      //           val targetid = _to_canonical_smorg_iri(link.pathname, link.href)
      //           // Lookup target by pathnameValue
      //           val targetOpt = resources.find(_.id == targetid)

      //           targetOpt.map { targetR =>
      //             Triple(
      //               sourceNode,
      //               Node.Uri(schemaUri("relatedLink")),
      //               Node.Uri(targetR.id)
      //             )
      //           }
      //         }
      //       }
      //     }
      //   }
      // }

      // Mentions extraction: schema:mentions and schema:mentionedIn
      val mentionsTriples: Seq[Triple] = {
        val lcOpt = metadata.linkCollection

        lcOpt.toSeq.flatMap { lc =>
          resources.flatMap { sourceR =>
            val sourceNode = Node.Uri(sourceR.id)
            val pathname = _smorg_iri_to_dox_path(sourceR.id)
            lc.get(pathname).toSeq.flatMap(_to_triples(sourceNode))
          }
        }
      }

      val resourceTriples: Seq[Triple] =
        resources.flatMap(_.toTriples)

      // ------------------------------------------------------------
      // BoK integration
      // ------------------------------------------------------------
      // SimpleModel graph placeholder until SimpleModelSchema is implemented
      val simpleModelGraph = Graph(Nil)

      // Build full BoK graph (DocumentModel + SimpleModel)
      val bokGraph = BokSchema.build(resources, simpleModelGraph, Graph(Nil))
      val bokTriples = bokGraph.triples

      Graph(
        rootBase ++
        siteClassTriples ++
        ontoTriples ++
        schemaTriples ++
        vocabTriples ++
        Seq(includesBokTriple) ++
        localePageTriples ++
//        relatedTriples ++
        mentionsTriples ++
        resourceTriples ++
        bokTriples
      )
    }

    private def _to_triples(sourceNode: Node)(p: DoxLinks) = {
      val internaltriples = p.internalLinks.links.flatMap { link =>
        val targetId = rule.resourceIri(sourceNode, link)
        resources.find(_.id == targetId).toSeq.flatMap(_ => rule.articleToArticle(sourceNode, targetId))
      }

      val glossarytriples = p.glossaryLinks.links.flatMap { link =>
        val targetId = rule.resourceIri(sourceNode, link)
        resources.find(_.id == targetId).toSeq.flatMap(_ => rule.articleToGlossaryUse(sourceNode, targetId))
      }

      internaltriples ++ glossarytriples
    }

    def toJsonLD: String = Site.toJsonLD(this)
    def toTurtle: String = Site.toTurtle(this)

    private def _smorg_iri_to_dox_path(iri: String): String = {
      // Convert absolute URL to /x/y/z.dox
      val noScheme =
        iri.replaceFirst("^https?://[^/]+/", "")
      "/" + noScheme + ".dox"
    }

    // private def _to_canonical_smorg_iri(
    //   basepath: Option[PathName],
    //   targetpath: URI
    // ): String = basepath match {
    //   case Some(s) => _to_canonical_smorg_iri(s.toString, targetpath.toString)
    //   case None => _to_canonical_smorg_iri(_normalize_relative_dox_path(targetpath.toString), targetpath.toString)
    // }

    // private def _to_canonical_smorg_iri(
    //   basePath: String,         // absolute path of the current .dox file
    //   targetPath: String,       // link path such as "../foo/bar.dox"
    //   iriPrefix: String = "https://www.simplemodeling.org/simplemodelingorg/ontology/0.1-SNAPSHOT#"
    // ): String = {
    //   import java.nio.file.{Paths, Path}

    //   // Get the directory of the base .dox file
    //   val baseDir: Path = Paths.get(basePath).getParent

    //   // Resolve the relative target path against the base directory
    //   // Normalize to remove "../" and "./"
    //   val resolved: Path = baseDir.resolve(targetPath).normalize()

    //   // Convert to fragment — strip leading slash and remove ".dox" suffix
    //   val fragment =
    //     resolved.toString.stripPrefix("/").stripSuffix(".dox")

    //   // Build canonical IRI
    //   iriPrefix + fragment
    // }

    private def _normalize_relative_dox_path(path: String): String = {
      import java.nio.file.{Paths, Path}

      // Resolve the given path against root ("/") so that "../" parts disappear.
      // Note: Paths.get("/") is used as a fake root; resolve() then normalize() cleans it.
      val root: Path = Paths.get("/")
      val resolved: Path = root.resolve(path).normalize()

      // Convert to string. Ensure the result always starts with "/".
      val s = resolved.toString
      if (s.startsWith("/")) s else "/" + s
    }
  }
  object SiteModel {
    val empty = SiteModel()

    def create(
      metadata: MetaData,
      resources: Seq[SiteResource]
    ): SiteModel = {
      val locals = List("ja", "en")
      val o = SimpleModelingOrgPublicOntology.namespace.stripSuffix("#") + "/index.jsonld"
      val s = SimpleModelingOrgPublicSchema.namespace.stripSuffix("#") + "/index.jsonld"
      val v = Vocabulary.Rdf.namespace.stripSuffix("#")
      SiteModel(metadata, resources, locals, Some(o), Some(s), Some(v))
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
  lazy val jsonldContext: VectorMap[String, Any] = VectorMap(
    "rdf"      -> Vocabulary.Rdf.namespace,
    "rdfs"     -> Vocabulary.Rdfs.namespace,
    "owl"      -> Vocabulary.Owl.namespace,
    "dcterms"  -> Vocabulary.Dcterms.namespace,
    "schema"   -> Vocabulary.Schema.namespace,
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
