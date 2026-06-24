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
import org.smartdox.metadata.PublishMetadata
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
 *  version Nov. 29, 2025
 *  version May. 14, 2026
 * @version Jun. 24, 2026
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

  case class SiteMetadata(
    name: Option[String] = None,
    alternateName: Option[String] = None,
    url: Option[String] = None,
    description: Option[String] = None,
    author: Option[SiteMetadata.Agent] = None,
    publisher: Option[SiteMetadata.Agent] = None,
    inLanguage: Vector[String] = Vector.empty,
    license: Option[String] = None,
    keywords: Vector[String] = Vector.empty,
    datePublished: Option[String] = None,
    dateModified: Option[String] = None
  ) {
    def isEmpty: Boolean =
      name.isEmpty &&
      alternateName.isEmpty &&
      url.isEmpty &&
      description.isEmpty &&
      author.isEmpty &&
      publisher.isEmpty &&
      inLanguage.isEmpty &&
      license.isEmpty &&
      keywords.isEmpty &&
      datePublished.isEmpty &&
      dateModified.isEmpty

    def toTriples(root: Node.Uri): Seq[Triple] =
      if (isEmpty)
        Vector.empty
      else
        _literal(root, "name", name) ++
        _literal(root, "alternateName", alternateName) ++
        _literal(root, "url", url) ++
        _literal(root, "description", description) ++
        _literal(root, "license", license) ++
        _literal(root, "datePublished", datePublished) ++
        _literal(root, "dateModified", dateModified) ++
        inLanguage.map(x => Triple(root, Node.Uri(schemaUri("inLanguage")), Node.Literal(x))) ++
        keywords.map(x => Triple(root, Node.Uri(schemaUri("keywords")), Node.Literal(x))) ++
        _agent(root, "author", "author", "Person", author) ++
        _agent(root, "publisher", "publisher", "Organization", publisher)

    private def _literal(
      root: Node.Uri,
      property: String,
      value: Option[String]
    ): Seq[Triple] =
      value.toVector.map(x => Triple(root, Node.Uri(schemaUri(property)), Node.Literal(x)))

    private def _agent(
      root: Node.Uri,
      id: String,
      property: String,
      schemaType: String,
      agent: Option[SiteMetadata.Agent]
    ): Seq[Triple] =
      agent.toVector.flatMap { a =>
        val node = Node.Blank(id)
        Vector(
          Triple(root, Node.Uri(schemaUri(property)), node),
          Triple(node, RdfType, Node.Uri(schemaUri(schemaType)))
        ) ++ a.name.toVector.map(x => Triple(node, Node.Uri(schemaUri("name")), Node.Literal(x)))
      }
  }
  object SiteMetadata {
    case class Agent(
      name: Option[String] = None
    ) {
      def isEmpty: Boolean = name.isEmpty
    }

    val empty = SiteMetadata()

    import io.circe.{Decoder, Encoder, HCursor, Json}
    import io.circe.syntax._

    implicit val agentDecoder: Decoder[Agent] = Decoder.instance { c =>
      for {
        name <- c.downField("name").as[Option[String]]
      } yield Agent(name)
    }

    implicit val agentEncoder: Encoder[Agent] = Encoder.instance { a =>
      Json.obj(
        "name" -> a.name.fold(Json.Null)(Json.fromString)
      )
    }

    implicit val siteMetadataDecoder: Decoder[SiteMetadata] = Decoder.instance { c =>
      for {
        name <- c.downField("name").as[Option[String]]
        alternateName <- _string(c, "alternate_name", "alternateName")
        url <- c.downField("url").as[Option[String]]
        description <- c.downField("description").as[Option[String]]
        author <- c.downField("author").as[Option[Agent]]
        publisher <- c.downField("publisher").as[Option[Agent]]
        inLanguage <- _string_vector(c, "in_language", "inLanguage")
        license <- c.downField("license").as[Option[String]]
        keywords <- c.downField("keywords").as[Option[Vector[String]]]
        datePublished <- _string(c, "date_published", "datePublished")
        dateModified <- _string(c, "date_modified", "dateModified")
      } yield SiteMetadata(
        name = name,
        alternateName = alternateName,
        url = url,
        description = description,
        author = author.filterNot(_.isEmpty),
        publisher = publisher.filterNot(_.isEmpty),
        inLanguage = inLanguage,
        license = license,
        keywords = keywords.getOrElse(Vector.empty),
        datePublished = datePublished,
        dateModified = dateModified
      )
    }

    implicit val siteMetadataEncoder: Encoder[SiteMetadata] = Encoder.instance { a =>
      Json.obj(
        "name" -> a.name.fold(Json.Null)(Json.fromString),
        "alternate_name" -> a.alternateName.fold(Json.Null)(Json.fromString),
        "url" -> a.url.fold(Json.Null)(Json.fromString),
        "description" -> a.description.fold(Json.Null)(Json.fromString),
        "author" -> a.author.fold(Json.Null)(_.asJson),
        "publisher" -> a.publisher.fold(Json.Null)(_.asJson),
        "in_language" -> Json.arr(a.inLanguage.map(Json.fromString): _*),
        "license" -> a.license.fold(Json.Null)(Json.fromString),
        "keywords" -> Json.arr(a.keywords.map(Json.fromString): _*),
        "date_published" -> a.datePublished.fold(Json.Null)(Json.fromString),
        "date_modified" -> a.dateModified.fold(Json.Null)(Json.fromString)
      )
    }

    private def _string(
      c: HCursor,
      snake: String,
      camel: String
    ): Decoder.Result[Option[String]] =
      c.downField(snake).as[Option[String]].flatMap {
        case Some(s) => Right(Some(s))
        case None => c.downField(camel).as[Option[String]]
      }

    private def _string_vector(
      c: HCursor,
      snake: String,
      camel: String
    ): Decoder.Result[Vector[String]] =
      c.downField(snake).as[Option[Vector[String]]].flatMap {
        case Some(s) => Right(s)
        case None => c.downField(camel).as[Option[Vector[String]]].map(_.getOrElse(Vector.empty))
      }
  }

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
      meta: DocumentMetaData, // SmartDox document metadata
      extraTriples: Seq[Triple] = Seq.empty
    ) extends SiteResource {
      val typeTriple =
        Triple(Node.Uri(id), RdfType, Dcterms.node.BibliographicResource)

      override def toTriples: Seq[Triple] =
        super.toTriples ++ extraTriples
    }
    object Bibliography {
      def create(
        path: URI,
        meta: DocumentMetaData,
        entrytype: Option[String] = None,
        identifiers: Vector[String] = Vector.empty,
        sourceurl: Option[String] = None,
        citation: Option[String] = None,
        terms: Vector[String] = Vector.empty
      ): Bibliography = {
        val cid = _create_canonical_id(path)
        val subject = Node.Uri(cid)
        val triples =
          entrytype.toVector.map(x => Triple(subject, Dcterms.node.type_, Node.Literal(x))) ++
          identifiers.map(x => Triple(subject, Dcterms.node.identifier, Node.Literal(x))) ++
          sourceurl.toVector.map(x => Triple(subject, Dcterms.node.source, _uri_or_literal(x))) ++
          citation.toVector.map(x => Triple(subject, Schema.node.citation, Node.Literal(x))) ++
          terms.map(x => Triple(subject, Dcterms.node.subject, Node.Literal(x)))
        Bibliography(cid, meta, triples)
      }

      private def _uri_or_literal(value: String): Node =
        try {
          val uri = new URI(value)
          if (uri.isAbsolute) Node.Uri(value) else Node.Literal(value)
        } catch {
          case _: Exception => Node.Literal(value)
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
    siteMetadata: SiteMetadata = SiteMetadata.empty,
    ontology: Option[String] = None,
    schema: Option[String] = None,
    vocabulary: Option[String] = None,
    videoPublications: Seq[PublishMetadata.VideoPublication] = Seq.empty,
    publicationTriples: Seq[Triple] = Seq.empty
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
      ) ++
      siteMetadata.toTriples(root) ++
      (if (siteMetadata.isEmpty) Vector.empty else Vector(
        Triple(root, RdfType, Node.Uri(schemaUri("WebSite")))
      ))

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

      val videoTriples: Seq[Triple] =
        videoPublications.flatMap(_video_publication_triples(root, _))

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
        videoTriples ++
        publicationTriples ++
        bokTriples
      )
    }

    private def _video_publication_triples(root: Node.Uri, video: PublishMetadata.VideoPublication): Seq[Triple] = {
      val version = if (video.version.nonEmpty) video.version else "default"
      val videonode = Node.Uri(uri(s"Video/${_iri_segment(video.name)}/${_iri_segment(version)}"))
      val documenttriples = video.sourcePackage.map { source =>
        val document = Node.Uri(_video_document_uri(source))
        Vector(
          Triple(document, Node.Uri(uri("hasVideo")), videonode),
          Triple(videonode, Node.Uri(Vocabulary.Dcterms.source), document)
        )
      }.getOrElse(Vector.empty)
      val base = Vector(
        Triple(root, Node.Uri(uri("hasVideo")), videonode),
        Triple(videonode, RdfType, Node.Uri(schemaUri("VideoObject"))),
        Triple(videonode, Rdfs.node.label, Node.Literal(video.name))
      )
      val artifacts =
        video.artifact.toVector.flatMap(_video_artifact_triples(videonode, "hasArtifact", _)) ++
        video.caption.toVector.flatMap(_video_artifact_triples(videonode, "hasCaption", _)) ++
        video.transcript.toVector.flatMap(_video_artifact_triples(videonode, "hasTranscript", _)) ++
        video.rdf.toVector.flatMap(_video_rdf_triples(videonode, _))
      base ++ documenttriples ++ artifacts
    }

    private def _video_rdf_triples(videonode: Node.Uri, rdf: PublishMetadata.VideoRdfPublication): Seq[Triple] = {
      val version = if (rdf.version.nonEmpty) rdf.version else "default"
      val registry = Node.Uri(uri(s"VideoRdf/${_iri_segment(rdf.name)}/${_iri_segment(version)}"))
      val base = Vector(
        Triple(videonode, Node.Uri(uri("hasRdfRegistry")), registry),
        Triple(registry, Rdfs.node.label, Node.Literal(rdf.registryPath))
      )
      base ++
        rdf.turtle.toVector.flatMap(_video_artifact_triples(videonode, "hasRdfArtifact", _)) ++
        rdf.jsonLd.toVector.flatMap(_video_artifact_triples(videonode, "hasRdfArtifact", _)) ++
        rdf.manifest.toVector.flatMap(_video_artifact_triples(videonode, "hasRdfManifest", _))
    }

    private def _video_artifact_triples(
      videonode: Node.Uri,
      relation: String,
      artifact: PublishMetadata.VideoArtifactReference
    ): Seq[Triple] = {
      val artifactnode = Node.Uri(_public_uri(artifact.publicPath))
      Vector(
        Triple(videonode, Node.Uri(uri(relation)), artifactnode),
        Triple(artifactnode, RdfType, Node.Uri(schemaUri("MediaObject"))),
        Triple(artifactnode, Node.Uri(schemaUri("contentUrl")), Node.Uri(_public_uri(artifact.publicPath))),
        Triple(artifactnode, Node.Uri(schemaUri("encodingFormat")), Node.Literal(artifact.kind))
      ) ++ artifact.sha256.map(x => Triple(artifactnode, Node.Uri(schemaUri("sha256")), Node.Literal(x))).toVector
    }

    private def _video_document_uri(sourcepackage: String): String = {
      val path = sourcepackage.stripPrefix("/").stripSuffix(".video")
      s"https://www.simplemodeling.org/$path"
    }

    private def _public_uri(path: String): String =
      if (path.startsWith("http://") || path.startsWith("https://"))
        path
      else
        s"https://www.simplemodeling.org/${path.stripPrefix("/")}"

    private def _iri_segment(value: String): String =
      value.toLowerCase(Locale.ROOT).map {
        case c if c.isLetterOrDigit => c
        case '-' => '-'
        case '_' => '-'
        case _ => '-'
      }.mkString.replaceAll("-+", "-").stripPrefix("-").stripSuffix("-") match {
        case "" => "unknown"
        case s => s
      }

    private def _to_triples(sourcenode: Node)(p: DoxLinks) = {
      val internaltriples = p.internalLinks.links.flatMap { link =>
        val targetid = rule.resourceIri(sourcenode, link)
        resources.find(_.id == targetid).toSeq.flatMap(_ => rule.articleToArticle(sourcenode, targetid))
      }

      val glossarytriples = p.glossaryLinks.links.flatMap { link =>
        val targetid = rule.resourceIri(sourcenode, link)
        resources.find(_.id == targetid).toSeq.flatMap(_ => rule.articleToGlossaryUse(sourcenode, targetid))
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
      resources: Seq[SiteResource],
      siteMetadata: SiteMetadata = SiteMetadata.empty,
      videoPublications: Seq[PublishMetadata.VideoPublication] = Nil,
      publicationTriples: Seq[Triple] = Nil
    ): SiteModel = {
      val locals = List("ja", "en")
      val o = SimpleModelingOrgPublicOntology.namespace.stripSuffix("#") + "/index.jsonld"
      val s = SimpleModelingOrgPublicSchema.namespace.stripSuffix("#") + "/index.jsonld"
      val v = Vocabulary.Rdf.namespace.stripSuffix("#")
      SiteModel(metadata, resources, locals, siteMetadata, Some(o), Some(s), Some(v), videoPublications, publicationTriples)
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
  private def _default_policy: RdfRenderer.Policy =
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
      policy = _default_policy
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
