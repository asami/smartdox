package org.smartdox.semanticweb

/*
 * SimpleModeling.org Ontology
 * ----------------------------------------------------------------------
 * Defines the meta-level RDF/OWL vocabulary connecting
 * SimpleModeling, SmartDox, BoK, Project, Category, and Glossary ontologies.
 *
 * This ontology acts as a meta-metamodel vocabulary for the
 * entire SimpleModeling.org knowledge ecosystem.
 *
 * @since   Nov. 12, 2025
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary.{Rdf => VocaRdf, Rdfs, Owl}
import org.smartdox.semanticweb.Rdf.Node

object SimpleModelingOrgOntology extends KnowledgeModel {
  val prefix = "smorg"
  val namespace = "https://www.simplemodeling.org/ontology/0.1-SNAPSHOT#"

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val Ontology       = uri("Ontology")
  val Schema         = uri("Schema")
  val Site           = uri("Site")
  val KnowledgeBase  = uri("KnowledgeBase")
  val Vocabulary     = uri("Vocabulary")
  val Module         = uri("Module")
  val Component      = uri("Component")
  val System         = uri("System")

  // ------------------------------------------------------------------
  // Relationships among ontologies and schemas
  // ------------------------------------------------------------------
  val definesVocabulary = uri("definesVocabulary") // Ontology → Vocabulary
  val definesSchema     = uri("definesSchema")     // Ontology → Schema
  val governsSite       = uri("governsSite")       // Ontology → Site
  val includesOntology  = uri("includesOntology")  // KnowledgeBase → Ontology
  val includesSchema    = uri("includesSchema")    // KnowledgeBase → Schema
  val includesSite      = uri("includesSite")      // KnowledgeBase → Site
  val partOfSystem      = uri("partOfSystem")      // Component → System
  val includesModule    = uri("includesModule")    // System → Module
  val alignsWith        = uri("alignsWith")        // Ontology連携
  val supersedes        = uri("supersedes")        // 旧バージョン関係
  val hasVersion        = uri("hasVersion")        // バージョン表現

  // ------------------------------------------------------------------
  // Linked Ontologies (namespaces)
  // ------------------------------------------------------------------
  val SimpleModelingOntology  = "https://www.simplemodeling.org/simplemodeling/ontology/0.1-SNAPSHOT#"
  val SmartDoxOntology        = "https://www.simplemodeling.org/smartdox/ontology/0.1-SNAPSHOT#"
  val BokOntology             = "https://www.simplemodeling.org/bok/ontology/0.1-SNAPSHOT#"
  val ProjectOntology         = "https://www.simplemodeling.org/project/ontology/0.1-SNAPSHOT#"
  val CategoryOntology        = "https://www.simplemodeling.org/category/ontology/0.1-SNAPSHOT#"
  val GlossaryOntology        = "https://www.simplemodeling.org/glossary/ontology/0.1-SNAPSHOT#"

  // ------------------------------------------------------------------
  // Hierarchical relationships (OWL/RDFS subclass semantics)
  // ------------------------------------------------------------------
  val subclassOf: Seq[(String, String)] = Seq(
    Schema -> Ontology,
    Site -> Ontology,
    Vocabulary -> Ontology,
    KnowledgeBase -> Ontology,
    Component -> Module,
    Module -> System
  )

  // ------------------------------------------------------------------
  // Human-readable labels and comments (optional)
  // ------------------------------------------------------------------
  val label: Map[String, String] = Map(
    Ontology -> "Ontology",
    Schema -> "Schema",
    Site -> "Site",
    KnowledgeBase -> "Knowledge Base",
    Vocabulary -> "Vocabulary",
    Module -> "Module",
    Component -> "Component",
    System -> "System"
  )

  val comment: Map[String, String] = Map(
    Ontology -> "A formal specification of a conceptual model or vocabulary.",
    Schema -> "A concrete data-level schema derived from an ontology.",
    Site -> "A web or documentation site managed under an ontology.",
    KnowledgeBase -> "A collection of ontologies, schemas, and sites forming a knowledge system."
  )

  // ------------------------------------------------------------------
  // JSON-LD context for export
  // ------------------------------------------------------------------
  lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "Ontology" -> Ontology,
    "Schema" -> Schema,
    "Site" -> Site,
    "KnowledgeBase" -> KnowledgeBase,
    "Vocabulary" -> Vocabulary,
    "definesVocabulary" -> definesVocabulary,
    "definesSchema" -> definesSchema,
    "governsSite" -> governsSite,
    "includesOntology" -> includesOntology,
    "includesSchema" -> includesSchema,
    "includesSite" -> includesSite,
    "alignsWith" -> alignsWith,
    "hasVersion" -> hasVersion,
    "supersedes" -> supersedes
  )

  // ------------------------------------------------------------------
  // RDF Graph Construction
  // ------------------------------------------------------------------
  def toGraph: Graph = {
    val ontologyNode = Node.Uri(namespace)

    val baseTriples = Seq(
      Triple(ontologyNode, VocaRdf.node.`type`, Node.Uri(Owl.Ontology)),
      Triple(ontologyNode, Node.Uri(hasVersion), Node.Literal("0.1-SNAPSHOT"))
    )

    val labelTriples = label.toSeq.map { case (cls, lbl) =>
      Triple(Node.Uri(cls), Node.Uri(Rdfs.label), Node.Literal(lbl))
    }

    val commentTriples = comment.toSeq.map { case (cls, cm) =>
      Triple(Node.Uri(cls), Node.Uri(Rdfs.comment), Node.Literal(cm))
    }

    val subclassTriples = subclassOf.map { case (child, parent) =>
      Triple(Node.Uri(child), Node.Uri(Rdfs.subClassOf), Node.Uri(parent))
    }

    Graph(baseTriples ++ labelTriples ++ commentTriples ++ subclassTriples)
  }

  def jsonldProfile: RdfRenderer.JsonLDProfile = RdfRenderer.JsonLDProfile.BoK
}
