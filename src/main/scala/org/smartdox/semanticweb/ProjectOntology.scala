package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.semanticweb.Vocabulary.Owl

/*
 * Project Ontology
 * ----------------------------------------------------------------------
 * Defines the software-side vocabulary of the SimpleModeling Body of Knowledge.
 * Each project corresponds to a software initiative that applies
 * the SimpleModeling methodology and defines domain models (SimpleModel).
 *
 * This ontology is linked with:
 *   - SimpleModelOntology (for model-level concepts)
 *   - ProjectSchema (for instance-level RDF/JSON-LD structure)
 *
 * Namespace: https://www.simplemodeling.org/project/ontology/0.1-SNAPSHOT#
 * Prefix: proj
 *
 * @since   Nov. 13, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object ProjectOntology extends OntologyModel {
  override val prefix: String = Vocabulary.Project.prefix
  override val namespace: String = Vocabulary.Project.namespace
  override def uri(local: String): String = Vocabulary.Project.uri(local)

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val Project        = uri("Project")
  val ProjectFamily  = uri("ProjectFamily")
  val Module         = uri("Module")
  val Component      = uri("Component")
  val Build          = uri("Build")
  val Release        = uri("Release")
  val Artifact       = uri("Artifact")
  val Documentation  = uri("Documentation")
  val Repository     = uri("Repository")
  val Contributor    = uri("Contributor")
  val License        = uri("License")

  // ------------------------------------------------------------------
  // Common Literal Properties
  // ------------------------------------------------------------------
  val title          = uri("title")
  val description    = uri("description")
  val version        = uri("version")
  val language       = uri("language")
  val license        = uri("licenseText")
  val owner          = uri("owner")
  val status         = uri("status")
  val url            = uri("url")

  // ------------------------------------------------------------------
  // Structural Relations
  // ------------------------------------------------------------------
  val hasModule        = uri("hasModule")        // Project → Module
  val hasComponent     = uri("hasComponent")     // Module → Component
  val hasBuild         = uri("hasBuild")         // Project → Build
  val hasRelease       = uri("hasRelease")       // Project → Release
  val hasArtifact      = uri("hasArtifact")      // Project → Artifact
  val hasDocumentation = uri("hasDocumentation") // Project → Documentation
  val hasRepository    = uri("hasRepository")    // Project → Repository
  val hasContributor   = uri("hasContributor")   // Project → Contributor
  val hasLicense       = uri("hasLicense")       // Project → License
  val belongsToFamily  = uri("belongsToFamily")  // Project → ProjectFamily
  val dependsOn        = uri("dependsOn")        // Module/Project dependency

  // ------------------------------------------------------------------
  // Integration with Modeling Ontologies
  // ------------------------------------------------------------------
  val usesModel        = uri("usesModel")          // Project → sm:Entity / sm:Model
  val definesModel     = uri("definesModel")       // Project → sm:Entity
  val basedOnMethodology = uri("basedOnMethodology") // Project → smont:Methodology

  // ------------------------------------------------------------------
  // Integration with ProjectSchema
  // ------------------------------------------------------------------
  val hasSchema        = uri("hasSchema")          // Ontology → Schema
  val usesSchema       = uri("usesSchema")         // Instance → Schema

  // ------------------------------------------------------------------
  // Cross-links to other ontologies
  // ------------------------------------------------------------------
  val SimpleModelOntology    = "https://www.simplemodeling.org/simplemodel/ontology/0.1-SNAPSHOT#"
  val SimpleModelingOntology = "https://www.simplemodeling.org/simplemodeling/ontology/0.1-SNAPSHOT#"

  // ------------------------------------------------------------------
  // TBox: Core class/property declarations
  // ------------------------------------------------------------------
  override def toTriples: Seq[Triple] = Seq(
    // Classes
    Triple(Node.Uri(Project),       RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(ProjectFamily), RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Module),        RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Component),     RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Build),         RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Release),       RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Artifact),      RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Documentation), RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Repository),    RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(Contributor),   RdfType, Node.Uri(Owl.Class)),
    Triple(Node.Uri(License),       RdfType, Node.Uri(Owl.Class)),

    // Datatype Properties
    Triple(Node.Uri(title),       RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(description), RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(version),     RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(language),    RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(license),     RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(owner),       RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(status),      RdfType, Node.Uri(Owl.DatatypeProperty)),
    Triple(Node.Uri(url),         RdfType, Node.Uri(Owl.DatatypeProperty)),

    // Object Properties
    Triple(Node.Uri(hasModule),        RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasComponent),     RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasBuild),         RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasRelease),       RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasArtifact),      RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasDocumentation), RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasRepository),    RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasContributor),   RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasLicense),       RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(belongsToFamily),  RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(dependsOn),        RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(usesModel),        RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(definesModel),     RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(basedOnMethodology), RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(hasSchema),        RdfType, Node.Uri(Owl.ObjectProperty)),
    Triple(Node.Uri(usesSchema),       RdfType, Node.Uri(Owl.ObjectProperty))
  )

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  override lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    // Classes
    "Project" -> Project,
    "ProjectFamily" -> ProjectFamily,
    "Module" -> Module,
    "Component" -> Component,
    "Build" -> Build,
    "Release" -> Release,
    "Artifact" -> Artifact,
    "Documentation" -> Documentation,
    "Repository" -> Repository,
    "Contributor" -> Contributor,
    "License" -> License,
    // Literals
    "title" -> title,
    "description" -> description,
    "version" -> version,
    "language" -> language,
    "license" -> license,
    "owner" -> owner,
    "status" -> status,
    "url" -> url,
    // Relations
    "hasModule" -> hasModule,
    "hasComponent" -> hasComponent,
    "hasBuild" -> hasBuild,
    "hasRelease" -> hasRelease,
    "hasArtifact" -> hasArtifact,
    "hasDocumentation" -> hasDocumentation,
    "hasRepository" -> hasRepository,
    "hasContributor" -> hasContributor,
    "hasLicense" -> hasLicense,
    "belongsToFamily" -> belongsToFamily,
    "dependsOn" -> dependsOn,
    // Meta-links
    "usesModel" -> usesModel,
    "definesModel" -> definesModel,
    "basedOnMethodology" -> basedOnMethodology,
    "hasSchema" -> hasSchema,
    "usesSchema" -> usesSchema
  )
}
