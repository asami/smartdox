package org.smartdox.semanticweb

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
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
object ProjectOntology {
  val prefix = "proj"
  val namespace = "https://www.simplemodeling.org/project/ontology/0.1-SNAPSHOT#"
  def uri(local: String) = namespace + local

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
  // JSON-LD Context
  // ------------------------------------------------------------------
  lazy val jsonldContext: Map[String, Any] = Map(
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

