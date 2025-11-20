package org.smartdox.semanticweb

/*
 * Project Site Ontology
 * ----------------------------------------------------------------------
 * Defines the RDF/OWL vocabulary for the project-site layer
 * in the SimpleModeling BoK publication.
 * This ontology represents how software projects are structured
 * and linked within the BoK site (e.g., project index, modules, components).
 *
 * @since   Nov. 13, 2025
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
object ProjectSiteOntology {
  val prefix = "projectsite"
  val namespace = "https://www.simplemodeling.org/project-site/ontology/0.1-SNAPSHOT#"
  def uri(local: String) = namespace + local

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val ProjectSite   = uri("ProjectSite")     // The overall site for projects
  val ProjectPage   = uri("ProjectPage")     // A project-level page
  val ProjectFamily = uri("ProjectFamily")   // A collection of related projects
  val Module        = uri("Module")          // A module within a project
  val Component     = uri("Component")       // A functional or software component

  // ------------------------------------------------------------------
  // Properties
  // ------------------------------------------------------------------
  val hasProject     = uri("hasProject")     // ProjectSite → ProjectPage
  val hasFamily      = uri("hasFamily")      // ProjectSite → ProjectFamily
  val hasModule      = uri("hasModule")      // ProjectPage → Module
  val hasComponent   = uri("hasComponent")   // Module → Component
  val represents     = uri("represents")     // Any → BoK or modeling concept
  val dependsOn      = uri("dependsOn")      // Project or Module dependency
  val relatedTo      = uri("relatedTo")      // Cross-project or module relationship
  val taggedWith     = uri("taggedWith")     // Project → Glossary term or tag
  val order          = uri("order")          // Display order
  val title          = uri("title")          // Human-readable title
  val description    = uri("description")    // Text description

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    "ProjectSite"   -> ProjectSite,
    "ProjectPage"   -> ProjectPage,
    "ProjectFamily" -> ProjectFamily,
    "Module"        -> Module,
    "Component"     -> Component,
    "hasProject"    -> hasProject,
    "hasFamily"     -> hasFamily,
    "hasModule"     -> hasModule,
    "hasComponent"  -> hasComponent,
    "represents"    -> represents,
    "dependsOn"     -> dependsOn,
    "relatedTo"     -> relatedTo,
    "taggedWith"    -> taggedWith,
    "order"         -> order,
    "title"         -> title,
    "description"   -> description
  )
}
