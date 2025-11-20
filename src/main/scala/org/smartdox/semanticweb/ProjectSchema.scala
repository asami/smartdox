package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.semanticweb.ProjectOntology._

/*
 * Project Schema
 * ----------------------------------------------------------------------
 * Structural schema for RDF/JSON-LD representation of projects
 * within the SimpleModeling software ecosystem.
 *
 * It provides helper builders to generate RDF graphs describing:
 *   - Projects and project families
 *   - Modules and components
 *   - Repositories and builds
 *
 * Linked Ontologies:
 *   - ProjectOntology (vocabulary)
 *   - SimpleModelOntology (model linkage)
 *
 * Namespace: https://www.simplemodeling.org/project/schema/0.1-SNAPSHOT#
 *
 * @since   Nov. 13, 2025
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
object ProjectSchema {
  val prefix = "project-schema"
  val namespace = "https://www.simplemodeling.org/project/schema/0.1-SNAPSHOT#"
  def uri(local: String): String = namespace + local

  // ------------------------------------------------------------------
  // Node Builders
  // ------------------------------------------------------------------
  def projectNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def moduleNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def componentNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def repoNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def buildNode(uriStr: String): Node.Uri = Node.Uri(uriStr)

  // ------------------------------------------------------------------
  // Project Triples
  // ------------------------------------------------------------------
  def projectTriples(
    projectId: String,
    title: String,
    description: String,
    version: Option[String] = None,
    language: Option[String] = None,
    license: Option[String] = None,
    owner: Option[String] = None,
    status: Option[String] = None,
    modules: Seq[String] = Seq.empty,
    repos: Seq[String] = Seq.empty,
    builds: Seq[String] = Seq.empty,
    depends: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = projectNode(projectId)

    val moduleTriples = modules.map(mid => Triple(subject, Node.Uri(hasModule), moduleNode(mid)))
    val repoTriples   = repos.map(rid => Triple(subject, Node.Uri(hasRepository), repoNode(rid)))
    val buildTriples  = builds.map(bid => Triple(subject, Node.Uri(hasBuild), buildNode(bid)))
    val depTriples    = depends.map(did => Triple(subject, Node.Uri(dependsOn), projectNode(did)))

    val literalTriples = Seq(
      Option(title).map(v => Triple(subject, Node.Uri(ProjectOntology.title), Node.Literal(v))),
      Option(description).map(v => Triple(subject, Node.Uri(ProjectOntology.description), Node.Literal(v))),
      version.map(v => Triple(subject, Node.Uri(ProjectOntology.version), Node.Literal(v))),
      language.map(v => Triple(subject, Node.Uri(ProjectOntology.language), Node.Literal(v))),
      license.map(v => Triple(subject, Node.Uri(ProjectOntology.license), Node.Literal(v))),
      owner.map(v => Triple(subject, Node.Uri(ProjectOntology.owner), Node.Literal(v))),
      status.map(v => Triple(subject, Node.Uri(ProjectOntology.status), Node.Literal(v)))
    ).flatten

    // Link ontology and schema (meta triple)
    val schemaTriple = Triple(subject, Node.Uri(ProjectOntology.usesSchema), Node.Uri(namespace + "Project"))

    Seq(Triple(subject, RdfType, Node.Uri(Project))) ++
      moduleTriples ++ repoTriples ++ buildTriples ++ depTriples ++ literalTriples ++ Seq(schemaTriple)
  }

  // ------------------------------------------------------------------
  // Module Triples
  // ------------------------------------------------------------------
  def moduleTriples(
    moduleId: String,
    title: String,
    components: Seq[String] = Seq.empty,
    depends: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = moduleNode(moduleId)
    val compTriples = components.map(cid => Triple(subject, Node.Uri(hasComponent), componentNode(cid)))
    val depTriples  = depends.map(mid => Triple(subject, Node.Uri(dependsOn), moduleNode(mid)))

    Seq(
      Triple(subject, RdfType, Node.Uri(Module)),
      Triple(subject, Node.Uri(ProjectOntology.title), Node.Literal(title))
    ) ++ compTriples ++ depTriples
  }

  // ------------------------------------------------------------------
  // Component Triples
  // ------------------------------------------------------------------
  def componentTriples(componentId: String, title: String): Seq[Triple] = {
    val subject = componentNode(componentId)
    Seq(
      Triple(subject, RdfType, Node.Uri(Component)),
      Triple(subject, Node.Uri(ProjectOntology.title), Node.Literal(title))
    )
  }

  // ------------------------------------------------------------------
  // Repository Triples
  // ------------------------------------------------------------------
  def repoTriples(repoId: String, url: String): Seq[Triple] = {
    val subject = repoNode(repoId)
    Seq(
      Triple(subject, RdfType, Node.Uri(Repository)),
      Triple(subject, Node.Uri(ProjectOntology.url), Node.Literal(url))
    )
  }

  // ------------------------------------------------------------------
  // Build Triples
  // ------------------------------------------------------------------
  def buildTriples(buildId: String, desc: String): Seq[Triple] = {
    val subject = buildNode(buildId)
    Seq(
      Triple(subject, RdfType, Node.Uri(Build)),
      Triple(subject, Node.Uri(ProjectOntology.description), Node.Literal(desc))
    )
  }

  // ------------------------------------------------------------------
  // Graph Builder
  // ------------------------------------------------------------------
  def toGraph(
    projectId: String,
    title: String,
    description: String,
    modules: Seq[(String, String)] = Seq.empty
  ): Graph = {
    val projTriples = projectTriples(projectId, title, description, modules = modules.map(_._1))
    val modTriples = modules.flatMap { case (mid, mtitle) => moduleTriples(mid, mtitle) }

    val allTriples = projTriples ++ modTriples
    Graph(allTriples)
  }
}
