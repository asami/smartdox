package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.semanticweb.ProjectSiteOntology._

/*
 * Project Site Schema
 * ----------------------------------------------------------------------
 * Defines the RDF/JSON-LD structural schema for the project-site layer
 * within the SimpleModeling BoK.
 * Used to generate JSON-LD or Turtle graphs for project family,
 * project pages, and their internal module/component structures.
 *
 * @since   Nov. 13, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object ProjectSiteSchema {
  // ------------------------------------------------------------------
  // Node Builders
  // ------------------------------------------------------------------
  def siteNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def familyNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def projectNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def moduleNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def componentNode(uriStr: String): Node.Uri = Node.Uri(uriStr)

  // ------------------------------------------------------------------
  // ProjectSite Triples
  // ------------------------------------------------------------------
  def siteTriples(siteId: String, families: Seq[String], projects: Seq[String]): Seq[Triple] = {
    val subject = siteNode(siteId)
    val familyTriples = families.map(fid => Triple(subject, Node.Uri(hasFamily), familyNode(fid)))
    val projectTriples = projects.map(pid => Triple(subject, Node.Uri(hasProject), projectNode(pid)))
    Seq(
      Triple(subject, RdfType, Node.Uri(ProjectSite)),
      Triple(subject, Rdfs.node.label, Node.Literal("Project Site"))
    ) ++ familyTriples ++ projectTriples
  }

  // ------------------------------------------------------------------
  // ProjectFamily Triples
  // ------------------------------------------------------------------
  def familyTriples(familyId: String, title: String, projects: Seq[String] = Seq.empty): Seq[Triple] = {
    val subject = familyNode(familyId)
    val projectTriples = projects.map(pid => Triple(subject, Node.Uri(hasProject), projectNode(pid)))
    Seq(
      Triple(subject, RdfType, Node.Uri(ProjectFamily)),
      Triple(subject, Node.Uri(ProjectSiteOntology.title), Node.Literal(title))
    ) ++ projectTriples
  }

  // ------------------------------------------------------------------
  // ProjectPage Triples
  // ------------------------------------------------------------------
  def projectTriples(
    projectId: String,
    title: String,
    description: String,
    modules: Seq[String] = Seq.empty,
    related: Seq[String] = Seq.empty,
    tags: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = projectNode(projectId)
    val moduleTriples = modules.map(mid => Triple(subject, Node.Uri(hasModule), moduleNode(mid)))
    val relatedTriples = related.map(rid => Triple(subject, Node.Uri(relatedTo), projectNode(rid)))
    val tagTriples = tags.map(tid => Triple(subject, Node.Uri(taggedWith), Node.Uri(tid)))
    Seq(
      Triple(subject, RdfType, Node.Uri(ProjectPage)),
      Triple(subject, Node.Uri(ProjectSiteOntology.title), Node.Literal(title)),
      Triple(subject, Node.Uri(ProjectSiteOntology.description), Node.Literal(description))
    ) ++ moduleTriples ++ relatedTriples ++ tagTriples
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
    val componentTriples = components.map(cid => Triple(subject, Node.Uri(hasComponent), componentNode(cid)))
    val dependencyTriples = depends.map(mid => Triple(subject, Node.Uri(dependsOn), moduleNode(mid)))
    Seq(
      Triple(subject, RdfType, Node.Uri(Module)),
      Triple(subject, Node.Uri(ProjectSiteOntology.title), Node.Literal(title))
    ) ++ componentTriples ++ dependencyTriples
  }

  // ------------------------------------------------------------------
  // Component Triples
  // ------------------------------------------------------------------
  def componentTriples(componentId: String, title: String): Seq[Triple] = {
    val subject = componentNode(componentId)
    Seq(
      Triple(subject, RdfType, Node.Uri(Component)),
      Triple(subject, Node.Uri(ProjectSiteOntology.title), Node.Literal(title))
    )
  }

  // ------------------------------------------------------------------
  // Graph Builder
  // ------------------------------------------------------------------
  def toGraph(
    siteId: String,
    families: Seq[(String, String, Seq[(String, String)])],
    standaloneProjects: Seq[(String, String, String)]
  ): Graph = {
    val siteTrip = siteTriples(siteId, families.map(_._1), standaloneProjects.map(_._1))
    val familyTrip = families.flatMap { case (fid, ftitle, projects) =>
      familyTriples(fid, ftitle, projects.map(_._1))
    }
    val projectTrip = families.flatMap { case (_, _, projects) =>
      projects.flatMap { case (pid, ptitle) => projectTriples(pid, ptitle, "") }
    } ++ standaloneProjects.flatMap { case (pid, ptitle, pdesc) =>
      projectTriples(pid, ptitle, pdesc)
    }
    Graph(siteTrip ++ familyTrip ++ projectTrip)
  }
}
