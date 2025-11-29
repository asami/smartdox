package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.ComponentRepositoryOntology._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.semanticweb.Vocabulary.Rdfs

/*
 * Component Repository Schema
 * ----------------------------------------------------------------------
 * Generates RDF triples for Component / ComponentVersion / Artifact /
 * Capability objects using the ComponentRepositoryOntology vocabulary.
 *
 * Namespace: https://www.simplemodeling.org/component/schema/0.1-SNAPSHOT#
 * Prefix: cmpo-schema
 *
 * @since   Nov. 28, 2025
 * @version Nov. 28, 2025
 * @author  ASAMI, Tomoharu
 */
object ComponentRepositorySchema extends SchemaModel {

  override val prefix: String = ComponentRepositoryOntology.prefix
  override val namespace: String = ComponentRepositoryOntology.namespace

  override lazy val jsonldContext: Map[String, Any] =
    ComponentRepositoryOntology.jsonldContext

  override def jsonldProfile: RdfRenderer.JsonLDProfile =
    RdfRenderer.JsonLDProfile.BoK

  // ---------------------------------------------------------
  // OWL Typing
  // ---------------------------------------------------------
  override def toTriples: Seq[Triple] = Seq(
    Triple(Node.Uri(ComponentRepositoryOntology.Component), RdfType, Node.Uri(Vocabulary.Owl.Class)),
    Triple(Node.Uri(ComponentRepositoryOntology.ComponentVersion), RdfType, Node.Uri(Vocabulary.Owl.Class)),
    Triple(Node.Uri(ComponentRepositoryOntology.Artifact), RdfType, Node.Uri(Vocabulary.Owl.Class)),
    Triple(Node.Uri(ComponentRepositoryOntology.Capability), RdfType, Node.Uri(Vocabulary.Owl.Class)),
    Triple(Node.Uri(ComponentRepositoryOntology.hasVersion), RdfType, Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Triple(Node.Uri(ComponentRepositoryOntology.hasArtifact), RdfType, Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Triple(Node.Uri(ComponentRepositoryOntology.provides), RdfType, Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Triple(Node.Uri(ComponentRepositoryOntology.requires), RdfType, Node.Uri(Vocabulary.Owl.ObjectProperty)),
    Triple(Node.Uri(ComponentRepositoryOntology.dependsOn), RdfType, Node.Uri(Vocabulary.Owl.ObjectProperty))
  )

  // ---------------------------------------------------------
  // Builders
  // ---------------------------------------------------------
  def componentNode(id: String)        = Node.Uri(id)
  def versionNode(id: String)          = Node.Uri(id)
  def artifactNode(id: String)         = Node.Uri(id)
  def capabilityNode(id: String)       = Node.Uri(id)

  // ---------------------------------------------------------
  // Component → ComponentVersion
  // ---------------------------------------------------------
  def componentTriples(
    id: String,
    versions: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = componentNode(id)
    val versionTriples =
      versions.map(vid => Triple(subject, ComponentRepositoryOntology.node.hasVersion, versionNode(vid)))

    Seq(
      Triple(subject, RdfType, ComponentRepositoryOntology.node.Component)
    ) ++ versionTriples
  }

  // ---------------------------------------------------------
  // ComponentVersion triples
  // ---------------------------------------------------------
  def versionTriples(
    id: String,
    provides: Seq[String] = Seq.empty,
    requires: Seq[String] = Seq.empty,
    depends: Seq[String] = Seq.empty,
    artifacts: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = versionNode(id)

    val prov = provides.map(cid => Triple(subject, ComponentRepositoryOntology.node.provides, capabilityNode(cid)))
    val req  = requires.map(cid => Triple(subject, ComponentRepositoryOntology.node.requires, capabilityNode(cid)))
    val dep  = depends.map(vid => Triple(subject, ComponentRepositoryOntology.node.dependsOn, versionNode(vid)))
    val arts = artifacts.map(aid => Triple(subject, ComponentRepositoryOntology.node.hasArtifact, artifactNode(aid)))

    Seq(Triple(subject, RdfType, ComponentRepositoryOntology.node.ComponentVersion)) ++
      prov ++ req ++ dep ++ arts
  }

  // ---------------------------------------------------------
  // Artifact triples
  // ---------------------------------------------------------
  def artifactTriples(
    id: String,
    url: Option[String] = None,
    digest: Option[String] = None
  ): Seq[Triple] = {
    val subject = artifactNode(id)
    val urlTriple   = url.map(u => Triple(subject, ComponentRepositoryOntology.node.downloadUrl, Node.Literal(u)))
    val digTriple   = digest.map(d => Triple(subject, ComponentRepositoryOntology.node.digest, Node.Literal(d)))

    Seq(Triple(subject, RdfType, ComponentRepositoryOntology.node.Artifact)) ++ urlTriple ++ digTriple
  }

  // ---------------------------------------------------------
  // Capability triples
  // ---------------------------------------------------------
  def capabilityTriples(
    id: String,
    label: String
  ): Seq[Triple] = Seq(
    Triple(capabilityNode(id), RdfType, ComponentRepositoryOntology.node.Capability),
    Triple(capabilityNode(id), Rdfs.node.label, Node.Literal(label))
  )

  // ---------------------------------------------------------
  // To Graph Builder
  // ---------------------------------------------------------
  def toGraph(
    components: Seq[(String, Seq[String])] = Seq.empty,
    versions: Seq[(String, Seq[String], Seq[String], Seq[String], Seq[String])] = Seq.empty,
    artifacts: Seq[(String, Option[String], Option[String])] = Seq.empty,
    capabilities: Seq[(String, String)] = Seq.empty
  ): Graph = {
    val comp = components.flatMap { case (id, vers) =>
      componentTriples(id, vers)
    }

    val versTriples = versions.flatMap { case (id, prov, req, dep, arts) =>
      versionTriples(id, prov, req, dep, arts)
    }

    val artTriples = artifacts.flatMap { case (id, url, dig) =>
      artifactTriples(id, url, dig)
    }

    val capTriples = capabilities.flatMap { case (id, label) =>
      capabilityTriples(id, label)
    }

    Graph(comp ++ versTriples ++ artTriples ++ capTriples)
  }
}
