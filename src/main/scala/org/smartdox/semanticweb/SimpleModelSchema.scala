package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}
import org.smartdox.semanticweb.SimpleModelOntology._

/*
 * SimpleModel Schema
 * ----------------------------------------------------------------------
 * Defines the RDF/JSON-LD structural schema for the Object-Functional Model
 * elements used in SimpleModeling.
 *
 * This schema generates RDF graphs for entities, values, rules, services,
 * and events, based on the vocabulary defined in SimpleModelOntology.
 *
 * Namespace: https://www.simplemodeling.org/simplemodel/schema/1.0#
 * Prefix: sm-schema
 *
 * @since   Nov. 13, 2025
 * @version Nov. 13, 2025
 * @author  ASAMI, Tomoharu
 */
object SimpleModelSchema {
  val prefix = "sm-schema"
  val namespace = "https://www.simplemodeling.org/simplemodel/schema/1.0#"
  def uri(local: String): String = namespace + local

  // ------------------------------------------------------------------
  // Node Builders
  // ------------------------------------------------------------------
  def entityNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def valueNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def ruleNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def serviceNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def eventNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def componentNode(uriStr: String): Node.Uri = Node.Uri(uriStr)
  def subsystemNode(uriStr: String): Node.Uri = Node.Uri(uriStr)

  // ------------------------------------------------------------------
  // Entity Triples
  // ------------------------------------------------------------------
  def entityTriples(
    id: String,
    name: String,
    description: Option[String] = None,
    attributes: Seq[(String, String)] = Seq.empty, // (name, type)
    relations: Seq[(String, String)] = Seq.empty   // (name, target)
  ): Seq[Triple] = {
    val subject = entityNode(id)
    val attrTriples = attributes.map { case (n, t) =>
      Triple(subject, Node.Uri(hasAttribute), Node.Literal(s"$n: $t"))
    }
    val relTriples = relations.map { case (n, target) =>
      Triple(subject, Node.Uri(hasRelation), Node.Literal(s"$n -> $target"))
    }

    val core = Seq(
      Triple(subject, RdfType, Node.Uri(Entity)),
      Triple(subject, Node.Uri(SimpleModelOntology.name), Node.Literal(name))
    )
    val desc = description.map(d => Triple(subject, Node.Uri(SimpleModelOntology.description), Node.Literal(d)))

    core ++ desc ++ attrTriples ++ relTriples
  }

  // ------------------------------------------------------------------
  // Value Triples
  // ------------------------------------------------------------------
  def valueTriples(
    id: String,
    name: String,
    baseType: Option[String] = None,
    description: Option[String] = None
  ): Seq[Triple] = {
    val subject = valueNode(id)
    val core = Seq(
      Triple(subject, RdfType, Node.Uri(Value)),
      Triple(subject, Node.Uri(SimpleModelOntology.name), Node.Literal(name))
    )
    val base = baseType.map(bt => Triple(subject, Node.Uri(SimpleModelOntology.relation), Node.Literal(bt)))
    val desc = description.map(d => Triple(subject, Node.Uri(SimpleModelOntology.description), Node.Literal(d)))

    core ++ base ++ desc
  }

  // ------------------------------------------------------------------
  // Rule Triples
  // ------------------------------------------------------------------
  def ruleTriples(
    id: String,
    title: String,
    condition: String,
    consequence: String
  ): Seq[Triple] = {
    val subject = ruleNode(id)
    Seq(
      Triple(subject, RdfType, Node.Uri(Rule)),
      Triple(subject, Node.Uri(SimpleModelOntology.title), Node.Literal(title)),
      Triple(subject, Node.Uri(SimpleModelOntology.relation), Node.Literal(s"if $condition then $consequence"))
    )
  }

  // ------------------------------------------------------------------
  // Service Triples
  // ------------------------------------------------------------------
  def serviceTriples(
    id: String,
    title: String,
    input: Option[String] = None,
    output: Option[String] = None,
    description: Option[String] = None
  ): Seq[Triple] = {
    val subject = serviceNode(id)
    val ioTriples = Seq(
      input.map(i => Triple(subject, Node.Uri(SimpleModelOntology.attribute), Node.Literal(s"input: $i"))),
      output.map(o => Triple(subject, Node.Uri(SimpleModelOntology.attribute), Node.Literal(s"output: $o")))
    ).flatten

    val desc = description.map(d => Triple(subject, Node.Uri(SimpleModelOntology.description), Node.Literal(d)))

    Seq(
      Triple(subject, RdfType, Node.Uri(Service)),
      Triple(subject, Node.Uri(SimpleModelOntology.title), Node.Literal(title))
    ) ++ ioTriples ++ desc
  }

  // ------------------------------------------------------------------
  // Event Triples
  // ------------------------------------------------------------------
  def eventTriples(
    id: String,
    title: String,
    trigger: Option[String] = None,
    result: Option[String] = None
  ): Seq[Triple] = {
    val subject = eventNode(id)
    val triggerTriple = trigger.map(t => Triple(subject, Node.Uri(SimpleModelOntology.attribute), Node.Literal(s"trigger: $t")))
    val resultTriple  = result.map(r => Triple(subject, Node.Uri(SimpleModelOntology.attribute), Node.Literal(s"result: $r")))
    Seq(
      Triple(subject, RdfType, Node.Uri(Event)),
      Triple(subject, Node.Uri(SimpleModelOntology.title), Node.Literal(title))
    ) ++ triggerTriple ++ resultTriple
  }

  // ------------------------------------------------------------------
  // Component Triples
  // ------------------------------------------------------------------
  def componentTriples(
    id: String,
    title: String,
    entities: Seq[String] = Seq.empty,
    services: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = componentNode(id)
    val entityTriples = entities.map(eid => Triple(subject, Node.Uri(hasRelation), entityNode(eid)))
    val serviceTriples = services.map(sid => Triple(subject, Node.Uri(hasRelation), serviceNode(sid)))

    Seq(
      Triple(subject, RdfType, Node.Uri(Component)),
      Triple(subject, Node.Uri(SimpleModelOntology.title), Node.Literal(title))
    ) ++ entityTriples ++ serviceTriples
  }

  // ------------------------------------------------------------------
  // Subsystem Triples
  // ------------------------------------------------------------------
  def subsystemTriples(
    id: String,
    title: String,
    components: Seq[String] = Seq.empty
  ): Seq[Triple] = {
    val subject = subsystemNode(id)
    val compTriples = components.map(cid => Triple(subject, Node.Uri(hasRelation), componentNode(cid)))
    Seq(
      Triple(subject, RdfType, Node.Uri(Subsystem)),
      Triple(subject, Node.Uri(SimpleModelOntology.title), Node.Literal(title))
    ) ++ compTriples
  }

  // ------------------------------------------------------------------
  // Graph Builder
  // ------------------------------------------------------------------
  def toGraph(
    entities: Seq[(String, String)] = Seq.empty,
    values: Seq[(String, String)] = Seq.empty,
    rules: Seq[(String, String, String, String)] = Seq.empty,
    services: Seq[(String, String)] = Seq.empty,
    events: Seq[(String, String)] = Seq.empty
  ): Graph = {
    val entityTriplesSeq = entities.flatMap { case (id, name) => entityTriples(id, name) }
    val valueTriplesSeq  = values.flatMap { case (id, name) => valueTriples(id, name) }
    val ruleTriplesSeq   = rules.flatMap { case (id, title, cond, cons) => ruleTriples(id, title, cond, cons) }
    val serviceTriplesSeq= services.flatMap { case (id, title) => serviceTriples(id, title) }
    val eventTriplesSeq  = events.flatMap { case (id, title) => eventTriples(id, title) }

    Graph(entityTriplesSeq ++ valueTriplesSeq ++ ruleTriplesSeq ++ serviceTriplesSeq ++ eventTriplesSeq)
  }
}
