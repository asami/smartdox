package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf.Triple
import org.smartdox.semanticweb.Rdf.Node

/*
 * SimpleModel Ontology
 * ----------------------------------------------------------------------
 * Defines the Object-Functional Model vocabulary for SimpleModeling.
 *
 * This ontology describes the conceptual elements (Entity, Value, Rule,
 * Service, Event, Component, Subsystem, Attribute, Relation) used to
 * construct domain models and supports RDF/JSON-LD serialization
 * via SimpleModelSchema.
 *
 * Namespace: https://www.simplemodeling.org/simplemodel/ontology/0.1-SNAPSHOT#
 * Prefix: sm
 *
 * @since   Nov. 13, 2025
 * @version Nov. 27, 2025
 * @author  ASAMI, Tomoharu
 */
object SimpleModelOntology extends OntologyModel {
  override val prefix: String = Vocabulary.SimpleModel.prefix
  override val namespace: String = Vocabulary.SimpleModel.namespace

  // ------------------------------------------------------------------
  // Core Classes
  // ------------------------------------------------------------------
  val Model       = uri("Model")
  val Entity      = uri("Entity")
  val Value       = uri("Value")
  val Rule        = uri("Rule")
  val Service     = uri("Service")
  val Event       = uri("Event")
  val Attribute   = uri("Attribute")
  val Relation    = uri("Relation")
  val Component   = uri("Component")
  val Subsystem   = uri("Subsystem")
  val StateMachine= uri("StateMachine")

  // ------------------------------------------------------------------
  // Literal Properties (basic metadata)
  // ------------------------------------------------------------------
  val name        = uri("name")
  val title       = uri("title")
  val description = uri("description")
  val dataType    = uri("dataType")
  val expression  = uri("expression")

  // ------------------------------------------------------------------
  // Structural Relations
  // ------------------------------------------------------------------
  val hasAttribute   = uri("hasAttribute")    // Entity → Attribute
  val hasRelation    = uri("hasRelation")     // Entity → Relation
  val hasRule        = uri("hasRule")         // Model → Rule
  val hasService     = uri("hasService")      // Model → Service
  val hasEvent       = uri("hasEvent")        // Model → Event
  val hasComponent   = uri("hasComponent")    // Subsystem → Component
  val hasSubsystem   = uri("hasSubsystem")    // Model → Subsystem
  val hasStateMachine= uri("hasStateMachine") // Entity → StateMachine

  // ------------------------------------------------------------------
  // Semantic/Functional Relations
  // ------------------------------------------------------------------
  val relation     = uri("relation")          // generic relation predicate
  val attribute    = uri("attribute")         // generic attribute predicate
  val targetEntity = uri("targetEntity")      // relation → target entity
  val input        = uri("input")             // service → input value(s)
  val output       = uri("output")            // service → output value(s)
  val triggeredBy  = uri("triggeredBy")       // event → cause

  // ------------------------------------------------------------------
  // Integration
  // ------------------------------------------------------------------
  val belongsToModel = uri("belongsToModel")  // element → model
  val partOfComponent= uri("partOfComponent") // entity/value → component
  val partOfSubsystem= uri("partOfSubsystem") // component → subsystem

  //
  val tag          = uri("tag")

  // ------------------------------------------------------------------
  // JSON-LD Context
  // ------------------------------------------------------------------
  override lazy val jsonldContext: Map[String, Any] = Map(
    prefix -> namespace,
    // Classes
    "Model" -> Model,
    "Entity" -> Entity,
    "Value" -> Value,
    "Rule" -> Rule,
    "Service" -> Service,
    "Event" -> Event,
    "Attribute" -> Attribute,
    "Relation" -> Relation,
    "Component" -> Component,
    "Subsystem" -> Subsystem,
    "StateMachine" -> StateMachine,
    // Literals
    "name" -> name,
    "title" -> title,
    "description" -> description,
    "dataType" -> dataType,
    "expression" -> expression,
    // Relations
    "hasAttribute" -> hasAttribute,
    "hasRelation" -> hasRelation,
    "hasRule" -> hasRule,
    "hasService" -> hasService,
    "hasEvent" -> hasEvent,
    "hasComponent" -> hasComponent,
    "hasSubsystem" -> hasSubsystem,
    "hasStateMachine" -> hasStateMachine,
    // Functional Links
    "relation" -> relation,
    "attribute" -> attribute,
    "targetEntity" -> targetEntity,
    "input" -> input,
    "output" -> output,
    "triggeredBy" -> triggeredBy,
    // Integration
    "belongsToModel" -> belongsToModel,
    "partOfComponent" -> partOfComponent,
    "partOfSubsystem" -> partOfSubsystem,
  )

  object node {
    val Entity       = Node.Uri(SimpleModelOntology.Entity)
    val Value        = Node.Uri(SimpleModelOntology.Value)
    val Rule         = Node.Uri(SimpleModelOntology.Rule)
    val Service      = Node.Uri(SimpleModelOntology.Service)
    val Event        = Node.Uri(SimpleModelOntology.Event)
    val Component    = Node.Uri(SimpleModelOntology.Component)
    val Subsystem    = Node.Uri(SimpleModelOntology.Subsystem)
    val StateMachine = Node.Uri(SimpleModelOntology.StateMachine)
    val Attribute    = Node.Uri(SimpleModelOntology.Attribute)
    val Relation     = Node.Uri(SimpleModelOntology.Relation)
    val tag          = Node.Uri(SimpleModelOntology.tag)
  }

  override def toTriples: Seq[Triple] = Seq.empty
}
