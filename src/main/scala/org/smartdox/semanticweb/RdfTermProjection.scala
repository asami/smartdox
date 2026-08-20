package org.smartdox.semanticweb

import java.util.Locale
import org.goldenport.parser.ParseLocation
import org.smartdox.semanticweb.Rdf.{Graph, Node, Triple}
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * @since   Aug. 21, 2026
 * @version Aug. 21, 2026
 * @author  ASAMI, Tomoharu
 */
object RdfTermProjection {
  sealed trait OccurrenceKind {
    def value: String
  }
  object OccurrenceKind {
    case object Definition extends OccurrenceKind {
      val value = "definition"
    }
    case object Reference extends OccurrenceKind {
      val value = "reference"
    }
  }

  sealed trait ResolutionKind {
    def value: String
  }
  object ResolutionKind {
    case object Explicit extends ResolutionKind {
      val value = "explicit"
    }
  }

  case class Occurrence(
    id: String,
    conceptIri: String,
    surfaceForm: String,
    locale: String,
    kind: OccurrenceKind,
    resolutionKind: ResolutionKind,
    sourcePath: String,
    sourceLocation: ParseLocation
  )

  case class Projection(graph: Graph, occurrences: Vector[Occurrence]) {
    def toJsonLd: String = RdfRenderer.toJsonLD(
      graph,
      RdfRenderer.JsonLDProfile.SmartDox,
      Map(
        "dcterms" -> Vocabulary.Dcterms.namespace,
        "glossary" -> GlossaryOntology.namespace,
        "schema" -> Vocabulary.Schema.namespace
      )
    )
  }

  def project(
    result: RdfTermResolver.Result,
    sourceDocumentIri: String,
    publicGlossaryPageIri: String,
    sourcePath: String,
    locale: Locale,
    knownConcepts: Seq[RdfTermResolver.KnownConcept] = Vector.empty
  ): Projection = {
    require(sourceDocumentIri.trim.nonEmpty, "sourceDocumentIri is required")
    require(publicGlossaryPageIri.trim.nonEmpty, "publicGlossaryPageIri is required")
    require(sourcePath.trim.nonEmpty, "sourcePath is required")
    require(locale != null, "locale is required")

    val language = locale.toLanguageTag
    val concepts = (result.concepts ++ knownConcepts).groupBy(_.iri).values.map(_.head).toVector.sortBy(_.iri)
    val definitionoccurrences = result.definitions.zipWithIndex.flatMap { case (definition, index) =>
      definition.location.map { location =>
        Occurrence(
          _occurrence_id(sourceDocumentIri, OccurrenceKind.Definition, index + 1),
          definition.concept.iri,
          definition.concept.canonical,
          language,
          OccurrenceKind.Definition,
          ResolutionKind.Explicit,
          sourcePath,
          location
        )
      }
    }
    val referenceoccurrences = result.references.zipWithIndex.flatMap { case (reference, index) =>
      reference.location.map { location =>
        Occurrence(
          _occurrence_id(sourceDocumentIri, OccurrenceKind.Reference, index + 1),
          reference.concept.iri,
          reference.visibleText,
          language,
          OccurrenceKind.Reference,
          ResolutionKind.Explicit,
          sourcePath,
          location
        )
      }
    }
    val occurrences = (definitionoccurrences ++ referenceoccurrences).toVector
    val resources = _resource_triples(concepts, sourceDocumentIri, publicGlossaryPageIri, sourcePath)
    val occurrencetriples = occurrences.flatMap(_occurrence_triples(_, sourceDocumentIri))
    Projection(Graph((resources ++ occurrencetriples).toVector), occurrences)
  }

  private def _resource_triples(
    concepts: Vector[RdfTermResolver.KnownConcept],
    sourcedocumentiri: String,
    publicglossarypageiri: String,
    sourcepath: String
  ): Vector[Triple] = {
    val source = Node.Uri(sourcedocumentiri)
    val page = Node.Uri(publicglossarypageiri)
    val sourcetriples = Vector(
      Triple(source, RdfType, Node.Uri(SmartDoxOntology.sourceDocument)),
      Triple(source, Node.Uri(Vocabulary.Dcterms.identifier), Node.Literal(sourcepath))
    )
    val pagetriples = Vector(
      Triple(page, RdfType, Node.Uri(SmartDoxOntology.publicGlossaryPage)),
      Triple(page, RdfType, Node.Uri(Vocabulary.Schema.uri("WebPage")))
    )
    val concepttriples = concepts.flatMap { concept =>
      val node = Node.Uri(concept.iri)
      Vector(
        Triple(node, RdfType, Node.Uri(SmartDoxOntology.termConcept)),
        Triple(node, RdfType, Node.Uri(GlossaryOntology.Term)),
        Triple(node, Node.Uri(Vocabulary.Rdfs.label), Node.Literal(concept.canonical)),
        Triple(node, Node.Uri(SmartDoxOntology.hasPublicGlossaryPage), page)
      )
    }
    sourcetriples ++ pagetriples ++ concepttriples
  }

  private def _occurrence_triples(
    occurrence: Occurrence,
    sourcedocumentiri: String
  ): Vector[Triple] = {
    val node = Node.Uri(occurrence.id)
    val concept = Node.Uri(occurrence.conceptIri)
    val kindclass = occurrence.kind match {
      case OccurrenceKind.Definition => SmartDoxOntology.definitionOccurrence
      case OccurrenceKind.Reference => SmartDoxOntology.referenceOccurrence
    }
    val base = Vector(
      Triple(node, RdfType, Node.Uri(SmartDoxOntology.termOccurrence)),
      Triple(node, RdfType, Node.Uri(kindclass)),
      Triple(node, Node.Uri(SmartDoxOntology.denotesConcept), concept),
      Triple(node, Node.Uri(SmartDoxOntology.inSourceDocument), Node.Uri(sourcedocumentiri)),
      Triple(node, Node.Uri(SmartDoxOntology.surfaceForm), Node.Literal(occurrence.surfaceForm, lang = Some(occurrence.locale))),
      Triple(node, Node.Uri(SmartDoxOntology.occurrenceKind), Node.Literal(occurrence.kind.value)),
      Triple(node, Node.Uri(SmartDoxOntology.resolutionKind), Node.Literal(occurrence.resolutionKind.value)),
      Triple(node, Node.Uri(SmartDoxOntology.sourcePath), Node.Literal(occurrence.sourcePath)),
      Triple(node, Node.Uri(SmartDoxOntology.sourceLocation), Node.Literal(occurrence.sourceLocation.show)),
      Triple(node, Node.Uri(SmartDoxOntology.occurrenceId), Node.Literal(occurrence.id))
    )
    occurrence.kind match {
      case OccurrenceKind.Definition =>
        base :+ Triple(concept, Node.Uri(SmartDoxOntology.hasDefinitionOccurrence), node)
      case OccurrenceKind.Reference => base
    }
  }

  private def _occurrence_id(sourcedocumentiri: String, kind: OccurrenceKind, order: Int): String =
    s"$sourcedocumentiri#term-occurrence-${kind.value}-$order"
}
