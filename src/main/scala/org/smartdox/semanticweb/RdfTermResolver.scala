package org.smartdox.semanticweb

import java.net.URI
import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import com.typesafe.config.{Config => Hocon}
import org.goldenport.parser.ParseLocation
import org.smartdox._

/*
 * @since   Aug. 18, 2026
 * @version Aug. 19, 2026
 * @author  ASAMI, Tomoharu
 */
object RdfTermResolver {
  sealed trait TermForm {
    def value: String
  }
  object TermForm {
    case object Canonical extends TermForm {
      val value = "canonical"
    }
    case object Short extends TermForm {
      val value = "short"
    }
    case object Bilingual extends TermForm {
      val value = "bilingual"
    }
    case object Verbatim extends TermForm {
      val value = "verbatim"
    }

    val values: Vector[TermForm] = Vector(Canonical, Short, Bilingual, Verbatim)

    def parse(p: String): Option[TermForm] = values.find(_.value == p)
  }

  case class KnownConcept(
    iri: String,
    canonical: String,
    short: Option[String] = None,
    bilingual: Option[String] = None
  ) {
    def mergeSurfaceForms(p: KnownConcept): KnownConcept = copy(
      short = short orElse p.short,
      bilingual = bilingual orElse p.bilingual
    )
  }

  case class ResolvedDefinition(
    concept: KnownConcept,
    location: Option[ParseLocation]
  )

  case class ResolvedReference(
    concept: KnownConcept,
    form: TermForm,
    visibleText: String,
    location: Option[ParseLocation]
  )

  sealed trait DiagnosticCode {
    def value: String
  }
  object DiagnosticCode {
    case object MissingPrefix extends DiagnosticCode {
      val value = "missing-prefix"
    }
    case object UnknownPrefix extends DiagnosticCode {
      val value = "unknown-prefix"
    }
    case object RelativeOrMalformedIri extends DiagnosticCode {
      val value = "relative-or-malformed-iri"
    }
    case object MissingTermReference extends DiagnosticCode {
      val value = "missing-term-reference"
    }
    case object EmptyVisibleText extends DiagnosticCode {
      val value = "empty-visible-text"
    }
    case object UnsupportedForm extends DiagnosticCode {
      val value = "unsupported-form"
    }
    case object UnresolvedExplicitReference extends DiagnosticCode {
      val value = "unresolved-explicit-reference"
    }
    case object DuplicateConceptIdentity extends DiagnosticCode {
      val value = "duplicate-concept-identity"
    }
    case object ConflictingCanonicalLabel extends DiagnosticCode {
      val value = "conflicting-canonical-label"
    }
    case object IncompatibleVisibleForm extends DiagnosticCode {
      val value = "incompatible-visible-form"
    }
    case object NoTermNesting extends DiagnosticCode {
      val value = "noterm-nesting"
    }
  }

  case class Diagnostic(
    code: DiagnosticCode,
    message: String,
    location: Option[ParseLocation]
  )

  case class Result(
    namespaces: Map[String, String],
    concepts: Vector[KnownConcept],
    definitions: Vector[ResolvedDefinition],
    references: Vector[ResolvedReference],
    diagnostics: Vector[Diagnostic]
  )

  def apply(
    document: Document,
    knownConcepts: Seq[KnownConcept] = Vector.empty
  ): Result = resolve(document, knownConcepts)

  def resolve(
    document: Document,
    knownConcepts: Seq[KnownConcept] = Vector.empty
  ): Result = {
    val namespaces = _term_namespaces(document)
    val sources = _collect(document.body, false)
    val candidates = _definition_candidates(sources.definitions, namespaces)
    val initial = _initial_catalog(knownConcepts.toVector)
    val definitions = _resolve_definitions(initial, candidates.candidates)
    val references = _resolve_references(
      sources.references,
      namespaces,
      definitions.catalog
    )
    Result(
      namespaces,
      definitions.catalog.concepts.values.toVector.sortBy(_.iri),
      definitions.definitions,
      references.references,
      sources.diagnostics ++
        candidates.diagnostics ++
        initial.diagnostics ++
        definitions.diagnostics ++
        references.diagnostics
    )
  }

  private case class Sources(
    definitions: Vector[Dfn] = Vector.empty,
    references: Vector[Term] = Vector.empty,
    diagnostics: Vector[Diagnostic] = Vector.empty
  ) {
    def +(rhs: Sources): Sources = Sources(
      definitions ++ rhs.definitions,
      references ++ rhs.references,
      diagnostics ++ rhs.diagnostics
    )
  }

  private case class DefinitionCandidate(
    concept: KnownConcept,
    location: Option[ParseLocation]
  )

  private case class DefinitionCandidates(
    candidates: Vector[DefinitionCandidate] = Vector.empty,
    diagnostics: Vector[Diagnostic] = Vector.empty
  )

  private case class Catalog(
    concepts: Map[String, KnownConcept] = Map.empty,
    canonicallabels: Map[String, String] = Map.empty,
    diagnostics: Vector[Diagnostic] = Vector.empty
  )

  private case class Definitions(
    catalog: Catalog,
    definitions: Vector[ResolvedDefinition] = Vector.empty,
    identities: Map[String, String] = Map.empty,
    diagnostics: Vector[Diagnostic] = Vector.empty
  )

  private case class References(
    references: Vector[ResolvedReference] = Vector.empty,
    diagnostics: Vector[Diagnostic] = Vector.empty
  )

  private val _prefix_pattern = "[A-Za-z][A-Za-z0-9._-]*".r

  private def _term_namespaces(document: Document): Map[String, String] =
    document.head.metadata.properties.map(_term_namespaces).getOrElse(Map.empty)

  private def _term_namespaces(config: Hocon): Map[String, String] =
    try {
      if (config.hasPath("term_namespaces")) {
        val namespaces = config.getConfig("term_namespaces")
        namespaces.entrySet.asScala.toVector.flatMap { entry =>
          try {
            Some(entry.getKey -> namespaces.getString(entry.getKey))
          } catch {
            case NonFatal(_) => None
          }
        }.toMap
      } else {
        Map.empty
      }
    } catch {
      case NonFatal(_) => Map.empty
    }

  private def _collect(p: Dox, inNoTerm: Boolean): Sources = p match {
    case m: NoTerm =>
      val own = if (inNoTerm)
        Sources(diagnostics = Vector(_diagnostic(DiagnosticCode.NoTermNesting, m.location, "noterm")))
      else
        Sources()
      own + _collect_children(m.contents, true)
    case m: Dfn =>
      val own = if (inNoTerm)
        Sources(diagnostics = Vector(_diagnostic(DiagnosticCode.NoTermNesting, m.location, "dfn")))
      else
        Sources(definitions = Vector(m))
      own + _collect_children(m.contents, inNoTerm)
    case m: Term =>
      val own = if (inNoTerm)
        Sources(diagnostics = Vector(_diagnostic(DiagnosticCode.NoTermNesting, m.location, "term")))
      else
        Sources(references = Vector(m))
      own + _collect_children(m.contents, inNoTerm)
    case m => _collect_children(m.elements, inNoTerm)
  }

  private def _collect_children(ps: Seq[Dox], inNoTerm: Boolean): Sources =
    ps.foldLeft(Sources()) { (z, x) => z + _collect(x, inNoTerm) }

  private def _definition_candidates(
    definitions: Vector[Dfn],
    namespaces: Map[String, String]
  ): DefinitionCandidates =
    definitions.foldLeft(DefinitionCandidates()) { (z, definition) =>
      definition.attribute("about") match {
        case None => z
        case Some(reference) =>
          val canonical = _visible_text(definition.contents)
          val visiblediagnostics =
            if (canonical.trim.isEmpty)
              Vector(_diagnostic(DiagnosticCode.EmptyVisibleText, definition.location, "dfn"))
            else
              Vector.empty
          _expand(reference, namespaces, definition.location) match {
            case Left(diagnostic) => z.copy(diagnostics = z.diagnostics ++ visiblediagnostics :+ diagnostic)
            case Right(iri) if visiblediagnostics.nonEmpty =>
              z.copy(diagnostics = z.diagnostics ++ visiblediagnostics)
            case Right(iri) => z.copy(
              candidates = z.candidates :+ DefinitionCandidate(
                KnownConcept(iri, canonical),
                definition.location
              ),
              diagnostics = z.diagnostics ++ visiblediagnostics
            )
          }
      }
    }

  private def _initial_catalog(knownconcepts: Vector[KnownConcept]): Catalog =
    knownconcepts.foldLeft(Catalog()) { (z, concept) =>
      z.concepts.get(concept.iri) match {
        case Some(existing) if existing.canonical == concept.canonical =>
          z.copy(concepts = z.concepts.updated(concept.iri, existing.mergeSurfaceForms(concept)))
        case Some(existing) => z.copy(
          diagnostics = z.diagnostics :+
            _diagnostic(DiagnosticCode.ConflictingCanonicalLabel, None, concept.canonical)
        )
        case None => z.canonicallabels.get(concept.canonical) match {
          case Some(iri) if iri != concept.iri => z.copy(
            diagnostics = z.diagnostics :+
              _diagnostic(DiagnosticCode.ConflictingCanonicalLabel, None, concept.canonical)
          )
          case _ => z.copy(
            concepts = z.concepts.updated(concept.iri, concept),
            canonicallabels = z.canonicallabels.updated(concept.canonical, concept.iri)
          )
        }
      }
    }

  private def _resolve_definitions(
    initial: Catalog,
    candidates: Vector[DefinitionCandidate]
  ): Definitions =
    candidates.foldLeft(Definitions(initial)) { (z, candidate) =>
      val concept = candidate.concept
      z.identities.get(concept.iri) match {
        case Some(canonical) =>
          val conflict = if (canonical == concept.canonical)
            Vector.empty
          else
            Vector(_diagnostic(DiagnosticCode.ConflictingCanonicalLabel, candidate.location, concept.canonical))
          z.copy(diagnostics = z.diagnostics ++
            Vector(_diagnostic(DiagnosticCode.DuplicateConceptIdentity, candidate.location, concept.iri)) ++
            conflict)
        case None => z.catalog.concepts.get(concept.iri) match {
          case Some(known) if known.canonical == concept.canonical => z.copy(
            definitions = z.definitions :+ ResolvedDefinition(known, candidate.location),
            identities = z.identities.updated(concept.iri, concept.canonical)
          )
          case Some(known) => z.copy(
            identities = z.identities.updated(concept.iri, concept.canonical),
            diagnostics = z.diagnostics :+
              _diagnostic(DiagnosticCode.ConflictingCanonicalLabel, candidate.location, concept.canonical)
          )
          case None => z.catalog.canonicallabels.get(concept.canonical) match {
            case Some(iri) if iri != concept.iri => z.copy(
              identities = z.identities.updated(concept.iri, concept.canonical),
              diagnostics = z.diagnostics :+
                _diagnostic(DiagnosticCode.ConflictingCanonicalLabel, candidate.location, concept.canonical)
            )
            case _ =>
              val catalog = z.catalog.copy(
                concepts = z.catalog.concepts.updated(concept.iri, concept),
                canonicallabels = z.catalog.canonicallabels.updated(concept.canonical, concept.iri)
              )
              z.copy(
                catalog = catalog,
                definitions = z.definitions :+ ResolvedDefinition(concept, candidate.location),
                identities = z.identities.updated(concept.iri, concept.canonical)
              )
          }
        }
      }
    }

  private def _resolve_references(
    references: Vector[Term],
    namespaces: Map[String, String],
    catalog: Catalog
  ): References =
    references.foldLeft(References()) { (z, term) =>
      val text = _visible_text(term.contents)
      val visiblediagnostics =
        if (text.trim.isEmpty)
          Vector(_diagnostic(DiagnosticCode.EmptyVisibleText, term.location, "term"))
        else
          Vector.empty
      val form = term.attribute("form") match {
        case Some(value) => TermForm.parse(value).toRight(
          _diagnostic(DiagnosticCode.UnsupportedForm, term.location, value)
        )
        case None => Right(TermForm.Canonical)
      }
      term.attribute("ref").filterNot(_.trim.isEmpty) match {
        case None => z.copy(diagnostics = z.diagnostics ++ visiblediagnostics ++ form.left.toOption.toVector :+
          _diagnostic(DiagnosticCode.MissingTermReference, term.location, "term"))
        case Some(reference) => _expand(reference, namespaces, term.location) match {
          case Left(diagnostic) => z.copy(
            diagnostics = z.diagnostics ++ visiblediagnostics ++ form.left.toOption.toVector :+ diagnostic
          )
          case Right(iri) => catalog.concepts.get(iri) match {
            case None => z.copy(
              diagnostics = z.diagnostics ++ visiblediagnostics ++ form.left.toOption.toVector :+
                _diagnostic(DiagnosticCode.UnresolvedExplicitReference, term.location, iri)
            )
            case Some(concept) => form match {
              case Left(diagnostic) => z.copy(diagnostics = z.diagnostics ++ visiblediagnostics :+ diagnostic)
              case Right(value) if visiblediagnostics.nonEmpty => z.copy(
                diagnostics = z.diagnostics ++ visiblediagnostics
              )
              case Right(value) if _is_compatible(value, text, concept) => z.copy(
                references = z.references :+
                  ResolvedReference(concept, value, text, term.location)
              )
              case Right(value) => z.copy(diagnostics = z.diagnostics :+
                _diagnostic(DiagnosticCode.IncompatibleVisibleForm, term.location, value.value)
              )
            }
          }
        }
      }
    }

  private def _expand(
    reference: String,
    namespaces: Map[String, String],
    location: Option[ParseLocation]
  ): Either[Diagnostic, String] = {
    if (reference.trim.isEmpty) {
      Left(_diagnostic(DiagnosticCode.RelativeOrMalformedIri, location, reference))
    } else if (_is_http_candidate(reference)) {
      if (_is_absolute_http_iri(reference))
        Right(reference)
      else
        Left(_diagnostic(DiagnosticCode.RelativeOrMalformedIri, location, reference))
    } else {
      val index = reference.indexOf(':')
      if (index == 0) {
        Left(_diagnostic(DiagnosticCode.MissingPrefix, location, reference))
      } else if (index > 0) {
        val prefix = reference.substring(0, index)
        val local = reference.substring(index + 1)
        if (!_is_prefix(prefix) || local.isEmpty) {
          Left(_diagnostic(DiagnosticCode.RelativeOrMalformedIri, location, reference))
        } else {
          namespaces.get(prefix) match {
            case Some(namespace) =>
              val iri = namespace + local
              if (_is_absolute_http_iri(iri))
                Right(iri)
              else
                Left(_diagnostic(DiagnosticCode.RelativeOrMalformedIri, location, reference))
            case None =>
              Left(_diagnostic(DiagnosticCode.UnknownPrefix, location, prefix))
          }
        }
      } else {
        Left(_diagnostic(DiagnosticCode.RelativeOrMalformedIri, location, reference))
      }
    }
  }

  private def _is_prefix(p: String): Boolean = _prefix_pattern.pattern.matcher(p).matches

  private def _is_http_candidate(p: String): Boolean =
    p.regionMatches(true, 0, "http:", 0, 5) ||
      p.regionMatches(true, 0, "https:", 0, 6)

  private def _is_absolute_http_iri(p: String): Boolean =
    try {
      val uri = new URI(p)
      uri.isAbsolute &&
        uri.getHost != null &&
        Option(uri.getScheme).exists(x => x.equalsIgnoreCase("http") || x.equalsIgnoreCase("https"))
    } catch {
      case NonFatal(_) => false
    }

  private def _is_compatible(
    form: TermForm,
    visibletext: String,
    concept: KnownConcept
  ): Boolean = form match {
    case TermForm.Canonical => visibletext == concept.canonical
    case TermForm.Short => concept.short.contains(visibletext)
    case TermForm.Bilingual => concept.bilingual.contains(visibletext)
    case TermForm.Verbatim => true
  }

  private def _visible_text(ps: Seq[Dox]): String = ps.map(_.toText).mkString

  private def _diagnostic(
    code: DiagnosticCode,
    location: Option[ParseLocation],
    detail: String
  ): Diagnostic = Diagnostic(code, s"${code.value}: $detail", location)
}
