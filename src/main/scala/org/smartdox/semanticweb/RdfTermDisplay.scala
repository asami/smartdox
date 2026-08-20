package org.smartdox.semanticweb

import java.net.URI
import java.util.IdentityHashMap
import java.util.Locale
import org.smartdox._

/*
 * @since   Aug. 20, 2026
 * @version Aug. 21, 2026
 * @author  ASAMI, Tomoharu
 */
object RdfTermDisplay {
  sealed trait LinkPolicy {
    def href: Option[String]
  }
  object LinkPolicy {
    case object NoLink extends LinkPolicy {
      val href = None
    }

    private final case class External(hrefvalue: String) extends LinkPolicy {
      val href = Some(hrefvalue)
    }

    def external(href: String): Option[LinkPolicy] =
      try {
        val uri = new URI(href)
        if (uri.isAbsolute &&
          Option(uri.getHost).exists(_.nonEmpty) &&
          Set("http", "https").contains(Option(uri.getScheme).map(_.toLowerCase(Locale.ROOT)).getOrElse("")))
          Some(External(uri.toString))
        else
          None
      } catch {
        case _: Exception => None
      }
  }

  case class Concept(
    iri: String,
    preferredLabels: Map[String, String],
    shortLabels: Map[String, String] = Map.empty,
    aliases: Map[String, Vector[String]] = Map.empty,
    abbreviations: Map[String, String] = Map.empty,
    scope: Option[String] = None,
    speechLabels: Map[String, String] = Map.empty,
    linkPolicy: LinkPolicy = LinkPolicy.NoLink
  ) {
    def knownConcept: RdfTermResolver.KnownConcept = {
      val canonical = _localized(preferredLabels, Locale.ENGLISH).orElse(preferredLabels.values.headOption).getOrElse(iri)
      RdfTermResolver.KnownConcept(
        iri,
        canonical,
        short = _localized(shortLabels, Locale.ENGLISH),
        bilingual = _bilingual_resolver(Locale.ENGLISH)
      )
    }

    def preferredLabel(locale: Locale): Option[String] = _localized(preferredLabels, locale)
    def shortLabel(locale: Locale): Option[String] = _localized(shortLabels, locale)
    def abbreviation(locale: Locale): Option[String] = _localized(abbreviations, locale)
    def speechLabel(locale: Locale): Option[String] = _localized(speechLabels, locale)

    def bilingualLabel(locale: Locale): Option[String] = _bilingual(locale)

    private def _bilingual(locale: Locale): Option[String] = {
      val primary = preferredLabel(locale)
      val secondary = _counterpart_label(locale)
      (primary, secondary) match {
        case (Some(a), Some(b)) if a != b => Some(s"${a}（${b}）")
        case (Some(a), _) => Some(a)
        case _ => None
      }
    }

    private def _bilingual_resolver(locale: Locale): Option[String] = {
      val primarylabel = preferredLabel(locale)
      val secondarylabel = _counterpart_label(locale)
      (primarylabel, secondarylabel) match {
        case (Some(a), Some(b)) if a != b => Some(s"${a}｜${b}")
        case (Some(a), _) => Some(a)
        case _ => None
      }
    }

    private def _counterpart_label(locale: Locale): Option[String] =
      if (locale.getLanguage == "ja")
        _localized(preferredLabels, Locale.ENGLISH)
      else
        _localized(preferredLabels, Locale.JAPANESE)
  }

  object Concept {
    def fallback(p: RdfTermResolver.KnownConcept): Concept =
      Concept(
        p.iri,
        Map("en" -> p.canonical),
        shortLabels = p.short.map(x => Map("en" -> x)).getOrElse(Map.empty),
        speechLabels = Map("en" -> p.canonical)
      )
  }

  case class Registry(entries: Map[String, Concept]) {
    def knownConcepts: Vector[RdfTermResolver.KnownConcept] = entries.values.toVector.sortBy(_.iri).map(_.knownConcept)
    def lookup(iri: String): Option[Concept] = entries.get(iri)
  }

  object Registry {
    val empty = Registry(entries = Map.empty)

    def apply(p: Concept, ps: Concept*): Registry = {
      val concepts = p +: ps.toVector
      Registry(concepts.map(x => x.iri -> x).toMap)
    }
  }

  case class Occurrence(
    iri: String,
    visibleText: String,
    speechText: String,
    locale: String,
    kind: String,
    form: String,
    scope: Option[String],
    href: Option[String]
  ) {
    def attributes: Map[String, String] =
      Map(
        "class" -> "smartdox-rdf-term",
        "data-rdf-term-iri" -> iri,
        "data-rdf-term-locale" -> locale,
        "data-rdf-term-kind" -> kind,
        "data-rdf-term-resolution" -> "explicit",
        "data-rdf-term-form" -> form,
        "aria-label" -> speechText
      ) ++ scope.map("data-rdf-term-scope" -> _)
  }

  final class Projection private (
    private val _references: IdentityHashMap[Term, Occurrence],
    private val _definitions: IdentityHashMap[Dfn, Occurrence],
    val references: Vector[Occurrence],
    val definitions: Vector[Occurrence]
  ) {
    def reference(source: Term): Option[Occurrence] = Option(_references.get(source))
    def definition(source: Dfn): Option[Occurrence] = Option(_definitions.get(source))
  }

  object Projection {
    private[semanticweb] def _create(
      referenceoccurrences: IdentityHashMap[Term, Occurrence],
      definitionoccurrences: IdentityHashMap[Dfn, Occurrence],
      references: Vector[Occurrence],
      definitions: Vector[Occurrence]
    ): Projection = new Projection(
      referenceoccurrences,
      definitionoccurrences,
      references,
      definitions
    )

    val empty = _create(
      new IdentityHashMap[Term, Occurrence](),
      new IdentityHashMap[Dfn, Occurrence](),
      Vector.empty,
      Vector.empty
    )
  }

  def project(document: Document, locale: Locale, registry: Registry): Projection = {
    val resolved = RdfTermResolver.resolve(document, registry.knownConcepts)
    val referenceoccurrences = new IdentityHashMap[Term, Occurrence]()
    val references = resolved.references.foldLeft((Vector.empty[Occurrence], Set.empty[String])) {
      case ((occurrences, canonicalseen), reference) =>
        val metadata = registry.lookup(reference.concept.iri).getOrElse(Concept.fallback(reference.concept))
        val occurrence = _reference_occurrence(reference, metadata, locale, canonicalseen.contains(reference.concept.iri))
        referenceoccurrences.put(reference.source, occurrence)
        val nextcanonicalseen = reference.form match {
          case RdfTermResolver.TermForm.Canonical => canonicalseen + reference.concept.iri
          case _ => canonicalseen
        }
        (occurrences :+ occurrence, nextcanonicalseen)
    }._1
    val definitionoccurrences = new IdentityHashMap[Dfn, Occurrence]()
    val definitions = resolved.definitions.map { definition =>
      val metadata = registry.lookup(definition.concept.iri).getOrElse(Concept.fallback(definition.concept))
      val occurrence = _definition_occurrence(definition, metadata, locale)
      definitionoccurrences.put(definition.source, occurrence)
      occurrence
    }
    Projection._create(referenceoccurrences, definitionoccurrences, references, definitions)
  }

  private def _reference_occurrence(
    reference: RdfTermResolver.ResolvedReference,
    metadata: Concept,
    locale: Locale,
    isseen: Boolean
  ): Occurrence = {
    val base = reference.form match {
      case RdfTermResolver.TermForm.Canonical => metadata.preferredLabel(locale).getOrElse(reference.visibleText)
      case RdfTermResolver.TermForm.Short => metadata.shortLabel(locale).getOrElse(reference.visibleText)
      case RdfTermResolver.TermForm.Bilingual => metadata.bilingualLabel(locale).getOrElse(reference.visibleText)
      case RdfTermResolver.TermForm.Verbatim => reference.visibleText
    }
    val visible =
      if (!isseen && reference.form == RdfTermResolver.TermForm.Canonical)
        _first_use(base, metadata, locale)
      else
        base
    val speech = metadata.speechLabel(locale).orElse(metadata.preferredLabel(locale)).getOrElse(reference.concept.canonical)
    Occurrence(reference.concept.iri, visible, speech, locale.toLanguageTag, "reference", reference.form.value, metadata.scope, metadata.linkPolicy.href)
  }

  private def _definition_occurrence(
    definition: RdfTermResolver.ResolvedDefinition,
    metadata: Concept,
    locale: Locale
  ): Occurrence = {
    val visible = metadata.preferredLabel(locale).getOrElse(definition.concept.canonical)
    val speech = metadata.speechLabel(locale).getOrElse(visible)
    Occurrence(definition.concept.iri, visible, speech, locale.toLanguageTag, "definition", "canonical", metadata.scope, None)
  }

  private def _first_use(label: String, metadata: Concept, locale: Locale): String = {
    val bilingual = metadata.bilingualLabel(locale).filter(_ != label).getOrElse(label)
    metadata.abbreviation(locale).filter(x => x != label && !bilingual.contains(x)).map { abbreviation =>
      s"${bilingual}（${abbreviation}）"
    }.getOrElse(bilingual)
  }

  private def _localized(values: Map[String, String], locale: Locale): Option[String] =
    values.get(locale.toLanguageTag).orElse(values.get(locale.getLanguage)).orElse(values.get("en"))
}
