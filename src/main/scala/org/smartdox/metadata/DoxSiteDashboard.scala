package org.smartdox.metadata

import org.joda.time.LocalDate
import org.joda.time.Days
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import org.smartdox.metadata.History.{ContentKind, EventKind}
import org.goldenport.context.Consequence
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.transformers.Dox2HtmlTransformer
import org.smartdox.semanticweb.Rdf
import scala.collection.JavaConverters._

/*
 * @since   Jun.  4, 2026
 * @version Jun. 29, 2026
 * @author  ASAMI, Tomoharu
 */
case class DoxSiteDashboard(
  counts: DoxSiteDashboard.Counts = DoxSiteDashboard.Counts.empty,
  rdf: DoxSiteDashboard.RdfSummary = DoxSiteDashboard.RdfSummary.empty,
  increments: DoxSiteDashboard.Increments = DoxSiteDashboard.Increments.empty,
  categories: Vector[DoxSiteDashboard.CategoryDashboard] = Vector.empty
)

object DoxSiteDashboard {
  implicit val circeconf: Configuration = Configuration.default.withDefaults.withSnakeCaseMemberNames

  val empty: DoxSiteDashboard = DoxSiteDashboard()

  case class Counts(
    categoryCount: Int = 0,
    articleCount: Int = 0,
    glossaryTermCount: Int = 0,
    totalItemCount: Int = 0
  )
  object Counts {
    val empty: Counts = Counts()
    implicit val countsEncoder: Encoder.AsObject[Counts] = deriveConfiguredEncoder
  }

  case class RdfSummary(
    resourceCount: Int = 0,
    tripleCount: Int = 0,
    subjectCount: Int = 0,
    predicateCount: Int = 0
  )
  object RdfSummary {
    val empty: RdfSummary = RdfSummary()
    implicit val rdfSummaryEncoder: Encoder.AsObject[RdfSummary] = deriveConfiguredEncoder
  }

  case class Increments(
    scale: String = "day",
    buckets: Vector[Bucket] = Vector.empty
  )
  object Increments {
    val empty: Increments = Increments()
    implicit val incrementsEncoder: Encoder.AsObject[Increments] = deriveConfiguredEncoder
  }

  case class Bucket(
    label: String,
    startDate: String,
    endDate: String,
    count: Int,
    articleCount: Int = 0,
    glossaryTermCount: Int = 0
  )
  object Bucket {
    implicit val bucketEncoder: Encoder.AsObject[Bucket] = deriveConfiguredEncoder
  }

  case class CategoryDashboard(
    name: String,
    title: String,
    counts: Counts,
    increments: Increments,
    rdf: RdfSummary = RdfSummary.empty
  )
  object CategoryDashboard {
    implicit val categoryDashboardEncoder: Encoder.AsObject[CategoryDashboard] = deriveConfiguredEncoder
  }

  case class RdfGraph(
    nodes: Vector[RdfGraphNode] = Vector.empty,
    edges: Vector[RdfGraphEdge] = Vector.empty,
    truncated: Boolean = false
  )
  object RdfGraph {
    val empty: RdfGraph = RdfGraph()
    implicit val rdfGraphEncoder: Encoder.AsObject[RdfGraph] = deriveConfiguredEncoder
  }

  case class RdfGraphNode(
    id: String,
    label: String,
    nodeType: String,
    category: Option[String],
    degree: Int,
    terms: Vector[String] = Vector.empty
  )
  object RdfGraphNode {
    implicit val rdfGraphNodeEncoder: Encoder.AsObject[RdfGraphNode] = deriveConfiguredEncoder
  }

  case class RdfGraphEdge(
    source: String,
    target: String,
    predicate: String,
    label: String,
    category: Option[String],
    terms: Vector[String] = Vector.empty
  )
  object RdfGraphEdge {
    implicit val rdfGraphEdgeEncoder: Encoder.AsObject[RdfGraphEdge] = deriveConfiguredEncoder
  }

  case class TermIndex(
    terms: Vector[TermEntry] = Vector.empty
  )
  object TermIndex {
    val empty: TermIndex = TermIndex()
    implicit val termIndexEncoder: Encoder.AsObject[TermIndex] = deriveConfiguredEncoder
  }

  case class TermEntry(
    id: String,
    slug: String,
    title: String,
    reading: Option[String],
    category: Option[String],
    sourcePath: String,
    publicPath: String,
    definitionHtml: String,
    summary: Option[String] = None,
    aliases: Vector[String] = Vector.empty,
    articleRefs: Vector[TermReference] = Vector.empty,
    termRefs: Vector[TermReference] = Vector.empty,
    rdfRefs: Vector[TermRdfReference] = Vector.empty,
    videoRefs: Vector[TermReference] = Vector.empty,
    termType: String = "concept",
    event: Option[TermEvent] = None,
    actor: Option[TermActor] = None,
    role: Option[TermRole] = None,
    quality: TermQuality = TermQuality.empty,
    tags: Vector[String] = Vector.empty
  )
  object TermEntry {
    implicit val termEntryEncoder: Encoder.AsObject[TermEntry] = deriveConfiguredEncoder
  }


  case class TermEvent(
    occurredAt: Option[String] = None,
    startAt: Option[String] = None,
    endAt: Option[String] = None,
    location: Option[String] = None,
    actors: Vector[String] = Vector.empty,
    roles: Vector[String] = Vector.empty,
    participants: Vector[String] = Vector.empty,
    scenarios: Vector[String] = Vector.empty,
    evidence: Vector[String] = Vector.empty,
    cmlEvent: Option[String] = None,
    cmlComponent: Option[String] = None,
    cmlStatemachine: Option[String] = None
  )
  object TermEvent {
    implicit val termEventEncoder: Encoder.AsObject[TermEvent] = deriveConfiguredEncoder
  }

  case class TermActor(
    roles: Vector[String] = Vector.empty,
    organization: Option[String] = None,
    description: Option[String] = None
  )
  object TermActor {
    implicit val termActorEncoder: Encoder.AsObject[TermActor] = deriveConfiguredEncoder
  }

  case class TermRole(
    actors: Vector[String] = Vector.empty,
    responsibilities: Vector[String] = Vector.empty,
    permissions: Vector[String] = Vector.empty
  )
  object TermRole {
    implicit val termRoleEncoder: Encoder.AsObject[TermRole] = deriveConfiguredEncoder
  }

  case class TermReference(
    title: String,
    path: String,
    relation: String = "related"
  )
  object TermReference {
    implicit val termReferenceEncoder: Encoder.AsObject[TermReference] = deriveConfiguredEncoder
  }

  case class TermRdfReference(
    resource: String,
    label: String,
    predicate: Option[String] = None,
    direction: String = "node"
  )
  object TermRdfReference {
    implicit val termRdfReferenceEncoder: Encoder.AsObject[TermRdfReference] = deriveConfiguredEncoder
  }

  case class TermQuality(
    isolated: Boolean = false,
    unreferenced: Boolean = false,
    weaklyConnected: Boolean = false
  )
  object TermQuality {
    val empty: TermQuality = TermQuality()
    implicit val termQualityEncoder: Encoder.AsObject[TermQuality] = deriveConfiguredEncoder
  }

  implicit val dashboardEncoder: Encoder.AsObject[DoxSiteDashboard] = deriveConfiguredEncoder

  def create(meta: MetaData): DoxSiteDashboard = {
    val categories = meta.categories.categoryVector.filterNot(_is_special_category)
    val articles = meta.notices.notices.filter(x => _is_article(x.effectiveKind) && !_is_special_notice(x))
    val glossary = meta.glossary.definitions.collect { case m: Glossary.Definition.InGlossary => m }
    val categorydashboards = categories.map { category =>
      val key = category.containerString
      val categoryarticles = articles.count(_.category.exists(_.containerString == key))
      val categoryglossary = glossary.count(x => _category_from_glossary_page(x.page.toString).contains(key))
      val counts = _counts(0, categoryarticles, categoryglossary)
      CategoryDashboard(key, category.effectiveTitle, counts, _increments(meta.history, Some(key)), _rdf_summary(meta, Some(key)))
    }
    DoxSiteDashboard(
      _counts(categories.size, articles.size, glossary.size),
      _rdf_summary(meta, None),
      _increments(meta.history, None),
      categorydashboards
    )
  }

  def toJsonString(p: DoxSiteDashboard): String =
    p.asJson.spaces2 + "\n"

  def toRdfGraphJsonString(meta: MetaData): String =
    _rdf_graph(meta).asJson.spaces2 + "\n"

  def toGlossaryTermsJsonString(meta: MetaData): String =
    _term_index(meta).asJson.spaces2 + "\n"

  private case class RdfCategoryIndex(
    categories: Set[String],
    resourceCategories: Map[String, String],
    resourceTerms: Map[String, Vector[String]] = Map.empty
  ) {
    def categoryOf(node: Rdf.Node): Option[String] = node match {
      case Rdf.Node.Uri(value) => resourceCategories.get(value).orElse(_category_from_rdf_uri(value, categories))
      case _ => None
    }
    def termsOf(node: Rdf.Node): Vector[String] = node match {
      case Rdf.Node.Uri(value) => resourceTerms.getOrElse(value, Vector.empty)
      case _ => Vector.empty
    }
  }

  private case class TermReferenceIndex(
    articleRefs: Map[String, TermReference],
    termRefs: Map[String, TermReference],
    videoRefs: Map[String, TermReference]
  )

  private def _counts(categorycount: Int, articlecount: Int, glossarytermcount: Int): Counts =
    Counts(categorycount, articlecount, glossarytermcount, articlecount + glossarytermcount)

  private def _rdf_summary(meta: MetaData, category: Option[String]): RdfSummary =
    if (meta.site.metadata == null)
      RdfSummary.empty
    else {
      val graph = meta.site.toGraph
      val index = _rdf_category_index(meta)
      val triples = category.fold(graph.triples)(key => graph.triples.filter(_triple_in_category(_, key, index)))
      val resourcecount = category match {
        case Some(_) => triples.flatMap(t => Vector(t.subject, t.obj)).distinct.size
        case None => meta.site.resources.size
      }
      RdfSummary(
        resourcecount,
        triples.size,
        triples.map(_.subject).distinct.size,
        triples.map(_.predicate).distinct.size
      )
    }

  private def _rdf_graph(meta: MetaData): RdfGraph =
    if (meta.site.metadata == null)
      RdfGraph.empty
    else {
      val graph = meta.site.toGraph
      val index = _rdf_category_index(meta)
      val triples = graph.triples.sortBy(t => (_node_id(t.subject), t.predicate.value, _node_id(t.obj)))
      val limit = 500
      val selected = triples.take(limit)
      val degree = selected.foldLeft(Map.empty[String, Int]) { (z, t) =>
        val s = _node_id(t.subject)
        val o = _node_id(t.obj)
        z + (s -> (z.getOrElse(s, 0) + 1)) + (o -> (z.getOrElse(o, 0) + 1))
      }
      val nodes = selected.flatMap(t => Vector(t.subject, t.obj)).distinct.map { node =>
        val id = _node_id(node)
        RdfGraphNode(id, _node_label(node), _node_type(node), index.categoryOf(node), degree.getOrElse(id, 0), index.termsOf(node))
      }.sortBy(_.id)
      val edges = selected.map { t =>
        val category = index.categoryOf(t.subject).orElse(index.categoryOf(t.obj))
        RdfGraphEdge(_node_id(t.subject), _node_id(t.obj), t.predicate.value, _short_label(t.predicate.value), category, (index.termsOf(t.subject) ++ index.termsOf(t.obj)).distinct)
      }
      RdfGraph(nodes, edges, triples.size > limit)
    }

  private def _triple_in_category(t: Rdf.Triple, key: String, index: RdfCategoryIndex): Boolean =
    index.categoryOf(t.subject).contains(key) || index.categoryOf(t.obj).contains(key)

  private def _rdf_category_index(meta: MetaData): RdfCategoryIndex = {
    val categories = meta.categories.categoryVector.filterNot(_is_special_category).map(_.containerString).toSet
    val articles = meta.notices.notices.flatMap { notice =>
      notice.category.map(_.containerString).filter(categories.contains).filter(_ => _is_article(notice.effectiveKind)).map { key =>
        notice.toSiteResource.id -> key
      }
    }
    val glossaries = meta.glossary.definitions.collect {
      case m: Glossary.Definition.InGlossary
        if _category_from_glossary_page(m.page.toString).exists(categories.contains) =>
        m.toSiteResource.id -> _category_from_glossary_page(m.page.toString).get
    }
    val terms = glossaries.map { case (resource, category) => resource -> Vector(_term_id(category, _slug_from_resource(resource))) }.toMap
    RdfCategoryIndex(categories, (articles ++ glossaries).toMap, terms)
  }

  private def _term_index(meta: MetaData): TermIndex = {
    val referenceindex = _term_reference_index(meta)
    val entries = meta.glossary.definitions.collect { case m: Glossary.Definition.InGlossary => m }.map { term =>
      val path = term.page.toString.stripPrefix("/")
      val sourcepath = Option(term.node).map(_.pathname.stripPrefix("/")).getOrElse(path.stripSuffix(".html") + ".dox")
      val category = _category_from_glossary_page(path)
      val slug = _term_slug(path)
      val id = _term_id(category.getOrElse("glossary"), slug)
      val resource = term.toSiteResource.id
      val adjacent = _term_adjacent_uri_resources(meta, resource)
      val articleRefs = adjacent.flatMap(referenceindex.articleRefs.get).distinct.sortBy(_.path)
      val termRefs = adjacent.flatMap(referenceindex.termRefs.get).filterNot(_.path == path).distinct.sortBy(_.path)
      val videoRefs = adjacent.flatMap(x => referenceindex.videoRefs.get(x).orElse(_video_reference(x))).distinct.sortBy(_.path)
      val rdfrefs = _term_rdf_refs(meta, resource)
      val hasrefs = articleRefs.nonEmpty || termRefs.nonEmpty || videoRefs.nonEmpty || rdfrefs.nonEmpty
      TermEntry(
        id,
        slug,
        term.term.name.en,
        _reading(term),
        category,
        sourcepath,
        path,
        _definition_html(term.description),
        _term_summary(term),
        term.term.aliases.valueVector,
        articleRefs,
        termRefs,
        rdfrefs,
        videoRefs,
        _term_type(term.metadata),
        _term_event(term.metadata),
        _term_actor(term.metadata),
        _term_role(term.metadata),
        TermQuality(
          isolated = !hasrefs,
          unreferenced = !hasrefs,
          weaklyConnected = rdfrefs.size <= 1 && articleRefs.isEmpty && termRefs.isEmpty && videoRefs.isEmpty
        ),
        _term_tags(term.metadata)
      )
    }.sortBy(x => (x.category.getOrElse(""), x.slug))
    TermIndex(entries)
  }

  private def _term_reference_index(meta: MetaData): TermReferenceIndex = {
    val articleRefs = meta.notices.notices.filter(x => _is_article(x.effectiveKind) && !_is_special_notice(x)).map { notice =>
      notice.toSiteResource.id -> TermReference(notice.title.en, notice.uri.toString, "article")
    }.toMap
    val termRefs = meta.glossary.definitions.collect { case m: Glossary.Definition.InGlossary =>
      m.toSiteResource.id -> TermReference(m.term.name.en, m.page.toString.stripPrefix("/"), "term")
    }.toMap
    TermReferenceIndex(articleRefs, termRefs, Map.empty)
  }

  private def _term_adjacent_uri_resources(meta: MetaData, resource: String): Vector[String] =
    if (meta.site.metadata == null)
      Vector.empty
    else {
      val graph = meta.site.toGraph
      graph.triples.filter(t => _node_id(t.subject) == resource || _node_id(t.obj) == resource).flatMap { t =>
        Vector(t.subject, t.obj).collect {
          case u: Rdf.Node.Uri if _node_id(u) != resource => _node_id(u)
        }
      }.distinct.sorted
    }

  private def _video_reference(resource: String): Option[TermReference] =
    if (resource.contains("/repository/video/") || resource.endsWith(".mp4"))
      Some(TermReference(_short_label(resource), resource, "video"))
    else
      None

  private def _term_rdf_refs(meta: MetaData, resource: String): Vector[TermRdfReference] =
    if (meta.site.metadata == null)
      Vector.empty
    else {
      val graph = meta.site.toGraph
      graph.triples.filter(t => _node_id(t.subject) == resource || _node_id(t.obj) == resource).flatMap { t =>
        val refs = Vector(t.subject, t.obj).filterNot(_node_id(_) == resource).collect {
          case u: Rdf.Node.Uri => TermRdfReference(_node_id(u), _node_label(u), Some(t.predicate.value), if (_node_id(t.subject) == resource) "outgoing" else "incoming")
        }
        if (refs.isEmpty)
          Vector(TermRdfReference(resource, _short_label(resource), Some(t.predicate.value), "self"))
        else
          refs
      }.distinct.sortBy(x => (x.resource, x.predicate.getOrElse("")))
    }

  private def _definition_html(dox: Dox): String = {
    val rule = Dox2HtmlTransformer.Rule(isDocument = false, isDefaultCss = false)
    Consequence.from(Dox2HtmlTransformer(Context.create(), rule).transform(dox)).foldConclusion(_.message)
  }

  private def _reading(term: Glossary.Definition.InGlossary): Option[String] =
    term.term.name.localeMapWithoutC.collectFirst { case (locale, value) if locale.getLanguage == "ja" && value != term.term.name.en => value }

  private def _term_summary(term: Glossary.Definition.InGlossary): Option[String] =
    _metadata_string(term.metadata, "summary", Glossary.PROP_BRIEF, "description", Glossary.PROP_DEFINITION).
      orElse(term.term.effectiveSummary.map(_.en))


  private def _term_type(metadata: DocumentMetaData): String =
    _metadata_string(metadata, "term_type").map(_normalize_term_type).getOrElse("concept")

  private def _term_tags(metadata: DocumentMetaData): Vector[String] =
    _metadata_string_list(metadata, "tags", "tag").distinct

  private def _normalize_term_type(value: String): String = value.trim.toLowerCase.replace('_', '-') match {
    case "event" => "event"
    case "actor" => "actor"
    case "role" => "role"
    case "concept" => "concept"
    case _ => "concept"
  }

  private def _term_event(metadata: DocumentMetaData): Option[TermEvent] = {
    val event = TermEvent(
      _metadata_string(metadata, "event.occurred_at", "event.occurredAt"),
      _metadata_string(metadata, "event.start_at", "event.startAt"),
      _metadata_string(metadata, "event.end_at", "event.endAt"),
      _metadata_string(metadata, "event.location"),
      _metadata_string_list(metadata, "event.actors"),
      _metadata_string_list(metadata, "event.roles"),
      _metadata_string_list(metadata, "event.participants"),
      _metadata_string_list(metadata, "event.scenarios"),
      _metadata_string_list(metadata, "event.evidence"),
      _metadata_string(metadata, "event.cml.event"),
      _metadata_string(metadata, "event.cml.component"),
      _metadata_string(metadata, "event.cml.statemachine", "event.cml.stateMachine")
    )
    if (_term_type(metadata) == "event" || event != TermEvent()) Some(event) else None
  }

  private def _term_actor(metadata: DocumentMetaData): Option[TermActor] = {
    val actor = TermActor(
      _metadata_string_list(metadata, "actor.roles"),
      _metadata_string(metadata, "actor.organization"),
      _metadata_string(metadata, "actor.description")
    )
    if (_term_type(metadata) == "actor" || actor != TermActor()) Some(actor) else None
  }

  private def _term_role(metadata: DocumentMetaData): Option[TermRole] = {
    val role = TermRole(
      _metadata_string_list(metadata, "role.actors"),
      _metadata_string_list(metadata, "role.responsibilities"),
      _metadata_string_list(metadata, "role.permissions")
    )
    if (_term_type(metadata) == "role" || role != TermRole()) Some(role) else None
  }

  private def _metadata_string(metadata: DocumentMetaData, keys: String*): Option[String] =
    metadata.properties.flatMap { hocon =>
      keys.toStream.flatMap { key =>
        try {
          if (hocon.hasPath(key))
            Some(hocon.getString(key)).filter(_.nonEmpty)
          else
            None
        } catch {
          case scala.util.control.NonFatal(_) => None
        }
      }.headOption
    }

  private def _metadata_string_list(metadata: DocumentMetaData, keys: String*): Vector[String] =
    metadata.properties.toVector.flatMap { hocon =>
      keys.toStream.flatMap { key =>
        try {
          if (hocon.hasPath(key)) {
            val xs = hocon.getStringList(key).asScala.toVector.flatMap(_metadata_list_token)
            if (xs.nonEmpty) Some(xs) else None
          } else {
            None
          }
        } catch {
          case scala.util.control.NonFatal(_) =>
            try {
              Some(hocon.getString(key).split(',').toVector.flatMap(_metadata_list_token)).filter(_.nonEmpty)
            } catch {
              case scala.util.control.NonFatal(_) => None
            }
        }
      }.headOption
    }.flatten

  private def _metadata_list_token(value: String): Option[String] = {
    val cleaned = value.trim.stripPrefix("[").stripSuffix("]").trim.stripPrefix("\"").stripSuffix("\"").trim
    Option(cleaned).filter(_.nonEmpty)
  }

  private def _term_slug(path: String): String =
    path.split('/').filter(_.nonEmpty).lastOption.getOrElse(path).stripSuffix(".html")

  private def _slug_from_resource(resource: String): String =
    resource.split('/').filter(_.nonEmpty).lastOption.getOrElse(resource)

  private def _term_id(category: String, slug: String): String =
    s"${category}:${slug}"

  private def _category_from_rdf_uri(value: String, categories: Set[String]): Option[String] = {
    val path = value.stripPrefix("https://www.simplemodeling.org/").stripPrefix("/")
    val parts = path.split('/').filter(_.nonEmpty).toVector
    parts match {
      case Vector("glossary", category, _*) if categories.contains(category) => Some(category)
      case Vector(category, _*) if categories.contains(category) => Some(category)
      case _ => None
    }
  }

  private def _node_id(node: Rdf.Node): String = node match {
    case Rdf.Node.Uri(value) => value
    case Rdf.Node.Blank(value) => s"_:${value}"
    case Rdf.Node.Literal(value, datatype, lang) =>
      val suffix = Vector(datatype, lang).flatten.mkString("|")
      s"literal:${value}:${suffix}"
  }

  private def _node_label(node: Rdf.Node): String = node match {
    case Rdf.Node.Uri(value) => _short_label(value)
    case Rdf.Node.Blank(value) => value
    case Rdf.Node.Literal(value, _, _) => if (value.length > 80) value.take(77) + "..." else value
  }

  private def _node_type(node: Rdf.Node): String = node match {
    case Rdf.Node.Uri(_) => "uri"
    case Rdf.Node.Blank(_) => "blank"
    case Rdf.Node.Literal(_, _, _) => "literal"
  }

  private def _short_label(value: String): String = {
    val a = value.split('#').lastOption.getOrElse(value)
    a.split('/').filter(_.nonEmpty).lastOption.getOrElse(a)
  }

  private def _is_article(kind: DocumentMetaData.Kind): Boolean = kind match {
    case DocumentMetaData.Kind.Article => true
    case DocumentMetaData.Kind.Blog => true
    case _ => false
  }

  private def _is_special_category(category: Category): Boolean =
    _is_special_category_name(category.containerString)

  private def _is_special_notice(notice: Notices.Notice): Boolean =
    notice.category.exists(_is_special_category)

  private def _is_special_slot(slot: History.Slot): Boolean =
    slot.contentKind match {
      case ContentKind.Article => slot.category.exists(_is_special_category)
      case _ => false
    }

  private def _is_special_category_name(name: String): Boolean =
    Set("glossary", "history", "manual").contains(name.toLowerCase)

  private def _is_slot_in_category(slot: History.Slot, key: String): Boolean =
    slot.contentKind match {
      case ContentKind.Glossary =>
        _category_from_glossary_page(slot.notice.uri.toString).contains(key)
      case _ =>
        slot.category.exists(_.containerString == key)
    }

  private def _category_from_glossary_page(page: String): Option[String] = {
    val s = page.stripPrefix("/")
    val parts = s.split('/').filter(_.nonEmpty).toVector
    parts match {
      case Vector("glossary", category, _*) => Some(category)
      case _ => None
    }
  }

  private def _increments(history: History, category: Option[String]): Increments = {
    val slots = history.slots.filter { slot =>
      slot.eventKind == EventKind.Created &&
      _is_increment_content(slot) &&
      category.fold(!_is_special_slot(slot))(key => _is_slot_in_category(slot, key))
    }
    if (slots.isEmpty)
      Increments.empty
    else {
      val dates = slots.map(_.date)
      val sorted = dates.sortBy(_.toString)
      val start = sorted.head
      val end = sorted.last
      val scale = _scale(start, end)
      Increments(scale, _buckets(scale, start, end, slots))
    }
  }

  private def _is_increment_content(slot: History.Slot): Boolean =
    slot.contentKind match {
      case ContentKind.Article => _is_article(slot.notice.effectiveKind)
      case ContentKind.Glossary => true
      case _ => false
    }

  private def _scale(start: LocalDate, end: LocalDate): String = {
    val days = Days.daysBetween(start, end).getDays + 1
    if (days <= 45)
      "day"
    else if (days <= 180)
      "week"
    else
      "month"
  }

  private def _buckets(scale: String, start: LocalDate, end: LocalDate, slots: Vector[History.Slot]): Vector[Bucket] = {
    def loop(current: LocalDate, xs: Vector[Bucket]): Vector[Bucket] = {
      if (current.isAfter(end))
        xs
      else {
        val nextstart = scale match {
          case "day" => current.plusDays(1)
          case "week" => current.plusDays(7)
          case _ => current.plusMonths(1)
        }
        val bucketend0 = nextstart.minusDays(1)
        val bucketend = if (bucketend0.isAfter(end)) end else bucketend0
        val bucketslots = slots.filter { slot =>
          !slot.date.isBefore(current) && !slot.date.isAfter(bucketend)
        }
        val articlecount = bucketslots.count(_.contentKind == ContentKind.Article)
        val glossarytermcount = bucketslots.count(_.contentKind == ContentKind.Glossary)
        val count = articlecount + glossarytermcount
        val bucket = Bucket(
          _label(scale, current, bucketend),
          current.toString,
          bucketend.toString,
          count,
          articlecount,
          glossarytermcount
        )
        loop(nextstart, xs :+ bucket)
      }
    }
    loop(start, Vector.empty)
  }

  private def _label(scale: String, start: LocalDate, end: LocalDate): String = scale match {
    case "day" => start.toString
    case "week" => s"${start.toString}/${end.toString}"
    case _ => f"${start.getYear}-${start.getMonthOfYear}%02d"
  }
}
