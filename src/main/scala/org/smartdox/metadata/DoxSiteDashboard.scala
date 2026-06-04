package org.smartdox.metadata

import org.joda.time.LocalDate
import org.joda.time.Days
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import org.smartdox.metadata.History.{ContentKind, EventKind}

/*
 * @since   Jun.  4, 2026
 * @version Jun.  5, 2026
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
    increments: Increments
  )
  object CategoryDashboard {
    implicit val categoryDashboardEncoder: Encoder.AsObject[CategoryDashboard] = deriveConfiguredEncoder
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
      CategoryDashboard(key, category.effectiveTitle, counts, _increments(meta.history, Some(key)))
    }
    DoxSiteDashboard(
      _counts(categories.size, articles.size, glossary.size),
      _rdf_summary(meta),
      _increments(meta.history, None),
      categorydashboards
    )
  }

  def toJsonString(p: DoxSiteDashboard): String =
    p.asJson.spaces2 + "\n"

  private def _counts(categorycount: Int, articlecount: Int, glossarytermcount: Int): Counts =
    Counts(categorycount, articlecount, glossarytermcount, articlecount + glossarytermcount)

  private def _rdf_summary(meta: MetaData): RdfSummary =
    if (meta.site.metadata == null)
      RdfSummary.empty
    else {
      val graph = meta.site.toGraph
      val triples = graph.triples
      RdfSummary(
        meta.site.resources.size,
        triples.size,
        triples.map(_.subject).distinct.size,
        triples.map(_.predicate).distinct.size
      )
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
