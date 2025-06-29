package org.smartdox.metadata

import scala.util.Try
import java.net.URI
import org.joda.time.DateTime
import com.typesafe.config.{Config => Hocon}
import org.goldenport.context.Consequence
import org.goldenport.context.DateTimeContext
import org.goldenport.i18n.I18NString
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.value._
import org.goldenport.values.LocalDateOrDateTime
import org.goldenport.util.VectorUtils
import org.goldenport.util.AnyUtils
import org.goldenport.util.OptionUtils
import org.goldenport.util.OptionUtils.lastMonoid
import org.smartdox._
import org.smartdox.generator.Context

/*
 * @since   Apr. 29, 2025
 *  version Apr. 30, 2025
 * @version Jun. 26, 2025
 * @author  ASAMI, Tomoharu
 */
case class DocumentMetaData(
  title: Option[I18NFragment] = None,
  titleImage: Option[URI] = None,
  category: Option[String] = None,
  description: Option[I18NFragment] = None,
  author: Option[I18NFragment] = None,
  keywords: List[String] = Nil,
  datePublished: Option[LocalDateOrDateTime] = None,
  dateModified: Option[LocalDateOrDateTime] = None,
  kindOption: Option[DocumentMetaData.Kind] = None,
  statusOption: Option[DocumentMetaData.Status] = None
) {
  import DocumentMetaData._

  def isEmpty = title.isEmpty && description.isEmpty && author.isEmpty && keywords.isEmpty && datePublished.isEmpty && dateModified.isEmpty

  def toOption = if (isEmpty) None else Some(this)

  def kind: DocumentMetaData.Kind = kindOption getOrElse DocumentMetaData.Kind.Article

  def status: DocumentMetaData.Status = statusOption getOrElse {
    if (datePublished.nonEmpty)
      DocumentMetaData.Status.Published
    else
      DocumentMetaData.Status.InPreparation
  }

  def getTitleString: Option[String] = title.map(_.distillString)

  def titleString: String = getTitleString getOrElse ""

  def getTitleInclineContents: Option[InlineContents] = title.map(_.distillInlineContents)

  def getTitleI18NString: Option[I18NString] = title.map(_.toI18NString)

  def getDescriptionI18NString: Option[I18NString] = description.map(_.toI18NString)

  def withTitle(p: InlineContents) = copy(title = Some(I18NFragment.create(p)))

  def withDescription(p: String) = copy(description = Some(I18NFragment.create(p)))

  def complementTitleDate(
    ptitle: InlineContents,
    pdate: InlineContents
  )(implicit context: Context) = {
    val t = title orElse _to_title(ptitle)
    val d = _to_date(pdate)
    val (dp, dm) = (datePublished, dateModified) match {
      case (Some(p), Some(m)) => (Some(p), Some(m))
      case (Some(p), None) => (Some(p), d)
      case (None, Some(m)) => (d, Some(m))
      case (None, None) => (d, None)
    }
    copy(
      title = t,
      datePublished = dp,
      dateModified = dm
    )
  }

  private def _to_title(p: InlineContents) = p match {
    case Nil => None
    case xs => Some(I18NFragment.create(xs))
  }

  private def _to_text(p: InlineContents): Option[String] = p match {
    case Nil => None
    case xs => Some(Dox.toText(xs))
  }

  private def _to_date(
    p: InlineContents
  )(implicit context: Context): Option[LocalDateOrDateTime] =
    _to_text(p).flatMap(LocalDateOrDateTime.parse(_)(context.dateTimeContext).toOption)

  def +(rhs: DocumentMetaData): DocumentMetaData =
    DocumentMetaData(
      title orElse rhs.title,
      titleImage orElse rhs.titleImage,
      category orElse rhs.category,
      description orElse rhs.description,
      author orElse rhs.author,
      (keywords ::: rhs.keywords).distinct,
      datePublished orElse rhs.datePublished,
      dateModified orElse rhs.dateModified,
      lastMonoid(kindOption, rhs.kindOption),
      lastMonoid(statusOption, rhs.statusOption)
    )

  def toFlattenVector: Vector[(String, String)] =
    VectorUtils.buildTupleVector(
      PROP_TITLE -> getTitleString,
      PROP_TITLE_IMAGE -> titleImage.map(_.toString),
      PROP_CATEGORY -> category,
      PROP_DESCRIPTION -> description.map(_.print),
      PROP_AUTHOR -> author.map(_.print),
      PROP_KEYWORDS -> _keywords_string,
      PROP_DATE_PUBLISHED -> datePublished.map(_to_string),
      PROP_DATE_MODIFIED -> dateModified.map(_to_string),
      PROP_KIND -> Some(kind.name),
      PROP_STATUS -> Some(status.name)
    )

  private def _keywords_string: Option[String] =
    keywords match {
      case Nil => None
      case xs => Some(xs.mkString(","))
    }

  private def _to_string(p: LocalDateOrDateTime) = p.print
}

object DocumentMetaData {
  import io.circe._
  import io.circe.generic.extras._
  import io.circe.generic.extras.semiauto._

  implicit val circeconf = Configuration.default.
    withDefaults.withSnakeCaseMemberNames

  final val PROP_TITLE = "title"
  final val PROP_TITLE_IMAGE = "titleImage"
  final val PROP_CATEGORY = "category"
  final val PROP_DESCRIPTION = "description"
  final val PROP_AUTHOR = "author"
  final val PROP_KEYWORDS = "keywords"
  final val PROP_DATE_PUBLISHED = "datePublished"
  final val PROP_DATE_MODIFIED = "dateModified"
  final val PROP_KIND = "kind"
  final val PROP_STATUS = "status"

  val empty = DocumentMetaData()

  sealed trait Kind extends NamedValueInstance
  object Kind extends EnumerationClass[Kind] {
    trait Post { Kind => }

    val elements = Vector(Article, News, Blog, Glossary)

    case object Article extends Kind with Post {
      val name = "article"
    }
    case object News extends Kind with Post {
      val name = "news"
    }
    case object Blog extends Kind with Post{
      val name = "blog"
    }
    case object Glossary extends Kind {
      val name = "glossary"
    }

    implicit val kindDecoder: Decoder[Kind] = Decoder.decodeString.emap(_create)

    implicit val kindEncoder: Encoder[Kind] = Encoder.encodeString.contramap(_.name)

    private def _create(p: String): Either[String, Kind] =
      get(p).toRight(s"Unknown kind: $p")
  }

  sealed trait Status extends NamedValueInstance {
    def noticePriority: Int
  }
  object Status extends EnumerationClass[Status] {
    val elements = Vector(Published, WorkInProgress, Draft, InPreparation, Inactive, Test)

    case object Published extends Status {
      val name = "published"
      def noticePriority = 4
    }
    case object WorkInProgress extends Status {
      val name = "work-in-progress"
      def noticePriority = 2
    }
    case object Draft extends Status {
      val name = "draft"
      def noticePriority = 3
    }
    case object InPreparation extends Status {
      val name = "in-preparation"
      def noticePriority = 9
    }
    case object Inactive extends Status {
      val name = "inactive"
      def noticePriority = 99
    }
    case object Test extends Status {
      val name = "test"
      def noticePriority = 1
    }

    implicit val statusDecoder: Decoder[Status] = Decoder.decodeString.emap(_create)

    implicit val statusEncoder: Encoder[Status] = Encoder.encodeString.contramap(_.name)

    private def _create(p: String): Either[String, Status] =
      get(p).toRight(s"Unknown status: $p")

    def compareOption(lhs: Option[Status], rhs: Option[Status]): Option[Boolean] =
      if (lhs == rhs)
        None
      else
        OptionUtils.compareDescOption(lhs.map(_.noticePriority), rhs.map(_.noticePriority))

    def compareOption(lhs: Status, rhs: Status): Option[Boolean] =
      if (lhs.noticePriority == rhs.noticePriority)
        None
      else
        Some(lhs.noticePriority < rhs.noticePriority)
  }

  def create(hocon: Hocon)(implicit ctx: DateTimeContext): DocumentMetaData =
    createC(hocon).take

  def createC(hocon: Hocon)(implicit ctx: DateTimeContext): Consequence[DocumentMetaData] = {
    for {
      title <- hocon.cStringOption(PROP_TITLE)
      titleimage <- hocon.cUriOption(PROP_TITLE_IMAGE)
      desc <- hocon.cStringOption(PROP_DESCRIPTION)
      category <- hocon.cStringOption(PROP_CATEGORY)
      auth <- hocon.cStringOption(PROP_AUTHOR)
      keywords <- hocon.cEagerStringList(PROP_KEYWORDS)
      published <- hocon.cLocalDateOrDateTimeOption(PROP_DATE_PUBLISHED)
      modified <- hocon.cLocalDateOrDateTimeOption(PROP_DATE_MODIFIED)
      kind <- hocon.cValueOption(Kind, PROP_KIND)
      status <- hocon.cValueOption(Status, PROP_STATUS)
    } yield {
      val inlinetitle = title.map(x => I18NFragment.create(List(Text(x))))
      DocumentMetaData(
        inlinetitle,
        titleimage,
        desc,
        category.map(I18NFragment.create),
        auth.map(I18NFragment.create),
        keywords,
        published,
        modified,
        kind,
        status
      )
    }
  }

  def create(title: Inline): DocumentMetaData =
    DocumentMetaData(Some(I18NFragment.create(List(title))))

  def create(
    title: InlineContents,
    date: InlineContents
  )(implicit ctx: DateTimeContext): DocumentMetaData = {
    val d = LocalDateOrDateTime.parse(Dox.toText(date)).take
    DocumentMetaData(Some(I18NFragment.create(title)), datePublished = Some(d))
  }
}
