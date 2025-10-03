package org.smartdox.metadata

import scalaz._, Scalaz._
import scala.util.Try
import scala.xml.{Node => XNode, Text => XText, _}
import java.net.URI
import java.util.Locale
import org.joda.time.DateTime
import com.typesafe.config.{Config => Hocon}
import org.goldenport.context.Consequence
import org.goldenport.context.DateTimeContext
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NContainer
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.value._
import org.goldenport.values.LocalDateOrDateTime
import org.goldenport.xml.XmlUtils
import org.goldenport.xml.XmlUtils.{printObject, printPowertype}
import org.goldenport.util.VectorUtils
import org.goldenport.util.AnyUtils
import org.goldenport.util.OptionUtils
import org.goldenport.util.OptionUtils.lastOption
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.parser.PureParser

/*

| attribute   | style    | lines | html        |
|-------------+----------+-------+-------------|
| title       | phrase   |     1 |             |
| headline    | phrase   |     1 |             |
| brief       | phrase   |     1 |             |
| summary     | sentense |     1 | description |
| abstract    | sentense |     N |             |
| description | sentense |     N |             |
| remarks     | sentense |     1 |             |

 * 
 * @since   Apr. 29, 2025
 *  version Apr. 30, 2025
 *  version Jun. 26, 2025
 *  version Jul. 27, 2025
 *  version Aug. 29, 2025
 *  version Sep. 28, 2025
 * @version Oct.  4, 2025
 * @author  ASAMI, Tomoharu
 */
case class DocumentMetaData(
  title: Option[I18NFragment] = None,
  titleImage: Option[URI] = None,
  category: Option[String] = None,
//  description: Option[I18NFragment] = None,
  explanation: Explanation = Explanation.empty,
  author: Option[I18NFragment] = None,
  keywords: List[String] = Nil,
  publishedAt: Option[LocalDateOrDateTime] = None,
  modifiedAt: Option[LocalDateOrDateTime] = None,
  kindOption: Option[DocumentMetaData.Kind] = None,
  statusOption: Option[DocumentMetaData.Status] = None,
  strategy: Set[DocumentMetaData.Strategy] = Set.empty,
  properties: Option[Hocon] = None
) extends Explanation.Holder {
  import DocumentMetaData._

  def isEmpty = title.isEmpty && explanation.isEmpty && author.isEmpty && keywords.isEmpty && publishedAt.isEmpty && modifiedAt.isEmpty

  def toOption = if (isEmpty) None else Some(this)

  def kind: DocumentMetaData.Kind = kindOption getOrElse DocumentMetaData.Kind.Article

  def status: DocumentMetaData.Status = statusOption getOrElse {
    if (publishedAt.nonEmpty)
      DocumentMetaData.Status.Published
    else
      DocumentMetaData.Status.InPreparation
  }

  def isStable: Boolean = strategy.contains(Strategy.Stable)

  def getTitleStringDefault: Option[String] = title.map(_.distillStringDefault)

  def titleStringDefault: String = getTitleStringDefault getOrElse ""

  def getTitleInclineContentsDefault: Option[InlineContents] = title.map(_.distillInlineContentsDefault)

  def getTitleI18NString: Option[I18NString] = title.map(_.toI18NString)

  private def getDescriptionStringDefault: Option[String] = description.map(_.distillStringDefault)

  def getBriefI18NString: Option[I18NString] = brief.map(_.toI18NString)

  def getSummaryI18NString: Option[I18NString] = summary.map(_.toI18NString)

  def getDescriptionI18NString: Option[I18NString] = (description orElse summary).map(_.toI18NString)

  def getHtmlDescriptionI18NString: Option[I18NString] = (summary orElse description).map(_.toI18NString)

  def getLead: Option[I18NFragment] = description

  def getEffectiveTooltip: Option[I18NString] =
    explanation.getEffectiveTooltip.map(_.toI18NString)

  def withTitle(p: InlineContents) = {
    val x = Dox.trimSingleLine(p)
    copy(title = Some(I18NFragment.create(x)))
  }

  def withTitle(p: String) = copy(title = Some(I18NFragment.create(p)))

  def withSummary(p: InlineContents) =
    copy(explanation = explanation.withSummary(p))

  def withSummary(p: String) =
    copy(explanation = explanation.withSummary(p))

  def withExplanation(p: Explanation) =
    copy(explanation = p)

  def complementTitleDate(
    ptitle: InlineContents,
    pdate: InlineContents
  )(implicit context: Context) = {
    val t = title orElse _to_title(ptitle)
    val d = _to_date(pdate)
    val (dp, dm) = (publishedAt, modifiedAt) match {
      case (Some(p), Some(m)) => (Some(p), Some(m))
      case (Some(p), None) => (Some(p), d)
      case (None, Some(m)) => (d, Some(m))
      case (None, None) => (d, None)
    }
    copy(
      title = t,
      publishedAt = dp,
      modifiedAt = dm
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
      explanation + rhs.explanation,
      author orElse rhs.author,
      (keywords ::: rhs.keywords).distinct,
      publishedAt orElse rhs.publishedAt,
      modifiedAt orElse rhs.modifiedAt,
      lastOption(kindOption, rhs.kindOption),
      lastOption(statusOption, rhs.statusOption),
      strategy ++ rhs.strategy
    )

  def distillLocale(p: Option[Locale]): DocumentMetaData =
    copy(
      title = title.map(_.distillI18NFragment(p)),
      author = author.map(_.distillI18NFragment(p)),
      explanation = explanation.distillLocale(p)
    )

  def distillLocale(p: Locale): DocumentMetaData =
    copy(
      title = title.map(_.distillI18NFragment(p)),
      author = author.map(_.distillI18NFragment(p)),
      explanation = explanation.distillLocale(p)
    )

  def toFlattenVector: Vector[(String, String)] =
    VectorUtils.buildTupleVector(
      PROP_TITLE -> getTitleStringDefault,
      PROP_TITLE_IMAGE -> titleImage.map(_.toString),
      PROP_CATEGORY -> category,
      PROP_DESCRIPTION -> getDescriptionStringDefault,
      PROP_AUTHOR -> author.map(_.print),
      PROP_KEYWORDS -> _keywords_string,
      PROP_PUBLISHED_AT -> publishedAt.map(_to_string), // TODO DatePublished
      PROP_MODIFIED_AT -> modifiedAt.map(_to_string), // TODO DateModified
      PROP_KIND -> Some(kind.name),
      PROP_STATUS -> Some(status.name)
    )

  private def _keywords_string: Option[String] =
    keywords match {
      case Nil => None
      case xs => Some(xs.mkString(","))
    }

  private def _to_string(p: LocalDateOrDateTime) = p.print

  def printFlat(buf: StringBuilder): Unit = {
    val kws = keywords match {
      case Nil => None
      case xs => Some(xs.mkString(","))
    }
    Dox.printI18NFragment(buf, "title", title)
    printObject(buf, "titleImage", titleImage)
    printObject(buf, "category", category)
    explanation.printFlat(buf)
    Dox.printDox(buf, "author", author)
    printObject(buf, "keywords", kws)
    printObject(buf, "publishedAt", publishedAt)
    printObject(buf, "modifiedAt", modifiedAt)
    printPowertype(buf, "kind", kindOption)
    printPowertype(buf, "status", statusOption)
  }
}

object DocumentMetaData {
  import io.circe._
  import io.circe.generic.extras._
  import io.circe.generic.extras.semiauto._

  implicit val circeconf = Configuration.default.
    withDefaults.withSnakeCaseMemberNames

  final val PROP_TITLE = "title"
  final val PROP_TITLE_IMAGE = "title_image"
  final val PROP_CATEGORY = "category"
  final val PROP_DESCRIPTION = "description"
  final val PROP_AUTHOR = "author"
  final val PROP_KEYWORDS = "keywords"
  final val PROP_PUBLISHED_AT = "published_at"
  final val PROP_MODIFIED_AT = "modified_at"
  final val PROP_KIND = "kind"
  final val PROP_STATUS = "status"
  final val PROP_STRATEGY = "strategy"

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
    def noticePriorityDraft: Int = noticePriority
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
        OptionUtils.compareAscOption(lhs.map(_.noticePriority), rhs.map(_.noticePriority))

    def compareOption(lhs: Status, rhs: Status): Option[Boolean] =
      if (lhs.noticePriority == rhs.noticePriority)
        None
      else
        Some(lhs.noticePriority < rhs.noticePriority)

    def compareDraftOption(lhs: Option[Status], rhs: Option[Status]): Option[Boolean] =
      if (lhs == rhs)
        None
      else
        OptionUtils.compareAscOption(lhs.map(_.noticePriorityDraft), rhs.map(_.noticePriorityDraft))
  }

  sealed trait Strategy extends NamedValueInstance {
  }
  object Strategy extends EnumerationClass[Strategy] {
    val elements = Vector(Stable)

    case object Stable extends Strategy {
      val name = "stable"
    }
  }

  def create(hocon: Hocon)(implicit ctx: DateTimeContext): DocumentMetaData =
    createC(hocon).take

  def createC(hocon: Hocon)(implicit ctx: DateTimeContext): Consequence[DocumentMetaData] =
    for {
      title <- hocon.cStringOption(PROP_TITLE)
      titleimage <- hocon.cUriOption(PROP_TITLE_IMAGE)
      category <- hocon.cStringOption(PROP_CATEGORY)
      exp <- Explanation.parse(hocon)
      auth <- hocon.cStringOption(PROP_AUTHOR)
      keywords <- hocon.cEagerStringList(PROP_KEYWORDS)
      published <- hocon.cLocalDateOrDateTimeOption(PROP_PUBLISHED_AT)
      modified <- hocon.cLocalDateOrDateTimeOption(PROP_MODIFIED_AT)
      kind <- hocon.cValueOption(Kind, PROP_KIND)
      status <- hocon.cValueOption(Status, PROP_STATUS)
      strategy <- hocon.cValueList(Strategy, PROP_STRATEGY)
    } yield {
      val inlinetitle = title.map(x => I18NFragment.create(List(Text(x))))
      DocumentMetaData(
        inlinetitle,
        titleimage,
        category,
        exp,
        auth.map(I18NFragment.create),
        keywords,
        published,
        modified,
        kind,
        status,
        strategy.toSet,
        Some(hocon)
      )
    }

  def create(title: Inline): DocumentMetaData =
    DocumentMetaData(Some(I18NFragment.create(List(title))))

  def create(
    title: InlineContents,
    date: InlineContents
  )(implicit ctx: DateTimeContext): DocumentMetaData = {
    val d = date match {
      case Nil => None
      case xs => LocalDateOrDateTime.parse(Dox.toText(date)).toOption
    }
    DocumentMetaData(Some(I18NFragment.create(title)), publishedAt = d)
  }

  def create(p: Explanation): DocumentMetaData =
    DocumentMetaData.empty.withExplanation(p)

  def create(title: InlineContents, explanation: Explanation): DocumentMetaData =
    DocumentMetaData.empty.withTitle(title).withExplanation(explanation)

  def create(title: String, explanation: Explanation): DocumentMetaData =
    DocumentMetaData.empty.withTitle(title).withExplanation(explanation)

  def parseFlat(p: XNode)(implicit ctx: DateTimeContext): Consequence[Option[DocumentMetaData]] = {
    for {
      title <- _get_i18nfragment(p, "title")
      titleimage <- _get_uri(p, "titleImage")
      category <- _get_string(p, "category")
      exp <- Explanation.parse(p)
      author <- _get_i18nfragment(p, "author")
      keywords <- _get_string_list_eager(p, "keywords")
      publishedat <- _get_localdateordatetime(p, "publishedAt")
      modifiedat <- _get_localdateordatetime(p, "modifiedAt")
      kind <- _get_powertype(p, Kind, "kind")
      status <- _get_powertype(p, Status, "status")
    } yield {
      DocumentMetaData(
        title,
        titleimage,
        category,
        exp,
        author,
        keywords,
        publishedat,
        modifiedat,
        kind,
        status
      ).toOption
    }
  }

  private def _get_string(p: XNode, name: String): Consequence[Option[String]] =
    XmlUtils.getStringC(p, name)

  private def _get_string_list_eager(p: XNode, name: String): Consequence[List[String]] =
    XmlUtils.getStringListEagerC(p, name)

  private def _get_uri(p: XNode, name: String): Consequence[Option[URI]] =
    XmlUtils.getUriC(p, name)

  private def _get_localdateordatetime(p: XNode, name: String)(implicit ctx: DateTimeContext): Consequence[Option[LocalDateOrDateTime]] =
    XmlUtils.getLocalDateOrDateTimeC(p, name)

  private def _get_i18nfragment(p: XNode, name: String): Consequence[Option[I18NFragment]] =
    I18NFragment.getC(name, p)

  private def _get_powertype[T <: NamedValueInstance](p: XNode, pt: EnumerationClass[T], name: String): Consequence[Option[T]] =
    XmlUtils.getPowertypeC(p, pt, name)
}
