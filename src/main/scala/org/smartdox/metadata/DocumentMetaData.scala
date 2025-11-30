package org.smartdox.metadata

import scalaz.{Ordering => _, _}, Scalaz._
import scala.collection.immutable.SortedSet
import scala.util.Try
import scala.xml.{Node => XNode, Text => XText, _}
import java.net.URI
import java.util.Locale
import org.joda.time.DateTime
import org.joda.time.LocalDate
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
import org.smartdox.structure.StructureObject
import org.smartdox.structure.SectionStructureObject
import org.smartdox.structure.StructureProperties.StructurePropertyProperties
import org.smartdox.structure.ListI18NFragmentPropertyProperty
import org.smartdox.structure.I18NFragmentProperty

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
 *  version Oct. 26, 2025
 * @version Nov. 30, 2025
 * @author  ASAMI, Tomoharu
 */
case class DocumentMetaData(
  title: Option[I18NFragment] = None,
  titleImage: Option[URI] = None,
  category: Option[String] = None,
//  description: Option[I18NFragment] = None,
  explanation: Explanation = Explanation.empty,
  author: Option[I18NFragment] = None,
  organization: Option[I18NFragment] = None,
  keywords: List[String] = Nil,
  publishedAt: Option[LocalDateOrDateTime] = None,
  modifiedAtHistory: DocumentMetaData.UpdateHistory = DocumentMetaData.UpdateHistory.empty,
  kindOption: Option[DocumentMetaData.Kind] = None,
  statusOption: Option[DocumentMetaData.Status] = None,
  strategy: Set[DocumentMetaData.Strategy] = Set.empty,
  directive: DocumentMetaData.Directive = DocumentMetaData.Directive.empty,
  properties: Option[Hocon] = None
) extends Explanation.Holder {
  import DocumentMetaData._

  def isEmpty = title.isEmpty && explanation.isEmpty && author.isEmpty && keywords.isEmpty && publishedAt.isEmpty && modifiedAtHistory.isEmpty

  def toOption = if (isEmpty) None else Some(this)

  def modifiedAt: Option[LocalDateOrDateTime] = modifiedAtHistory.lastOption

  def kind: DocumentMetaData.Kind = kindOption getOrElse DocumentMetaData.Kind.Article

  def status: DocumentMetaData.Status = statusOption getOrElse {
    if (publishedAt.nonEmpty)
      DocumentMetaData.Status.Published
    else
      DocumentMetaData.Status.InPreparation
  }

  def isStable: Boolean = strategy.contains(Strategy.Stable)

  def isAutoWire: Option[Boolean] = directive.autoWire

  def isAutoWireDt: Option[Boolean] = isAutoWire // TODO

  def getTitleStringDefault: Option[String] = title.map(_.distillStringDefault)

  def titleStringDefault: String = getTitleStringDefault getOrElse ""

  def getTitleInclineContentsDefault: Option[InlineContents] = title.map(_.distillInlineContentsDefault)

  def getTitleI18NString: Option[I18NString] = title.map(_.toI18NString)

  private def getDescriptionStringDefault: Option[String] = description.map(_.distillStringDefault)

  def getBriefI18NString: Option[I18NString] = brief.map(_.toI18NString)

  def getSummaryI18NString: Option[I18NString] = summary.map(_.toI18NString)

  def getDescriptionI18NString: Option[I18NString] = (description orElse summary).map(_.toI18NString)

  def getHtmlDescriptionI18NString: Option[I18NString] = (summary orElse description).map(_.toI18NString)

  def getLead: Option[I18NFragment] = lead

  def getEffectiveLead: Option[I18NFragment] = lead orElse summary orElse description

  def getEffectiveTooltip: Option[I18NString] =
    explanation.getEffectiveTooltip.map(_.toI18NString)

  def getTitleString(locale: Locale): Option[String] =
    title.map(_.toI18NString.as(locale))

  def takeEffectiveHeadlineString(locale: Locale): String =
    getEffectiveHeadlineString(locale) getOrElse ""

  def getEffectiveHeadlineString(locale: Locale): Option[String] =
    explanation.getEffectiveHeadline.map(_.toI18NString.as(locale))

  def getEffectiveSummaryString(locale: Locale): Option[String] =
    explanation.getEffectiveSummary.map(_.toI18NString.as(locale))

  def getEffectiveSummary: Option[I18NString] = explanation.getEffectiveSummaryI18NString

  def getEffectiveBrief: Option[I18NString] = explanation.getEffectiveBriefI18NString

  def getEffectiveBriefString(locale: Locale): Option[String] = explanation.getEffectiveBriefString(locale)

  def getEffectiveDescription: Option[I18NString] = explanation.getEffectiveDescriptionI18NString

  def getEffectiveDescriptionString(locale: Locale): Option[String] = explanation.getEffectiveDescriptionString(locale)

  def getPublishedString(locale: Locale): Option[String] = publishedAt.map(AnyUtils.toPrint)

  def getModifiedString(locale: Locale): Option[String] = modifiedAt.map(AnyUtils.toPrint)

  def getAuthorString(locale: Locale): Option[String] = author.map(_.toI18NString.as(locale))

  def getOrganizationString(locale: Locale): Option[String] = organization.map(_.toI18NString.as(locale))

  def withTitle(p: InlineContents) = {
    val x = Dox.trimSingleLine(p)
    copy(title = Some(I18NFragment.create(x)))
  }

  def withTitle(p: String) = copy(title = Some(I18NFragment.create(p)))

  def withSummary(p: InlineContents) =
    copy(explanation = explanation.withSummary(p))

  def withSummary(p: String) =
    copy(explanation = explanation.withSummary(p))

  def withSummaryIfRequired(p: InlineContents) =
    copy(explanation = explanation.withSummaryIfRequired(p))

  def withExplanation(p: Explanation) =
    copy(explanation = p)

  def withUpdateHistory(p: DocumentMetaData.UpdateHistory) =
    copy(modifiedAtHistory = p)

  def complementTitleDate(
    ptitle: InlineContents,
    pdate: InlineContents
  )(implicit context: Context) = {
    val t = title orElse _to_title(ptitle)
    val d = _to_date(pdate)
    val (dp, dm) = (publishedAt, modifiedAt) match {
      case (Some(p), Some(m)) => (Some(p), None)
      case (Some(p), None) => (Some(p), d)
      case (None, Some(m)) => (d, None)
      case (None, None) => (d, None)
    }
    copy(
      title = t,
      publishedAt = dp,
      modifiedAtHistory = modifiedAtHistory.add(dm)
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
      organization orElse rhs.organization,
      (keywords ::: rhs.keywords).distinct,
      publishedAt orElse rhs.publishedAt,
      modifiedAtHistory + rhs.modifiedAtHistory,
      lastOption(kindOption, rhs.kindOption),
      lastOption(statusOption, rhs.statusOption),
      strategy ++ rhs.strategy,
      directive + rhs.directive
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
      PROP_MODIFIED_AT -> modifiedAtHistory.marshall, // TODO DateModified
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
  import io.circe.syntax._
  import io.circe.generic.extras._
  import io.circe.generic.extras.semiauto._

  implicit val circeconf = Configuration.default.
    withDefaults.withSnakeCaseMemberNames

  final val PROP_TITLE = "title"
  final val PROP_TITLE_IMAGE = "title_image"
  final val PROP_CATEGORY = "category"
  final val PROP_DESCRIPTION = "description"
  final val PROP_AUTHOR = "author"
  final val PROP_ORGANIZATION = "organization"
  final val PROP_KEYWORDS = "keywords"
  final val PROP_PUBLISHED_AT = "published_at"
  final val PROP_MODIFIED_AT = "modified_at"
  final val PROP_UPDATE = "update"
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
    case object Bibliography extends Kind {
      val name = "bibliography"
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
    val elements = Vector(Published, WorkInProgress, Draft, InPreparation, Inactive, Test, Error)

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
    case object Error extends Status {
      val name = "error"
      def noticePriority = 999
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

  case class Directive(
    autoWire: Option[Boolean] = None
  ) {
    def +(rhs: Directive) =
      copy(
        autoWire = OptionUtils.lastOption(autoWire, rhs.autoWire)
      )
  }
  object Directive {
    val PROP_AUTOWIRE = "directive.autowire"

    val empty = Directive()

    def createC(hocon: Hocon): Consequence[Directive] =
      for {
        autowire <- hocon.cBooleanOption(PROP_AUTOWIRE)
      } yield Directive(autowire)
  }

  case class UpdateHistory(
    slots: SortedSet[UpdateHistory.Slot] = UpdateHistory.emptySlots
  ) {
    def isEmpty = slots.isEmpty

    def +(rhs: UpdateHistory) = copy(slots ++ rhs.slots)

    def add(p: Option[LocalDateOrDateTime]): UpdateHistory =
      p match {
        case None => this
        case Some(dt) =>
          copy(slots = slots + UpdateHistory.Slot(dt, None))
      }

    def lastOption: Option[LocalDateOrDateTime] = slots.lastOption.map(_.modifiedAt)

    def localDates: Vector[LocalDate] = slots.map(_.modifiedAt.toLocalDate).toVector

    def asJsonString: String = this.asJson.noSpaces

    def marshall: Option[String] = if (isEmpty) None else Some(asJsonString)
  }
  object UpdateHistory {
    implicit val slotOrdering: Ordering[Slot] = Ordering.by(_.modifiedAt)

    private lazy val emptySlots: SortedSet[Slot] = SortedSet.empty[Slot]

    val empty = UpdateHistory()

    // implicit val doxEncoder: Encoder[Dox] =
    //   Encoder.encodeString.contramap(_.toString) // TODO

    implicit val UpdateHistoryMonoid: Monoid[UpdateHistory] = new Monoid[UpdateHistory] {
      def zero: UpdateHistory = UpdateHistory.empty
      def append(f1: UpdateHistory, f2: => UpdateHistory): UpdateHistory = f1 + f2
    }

    implicit val i18nFragmentEncoder: Encoder[I18NFragment] =
      Encoder.encodeString.contramap(_.toString) // TODO

    case class Slot(modifiedAt: LocalDateOrDateTime, description: Option[I18NFragment])
    object Slot {
    }

    implicit val slotEncoder: Encoder[Slot] =
      Encoder.forProduct2("modifiedAt", "description")(x =>
        (x.modifiedAt, x.description)
      )

    implicit val updatehistoryEncoder: Encoder[UpdateHistory] = Encoder.forProduct1("slots")(_.slots)

    def create(key: String, content: I18NFragment): Consequence[UpdateHistory] =
      for {
        d <- LocalDateOrDateTime.parseStatic(key)
      } yield UpdateHistory(SortedSet(Slot(d, Some(content))))

    def parse(h: Section): Consequence[UpdateHistory] = {
      import StructureObject.Builder.Config.Schema
      val bc = StructureObject.Builder.Config(
        Schema(
          Schema.Node.SectionLocalDateOrDateTimeSections(PROP_UPDATE)
        )
      )
      val builder = new StructureObject.Builder(bc)
      val r = builder.build(h)
      _parse(r)
    }

    private def _parse(p: StructureObject): Consequence[UpdateHistory] =
      Consequence {
        p match {
          case m: SectionStructureObject => m.properties match {
            case mm: StructurePropertyProperties => mm.props.foldMap {
              case mmm: ListI18NFragmentPropertyProperty => mmm.content.foldMap {
                case mmmm: I18NFragmentProperty =>
                  UpdateHistory.create(mmmm.key.value, mmmm.content).orZero
                case _ => UpdateHistory.empty
              }
              case _ => UpdateHistory.empty
            }
            case _ => UpdateHistory.empty
          }
          case _ => UpdateHistory.empty
        }
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
      organization <- hocon.cStringOption(PROP_ORGANIZATION)
      keywords <- hocon.cEagerStringList(PROP_KEYWORDS)
      published <- _get_localdateordatetime(hocon, PROP_PUBLISHED_AT)
      modified <- _take_update_history(hocon, PROP_MODIFIED_AT)
      kind <- hocon.cValueOption(Kind, PROP_KIND)
      status <- hocon.cValueOption(Status, PROP_STATUS)
      directive <- Directive.createC(hocon)
      strategy <- hocon.cValueList(Strategy, PROP_STRATEGY)
    } yield {
      val inlinetitle = title.map(x => I18NFragment.create(List(Text(x))))
      DocumentMetaData(
        inlinetitle,
        titleimage,
        category,
        exp,
        auth.map(I18NFragment.create),
        organization.map(I18NFragment.create),
        keywords,
        published,
        modified,
        kind,
        status,
        strategy.toSet,
        directive,
        Some(hocon)
      )
    }

  private def _get_localdateordatetime(
    hocon: Hocon,
    key: String
  )(implicit ctx: DateTimeContext): Consequence[Option[LocalDateOrDateTime]] =
    Consequence(hocon.cLocalDateOrDateTimeOption(key).toOption.flatten)

  private def _take_localdateordatetime_set(
    hocon: Hocon,
    key: String
  )(implicit ctx: DateTimeContext): Consequence[SortedSet[LocalDateOrDateTime]] =
    hocon.cLocalDateOrDateTimeSet(key)

  private def _take_update_history(
    hocon: Hocon,
    key: String
  )(implicit ctx: DateTimeContext): Consequence[DocumentMetaData.UpdateHistory] =
    for {
      xs <- _take_localdateordatetime_set(hocon, key)
    } yield {
      val slots = xs.map(dt => DocumentMetaData.UpdateHistory.Slot(dt, None))
      DocumentMetaData.UpdateHistory(SortedSet(slots.toSeq: _*))
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

  def create(exp: Explanation, uh: UpdateHistory): DocumentMetaData =
    DocumentMetaData.empty.withExplanation(exp).withUpdateHistory(uh)

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
      organization <- _get_i18nfragment(p, "organization")
      keywords <- _get_string_list_eager(p, "keywords")
      publishedat <- _get_localdateordatetime(p, "publishedAt")
      modifiedat <- _take_update_history(p, "modifiedAt")
      kind <- _get_powertype(p, Kind, "kind")
      status <- _get_powertype(p, Status, "status")
    } yield {
      DocumentMetaData(
        title,
        titleimage,
        category,
        exp,
        author,
        organization,
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
    Consequence(XmlUtils.getLocalDateOrDateTimeC(p, name).toOption.flatten)

  private def _take_localdateordatetime_set(p: XNode, name: String)(implicit ctx: DateTimeContext): Consequence[SortedSet[LocalDateOrDateTime]] =
    XmlUtils.takeLocalDateOrDateTimeSetC(p, name)

  private def _take_update_history(
    p: XNode,
    name: String
  )(implicit ctx: DateTimeContext): Consequence[DocumentMetaData.UpdateHistory] =
    for {
      xs <- _take_localdateordatetime_set(p, name)
    } yield {
      val slots = xs.map(dt => DocumentMetaData.UpdateHistory.Slot(dt, None))
      DocumentMetaData.UpdateHistory(SortedSet(slots.toSeq: _*))
    }

  private def _get_i18nfragment(p: XNode, name: String): Consequence[Option[I18NFragment]] =
    I18NFragment.getC(name, p)

  private def _get_powertype[T <: NamedValueInstance](p: XNode, pt: EnumerationClass[T], name: String): Consequence[Option[T]] =
    XmlUtils.getPowertypeC(p, pt, name)
}
