package org.smartdox

/*
 * @since   Aug. 19, 2026
 * @version Aug. 19, 2026
 * @author  ASAMI, Tomoharu
 */

import scala.language.implicitConversions
import scalaz.{Value => _, _}, Scalaz._, WriterT._, Show._, Validation._
import java.net.URI
import java.net.URL
import java.util.Locale
import scala.xml.{Node => XNode, _}
import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.context.Showable
import org.goldenport.context.Conclusion
import org.goldenport.context.Consequence
import org.goldenport.context.DateTimeContext
import org.goldenport.collection.VectorMap
import org.goldenport.collection.NonEmptyVector
import org.goldenport.parser._
import org.goldenport.io.MimeType
import org.goldenport.bag.ChunkBag
import org.goldenport.extension.IDocument
import org.goldenport.extension.IRecord
import org.goldenport.tree.{Tree => GTree}
import org.goldenport.tree.TreeNode
import org.goldenport.tree.HomoTreeTransformer
import org.goldenport.xsv.{Lxsv, LxsvSequence}
import org.goldenport.hocon.HoconUtils
import org.goldenport.value._
import org.goldenport.values.PathName
import org.goldenport.values.LocalDateOrDateTime
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NContainer
import org.goldenport.i18n.I18NHangar
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.LocaleUtils
import org.goldenport.xml.XmlUtils
import org.goldenport.record.v3.{Table => RTable}
import org.goldenport.record.v3.ITable
import org.goldenport.util.StringUtils
import org.goldenport.util.AnyUtils
import org.goldenport.util.ListUtils
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.metadata.DoxCacheControl
import org.smartdox.generator.Context
import org.smartdox.converter.DoxTreeVisitor
import org.smartdox.parser.Dox2Parser
import org.smartdox.parser.DoxLinesParser.BlockMacro
import org.smartdox.parser.PureParser
import org.smartdox.structure.StructureObject.KeyContent
import org.smartdox.util.DoxUtils


case class Document(
  head: Head,
  body: Body,
  foot: Option[Foot] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Dox {
  def isVisialBlock: Boolean = false
  override val elements = List(head, body) ::: foot.toList
  override def showTerm = "html"
  override def showOpenText = "<!DOCTYPE html><html>"
  override def showCloseText = "</html>"

  override protected def print_Open(buf: StringBuilder): Unit = {
    print_open_tag(buf, "document")
  }

  override protected def print_Close(buf: StringBuilder): Unit = {
    print_close_tag(buf, "document")
  }

  override def equals_Value(o: Dox) = o match {
    case m: Document => head == m.head && body == m.body && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    def s(h: Head, b: Body): ValidationNel[String, Dox] = {
      Success(copy(h, b))
    }

    cs.length match {
      case 0 => s(head, body)
      case 1 => cs.head match {
        case h: Head => s(h, body)
        case b: Body => s(head, b)
        case d => _copy_v(cs)
      }
      case 2 =>
        (cs(0), cs(1)) match {
          case (h: Head, b: Body) => s(h, b)
          case _ => _copy_v(cs)
        }
        // val h: ValidationNel[String, Head] = cs(0) match {
        //   case h: Head => Success(h)
        //   case d => to_failure(cs)
        // }
        // val b: ValidationNel[String, Body] = cs(1) match {
        //   case b: Body => Success(b)
        //   case d => to_failure(cs)
        // }
        // (h |@| b) { case (h, b) => copy(h, b) }
      case _ => _copy_v(cs)
    }
  }

  private def _copy_v(cs: List[Dox]): ValidationNel[String, Dox] = for {
    x <- body.copyV(cs)
  } yield copy(head, x)

//  def isMarkCache: Boolean = head.isMarkCache
  def markCache: Document = copy(head = head.markCache)
}
object Document extends DoxFactory {
  val label = "document"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Document =
    RAISE.unsupportedOperationFault

  // def unapply(p: (String, VectorMap[String, String], Seq[Dox])): Option[Document] =
  //   if (p._1 == name)
  //     Some(apply(p._1, p._2, p._3))
  //   else
  //     None

  def create(p: Head): Document = Document(p, Body.empty)

  def create(p: Body): Document = Document(Head.empty, p)

  def create(p: Dox): Document = create(Body(p))
}

case class Head(
  css: Option[String] = None,
  csslink: Option[String] = None,
  cacheControl: Head.CacheControl = Head.CacheControl.empty,
  seo: Head.Seo = Head.Seo.empty,
  metadata: DocumentMetaData = DocumentMetaData.empty,
  attributes: VectorMap[String, String] = VectorMap.empty,
  doxCacheControl: Option[DoxCacheControl] = None,
  location: Option[ParseLocation] = None
) extends Dox {
  def isVisialBlock: Boolean = false
  override def isEmpty = (
    css.isEmpty && csslink.isEmpty &&
      cacheControl.isEmpty && seo.isEmpty &&
      metadata.isEmpty &&
      attributes.isEmpty && location.isEmpty
  )

  override def equals_Value(o: Dox) = o match {
    case m: Head =>
      css == m.css && csslink == m.csslink &&
      cacheControl == m.cacheControl && seo == m.seo &&
      metadata == m.metadata &&
      attributes == m.attributes
    case _ => false
  }

  def equalsWithoutDoxCacheControl(p: Head): Boolean = {
    css.equals(p.css) &&
    csslink.equals(p.csslink) &&
    cacheControl.equals(p.cacheControl) &&
    seo.equals(p.seo) &&
    metadata.equals(p.metadata) &&
    attributes.equals(p.attributes)
  }

  override def copyV(cs: List[Dox]) = {
    if (cs.isEmpty) Success(this)
    else to_failure(cs)
  }

  override def show_Open(buf: StringBuilder) {
    def showslot(name: String, contents: InlineContents) {
      if (contents.nonEmpty) {
        buf.append("<")
        buf.append(name)
        buf.append(">")
        contents.foreach(_.toString(buf))
        buf.append("</")
        buf.append(name)
        buf.append(">")
      }
    }
    buf.append(showOpenText)
    showslot("title", titleDefault)
    // showslot("author", author)
    showslot("date", date)
    css foreach { x =>
      buf.append("<style type=\"text/css\"><!--\n")
      buf.append(x)
      buf.append("\n--></style>")
    }
    csslink foreach { x =>
      buf.append("<link rel=\"stylesheet\" type=\"text/css\" href=\"")
      buf.append(x)
      buf.append("\">")
    }
  }

  override protected def print_Open(buf: StringBuilder): Unit = {
    print_open_tag(buf, "head", attributes)
    metadata.printFlat(buf)
    doxCacheControl.map(_.print(buf))
  }

  override protected def print_Close(buf: StringBuilder): Unit = {
    print_close_tag(buf, "head")
  }

  def title: Option[I18NFragment] = metadata.title
  def titleDefault: InlineContents = metadata.getTitleInclineContentsDefault getOrElse Nil
  def date: InlineContents = metadata.publishedAt.toList.map(x => Text(x.print))
  def author: InlineContents = seo.author getOrElse Nil
  def description: Option[I18NFragment] = metadata.description

  def getTitleI18NString: Option[I18NString] = title.map(_.toI18NString)

  def distillTitleStringDefault: Option[String] = titleDefault match {
    case Nil => None
    case xs => Some(Dox.toText(xs))
  }

  def distillTitleString(implicit ctx: I18NContext): Option[String] =
    title.map(_.distillString(ctx.locale))

  override def isOpenClose = titleDefault.isEmpty && author.isEmpty && date.isEmpty

  def getAuthorString(locale: Locale): Option[String] = metadata.author.map(_.distillString(locale))

  def getPublisedAtString(locale: Locale): Option[String] = metadata.publishedAt.map(_.toLocalDate.toString) // TODO DateTimeContext and Locale

  def getModefinedAtString(locale: Locale): Option[String] = metadata.modifiedAt.map(_.toLocalDate.toString) // TODO DateTimeContext and Locale

  def toOption: Option[Head] =
    if (isEmpty)
      None
    else
      Some(this)

  def withDocumentMetaData(p: DocumentMetaData) = copy(metadata = p)

  def withTitle(ps: InlineContents) = copy(metadata = metadata.withTitle(ps))

  def withSummary(ps: InlineContents) = copy(metadata = metadata.withSummary(ps))

  def withSummaryIfRequired(ps: InlineContents) = copy(metadata = metadata.withSummaryIfRequired(ps))

  private def _with_summary(p: String) = copy(metadata = metadata.withSummary(p))

  def merge(p: Head): Head = Head(
    css |+| p.css,
    csslink |+| p.csslink,
    cacheControl + p.cacheControl,
    seo + p.seo,
    metadata + p.metadata, // properties.withFallback(p.properties),
    attributes ++ p.attributes,
    doxCacheControl,
    location orElse p.location
  )

  def merge(p: DocumentMetaData): Head = copy(metadata = metadata + p)

//  def isMarkCache: Boolean = doxCacheControl.fold(false)(_.isMarked)

  def markCache: Head = {
    val dcc = doxCacheControl.map(_.mark) getOrElse DoxCacheControl.marked()
    copy(doxCacheControl = Some(dcc))
  }
}

object Head extends DoxFactory {
  val empty = Head()

  case class CacheControl(
  ) {
    def isEmpty = true

    def +(p: CacheControl) = this // TODO
  }
  object CacheControl {
    val empty = CacheControl()
  }

  case class Seo(
    basic: BasicSeo = BasicSeo.empty,
    openGraphProtocol: OpenGraphProtocol = OpenGraphProtocol.empty,
    twitterCard: TwitterCard = TwitterCard.empty
  ) {
    def isEmpty = basic.isEmpty && openGraphProtocol.isEmpty && twitterCard.isEmpty

    def toOption: Option[Seo] = if (isEmpty) None else Some(this)

    def +(rhs: Seo) = copy(
      basic = basic + rhs.basic,
      openGraphProtocol = openGraphProtocol + rhs.openGraphProtocol,
      twitterCard = twitterCard + rhs.twitterCard
    )

    def author = basic.author
  }
  object Seo {
    val empty = Seo()

    def author(p: InlineContents) = empty.copy(basic = BasicSeo.author(p))

    def parseFlat(elem: XNode): Consequence[Option[Seo]] = {
      for {
        bseo <- BasicSeo.parseFlat(elem)
      } yield {
        Seo(bseo).toOption
      }
    }
  }

  case class BasicSeo(
    description: Option[String] = None,
    robots: Option[String] = None,
    canonical: Option[URL] = None,
    author: Option[InlineContents] = None,
    keywords: Option[String] = None
  ) {
    def isEmpty = true // TODO

    def +(p: BasicSeo) = this // TODO
  }
  object BasicSeo {
    val empty = BasicSeo()

    def author(p: InlineContents) = if (p.isEmpty) empty else empty.copy(author = Some(p))

    def parseFlat(elem: XNode): Consequence[BasicSeo] = {
      for {
        authorx <- XmlUtils.getElementC(elem, "author")
        author <- authorx.traverse(PureParser.buildChildrenInlinesC)
      } yield {
        BasicSeo(author = author)
      }
    }
  }

  case class OpenGraphProtocol(
    title: Option[String] = None,
    description: Option[String] = None,
    image: Option[URL] = None,
    url: Option[URL] = None,
    pageType: Option[String] = None
  ) {
    def isEmpty = true // TODO

    def +(p: OpenGraphProtocol) = this // TODO

    def enablePageType = copy(pageType = Some("website"))
  }
  object OpenGraphProtocol {
    val empty = OpenGraphProtocol()
  }

  case class TwitterCard(
    card: Option[String] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    image: Option[URL] = None,
    site: Option[String] = None
  ) {
    def isEmpty = true // TODO

    def +(p: TwitterCard) = this // TODO
  }
  object TwitterCard {
    val empty = TwitterCard()
  }

  val label = "head"

  def apply(
    attrs: VectorMap[String, String],
    body: Seq[Dox]
  )(implicit ctx: DateTimeContext): Head = {
    case class Z(properties: Hocon = HoconUtils.empty) {
      def r = Head(metadata = DocumentMetaData.create(properties))

      def +(rhs: Dox) = rhs match {
        case m: Text =>
          HoconUtils.parseConfig(m.contents).fold(
            f => copy(properties = HoconUtils.addProperty(
              properties,
              "error",
              f.message
            )),
            s => copy(properties = properties.withFallback(s))
          )
        case m => this // TODO
      }
    }
    body.foldLeft(Z())(_+_).r
  }

  def apply(
    title: Inline,
    subtitle: String, // TODO
    css: Option[String],
    csslink: Option[String]
  ): Head = {
    val metadata = DocumentMetaData.create(title)
    new Head(metadata = metadata, css = css, csslink = csslink)
  }

  def create(
    md: Option[DocumentMetaData],
    seo: Option[Seo],
    dcc: Option[DoxCacheControl]
  ): Head = {
    new Head(
      seo = seo getOrElse Seo.empty,
      metadata = md getOrElse DocumentMetaData.empty,
      doxCacheControl = dcc
    )
  }

  def create(
    title: InlineContents,
    author: InlineContents,
    date: InlineContents,
    organization: InlineContents = Nil
  )(implicit dctx: DateTimeContext): Head = {
    val metadata = DocumentMetaData.create(title, date, author, organization)
    new Head(metadata = metadata)
  }

  def title(title: Inline): Head = Head(metadata = DocumentMetaData.create(title))

  def builder(implicit dctx: DateTimeContext) = new Builder()

  class Builder(implicit dctx: DateTimeContext)  {
    var title: InlineContents = Nil
    var author: InlineContents = Nil
    var date: InlineContents = Nil
    var organization: InlineContents = Nil

    def build() = create(title, author, date, organization)
  }
}

case class Body(
  contents: List[Dox],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Dox {
  def isVisialBlock: Boolean = false
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Body => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs, location = get_location(location, contents)))
  }
}

object Body extends DoxFactory {
  val label = "body"

  val empty = Body(Nil)

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Body =
    RAISE.unsupportedOperationFault

  def apply(node: Dox) = node match {
    case m: Fragment => new Body(m.contents)
    case m => new Body(List(m))
  }
}

// 2025-09-06
case class Foot(
  contents: List[Dox] = Nil,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override def showTerm = "foot"
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Foot => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs, location = get_location(location, cs)))
  }
}

case class Section(
  title: List[Inline],
  contents: List[Dox],
  level: Int = 1,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  lazy val titleName = Dox.toText(title)
  lazy val keyForModel: String = titleName.trim.toLowerCase
  lazy val nameForModel: String = titleName.trim

  def titleI18NFragment: I18NFragment = I18NFragment.create(title)

  override val elements = contents
  override def show_Open(buf: StringBuilder) {
    val showh = "h" + (level + 1)
    buf.append(showOpenText)
    buf.append("<")
    buf.append(showh)
    buf.append(">")
    title.foreach(_.toString(buf))
    buf.append("</")
    buf.append(showh)
    buf.append(">")
  }
  override def isOpenClose = false

  override def print_Open(buf: StringBuilder): Unit = {
    val showh = "title"
    buf.append(showOpenText)
    buf.append("<")
    buf.append(showh)
    buf.append(">")
    title.foreach(_.printDox(buf))
    buf.append("</")
    buf.append(showh)
    buf.append(">")
  }

  override def equals_Value(o: Dox) = o match {
    case m: Section => title == m.title && contents == m.contents && level == m.level && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(title, cs, level, location = get_location(location, cs))) // XXX level
  }

  def withTitle(p: InlineContents): Section = copy(title = p)

  def tableList: List[Table] = contents.collect {
    case m: Table => m
  }

  def distillDescription: Description = {
    val name = titleName
    val xs = contents.takeWhile(_.isInstanceOf[Section] == false)
    val dox = xs match {
      case Nil => EmptyDox
      case x :: Nil => x
      case xs => Fragment(xs)
    }
    Description.name(name, dox)
  }

  def makeKeyContentValue: KeyContent[Value] = {
    val key = keyForModel

    case class Z(
      prologue: Vector[Dox] = Vector.empty,
      ul: Option[Ul] = None,
      epilogue: Vector[Dox] = Vector.empty
    ) {
      def r = {
        val v = ul match {
          case Some(s) => s.contents.headOption match {
            case Some(ss) => Value.create(ss.contents)
            case None => Value.empty
          }
          case None => Value.empty
        }
        KeyContent(key, v)
      }

      def +(rhs: Dox) = ul match {
        case Some(s) => copy(epilogue = epilogue :+ rhs)
        case None => rhs match {
          case m: Ul => copy(ul = Some(m))
          case m => copy(prologue = prologue :+ rhs)
        }
      }
    }

    contents.foldLeft(Z())(_+_).r
  }

  def makeKeyContentValueList: KeyContent[List[Value]] = {
    val key = keyForModel

    case class Z(
      prologue: Vector[Dox] = Vector.empty,
      ul: Option[Ul] = None,
      epilogue: Vector[Dox] = Vector.empty
    ) {
      def r = {
        val v = ul match {
          case Some(s) => s.contents.map(x => Value.create(x.contents))
          case None => Nil
        }
        KeyContent(key, v)
      }

      def +(rhs: Dox) = ul match {
        case Some(s) => copy(epilogue = epilogue :+ rhs)
        case None => rhs match {
          case m: Ul => copy(ul = Some(m))
          case m => copy(prologue = prologue :+ rhs)
        }
      }
    }

    contents.foldLeft(Z())(_+_).r
  }
}
object Section {
  def apply(title: String, p: Dox, ps: Dox*): Section =
    Section(List(Dox.text(title)), p +: ps.toList)

  def apply(title: String, ps: Seq[Dox]): Section =
    Section(List(Dox.text(title)), ps.toList)

  def apply(title: I18NFragment, ps: Seq[Dox]): Section =
    Section(List(title), ps.toList)

  def create(title: I18NString, p: Option[Dox]): Section =
    Section(List(I18NFragment.create(title)), p.toList)

  def create(title: I18NString, p: Seq[Dox]): Section =
    Section(List(I18NFragment.create(title)), p.toList)

  def create(title: I18NString, attrs: Map[String, String], p: Seq[Dox]): Section =
    Section(List(I18NFragment.create(title)), p.toList, attributes = VectorMap(attrs))

  def create(title: I18NString, p: I18NFragment): Section =
    Section(List(I18NFragment.create(title)), List(p))

  // def toKeyValues(p: Section): (String, List[InlineContents]) =
  //   (p.keyForModel, Dox._to_values_as_inline_contents(p))

  def toKeyValueOrValues(p: Section): KeyContent[Value] =
    KeyContent(p.keyForModel, Value.buildValueOrValuesI18N(p))

  def toKeyDescription(p: Section): KeyContent[I18NFragment] =
    KeyContent(p.keyForModel, Dox.toDescriptionAsI18NFragment(p))

  def toKeySectionList(p: Section): KeyContent[List[KeyContent[Section]]] =
    KeyContent(p.keyForModel, p.sections.map(x => KeyContent(x.keyForModel, x)))

  // def toKeyListOfKeyValueOrValues(p: Section): KeyContent[List[Section]] =
  //   KeyContent(p.keyForModel, p.sections) // TODO
}

case class Div(
  contents: List[Dox] = Nil,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override def showTerm = "div"
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Div => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs, location = get_location(location, cs)))
  }
}

object Div extends Div(Nil, VectorMap.empty, None) with DoxFactory {

  val label = "div"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: org.goldenport.context.DateTimeContext): Div =
    Div(body.toList, attrs)

  def apply(d: Dox) = new Div(List(d))

  def create(locale: Locale, body: Dox): Div =
    Div(List(body), VectorMap("lang" -> locale.toString))

  def build(elem: XNode): Div = {
    val cs = PureParser.buildChildren(elem)
    val attrs = PureParser.getAttributes(elem)
    Div(cs, attrs)
  }
}

case class Paragraph(
  contents: List[Dox],
  attributes: VectorMap[String, String] = VectorMap.empty,
  logicalLine: Option[LogicalLine] = None,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents
  override def showTerm = "p"

  override protected def equals_Value(o: Dox) = o match {
    case m: Paragraph =>
      val r = contents.equals(m.contents)
      // println(s"E: ${r}")
      r
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    val r = copy(cs, location = get_location(location, cs))
    // println(s"Paragraph#copyV: $cs => $r")
    Success(r)
  }

  override protected def to_Plain_Text(buf: StringBuilder) {
    to_Text(buf)
    buf.append("\n")
  }

  override protected def to_Data(buf: StringBuilder) {
    logicalLine match {
      case Some(s) => for (x <- s.physicalLines) {
        buf.append(x)
        buf.append("\n")
      }
      case None => to_Plain_Text(buf)
    }
  }

  override def getStringIfOnlyText: Option[String] = getTextIfOnly.map(_.contents)

  def append(p: Dox): Paragraph = copy(contents = contents :+ p)
}
object Paragraph extends DoxFactory {
  val label = "p"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: org.goldenport.context.DateTimeContext): Paragraph =
    Paragraph(body.toList, attrs)

  def apply(p: List[Dox], ll: LogicalLine): Paragraph =
    Paragraph(p, logicalLine = Some(ll))

  def apply(p: Dox, ll: LogicalLine): Paragraph = apply(List(p), ll)

  def create(locale: Locale, p: List[Dox]): Paragraph = Paragraph(
    p,
    VectorMap("lang" -> locale.toLanguageTag)
  )

  def text(p: String): Paragraph = Paragraph(List(Text(p)))

  def build(elem: XNode): Paragraph = {
    val cs = PureParser.buildChildren(elem)
    val attrs = PureParser.getAttributes(elem)
    Paragraph(cs, attrs)
  }
}

case class Text(
  contents: String,
  location: Option[ParseLocation] = None
) extends Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""
  override def show_Contents(buf: StringBuilder) {
    buf.append(Dox.escape(contents))
  }
  override def to_Text(buf: StringBuilder) {
    buf.append(contents)
  }
  override def to_Plain_Text(buf: StringBuilder) {
    buf.append(XmlUtils.escape(contents))
  }
  override def to_Data(buf: StringBuilder) {
    buf.append(contents)
  }

  override def print_Open(buf: StringBuilder) = {}

  override def print_Contents(buf: StringBuilder): Unit = {
    buf.append(contents)
  }

  override def print_Close(buf: StringBuilder) = {}

  override def getHtmlTag = None

  override protected def equals_Value(o: Dox) = o match {
    case m: Text =>
      val r = contents.equals(m.contents)
      // println(s"Text: ${r}")
      r
    case _ => false
  }

  override def copyV(cs: List[Dox]) = Success(this)

  override def getTextIfOnly = Some(this)

  def prepend(p: String): Text = copy(contents = p ++ contents)

  def prepend(p: Char): Text = copy(contents = p +: contents)

  def append(p: String): Text = copy(contents = contents ++ p)

  def xmlString: String = XmlUtils.escape(contents)

  def isBlank: Boolean = Strings.blankp(contents)
}
object Text {
  def apply(p: Char): Text = Text(p.toString)
}

case class Bold(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "b"

  override def equals_Value(o: Dox) = o match {
    case m: Bold => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(contents = x, location = get_location(location, cs)))
  }
}

object Bold extends Bold(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "b"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Bold =
    Bold(ensure_inline(body), attrs)

  def apply(element: Inline) = new Bold(List(element))

  def build(elem: XNode): Bold = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Bold(cs, attrs)
  }
}

// 2025-10-16
case class Strong(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "strong"

  override def equals_Value(o: Dox) = o match {
    case m: Strong => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(contents = x, location = get_location(location, cs)))
  }
}

object Strong extends Strong(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "strong"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Strong =
    Strong(ensure_inline(body), attrs)

  def apply(element: Inline) = new Strong(List(element))

  def build(elem: XNode): Strong = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Strong(cs, attrs)
  }
}

// 2025-10-16
case class Em(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "em"

  override def equals_Value(o: Dox) = o match {
    case m: Em => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(contents = x, location = get_location(location, cs)))
  }
}

object Em extends Em(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "em"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Em =
    Em(ensure_inline(body), attrs)

  def apply(element: Inline) = new Em(List(element))

  def build(elem: XNode): Em = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Em(cs, attrs)
  }
}

// 2011-12-26
case class Italic(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "i"

  override def to_Data_Prologue(buf: StringBuilder) {
    buf.append("/")
  }

  override def to_Data_Epilogue(buf: StringBuilder) {
    buf.append("/")
  }

  override def equals_Value(o: Dox) = o match {
    case m: Italic => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(contents = x, location = get_location(location, cs)))
  }
}

object Italic extends Italic(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "i"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Italic =
    Italic(ensure_inline(body), attrs)

  def apply(element: Inline) = new Italic(List(element))

  def createLinkCandidate(p: InlineContents): Italic = new Italic(p)

  def build(elem: XNode): Italic = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Italic(cs, attrs)
  }
}

case class Underline(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "u"

  override def to_Data_Prologue(buf: StringBuilder) {
    buf.append("_")
  }

  override def to_Data_Epilogue(buf: StringBuilder) {
    buf.append("_")
  }

  override def equals_Value(o: Dox) = o match {
    case m: Underline => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(x, location = get_location(location, cs)))
  }
}

object Underline extends Underline(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "u"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Underline =
    Underline(body.toList)

  def apply(element: Inline) = new Underline(List(element))

  def build(elem: XNode): Underline = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Underline(cs, attrs)
  }
}

case class Code(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  kind: Option[Code.Kind] = None,
  location: Option[ParseLocation] = None
) extends Inline with Preserve {
  override val elements = contents

  override def showParams = ListUtils.buildTupleList("class" -> kind.map(_.name))

  override def equals_Value(o: Dox) = o match {
    case m: Code => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object Code extends Code(Nil, VectorMap.empty, None, None) with DoxFactory {
  val label = "code"

  sealed trait Kind extends NamedValueInstance {
  }
  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(Console)

    case object Console extends Kind {
      val name = "console"
    }
  }

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Code =
    Code(body.toList)

  def apply(content: Inline) = new Code(List(content))

  def apply(content: Inline, kind: Option[Kind]) = new Code(List(content), kind = kind)

  def build(elem: XNode): Code = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Code(cs, attrs)
  }
}

case class InlineMacro(
  name: String,
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline with Preserve {
  override val elements = List(Text(contents))
  override def showTerm = "inlinemacro"
  override def showParams = List("name" -> name)

  override def to_Data_Prologue(buf: StringBuilder) {
    buf.append(name).append(":[")
  }

  override def to_Data_Epilogue(buf: StringBuilder) {
    buf.append("]")
  }

  override def equals_Value(o: Dox) = o match {
    case m: InlineMacro =>
      name == m.name && contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) =
    Success(copy(contents = cs.map(_.toText).mkString, location = get_location(location, cs)))
}

object InlineMacro extends InlineMacro("", "", VectorMap.empty, None) with DoxFactory {
  val label = "inlinemacro"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): InlineMacro = {
    val name = attrs.get("name").getOrElse("macro")
    InlineMacro(name, body.map(_.toText).mkString, attrs)
  }

  def pass(contents: String, location: Option[ParseLocation] = None): InlineMacro =
    InlineMacro("pass", contents, location = location)
}

case class Pre(
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline with Preserve {
  override val elements = List(Text(contents))
//  override def showParams = attributes.list

  override def equals_Value(o: Dox) = o match {
    case m: Pre => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs) >| copy(contents, attributes, get_location(location, cs))
  }
}
object Pre extends DoxFactory {
  val label = "pre"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Pre =
    apply(to_text(body), attrs)
}

case class Ul(
  contents: List[Li],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Ul => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_li(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object Ul extends Ul(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "ul"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Ul =
    Ul(ensure_li(body))

  val empty = Ul(Nil)

  def apply(element: Li) = new Ul(List(element))
  def apply(lis: Seq[Li]) = new Ul(lis.toList)

  def create(ps: Seq[Dox]) = apply(ps.map(Li.make))

  def toValuesAsInlineContents(ul: Ul): List[InlineContents] =
    ul.contents.map(Dox.toInlineContents)

  def toValues(ul: Ul): Value.Multiple =
    Value.Multiple(ul.contents.map(_.toText).toVector)
}

case class Ol(
  contents: List[Li],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Ol => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_li(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object Ol extends Ol(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "ol"

  val empty = Ol(Nil)

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Ol =
    Ol(ensure_li(body))
  def apply(lis: Seq[Li]) = new Ol(lis.toList)
}

case class Li(
  contents: List[ListContent],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  def :+(elem: ListContent): Li = {
    Li(contents :+ elem)
  }

  override def equals_Value(o: Dox) = o match {
    case m: Li => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_list_content(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object Li extends DoxFactory {
  val label = "li"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Li =
    Li(ensure_list_content(body))

  val empty = Li(Nil)
  def apply(text: String) = new Li(List(Text(text)))
  def apply(element: ListContent) = new Li(List(element))
  def apply(ps: Seq[ListContent]) = new Li(ps.toList)

  def create(p: Dox): Dox = p match {
    case m: ListContent => apply(m)
    case m => Error(s"Li: Illegal list content = $p")
  }

  def create(ps: Seq[Dox]): Dox = {
    case class Z(
      lis: Vector[ListContent] = Vector.empty,
      errors: Vector[Dox] = Vector.empty
    ) {
      def r = if (errors.isEmpty)
        apply(lis)
      else
        Error(s"Li: Illegal list contents = ${errors.mkString}")

      def +(rhs: Dox) = rhs match {
        case m: ListContent => copy(lis = lis :+ m)
        case m => copy(errors = errors :+ m)
      }
    }
    ps.foldLeft(Z())(_+_).r
  }

  def make(p: Dox): Li = p match {
    case m: ListContent => apply(m)
    case m => apply(Error(s"Li: Illegal list content = $m"))
  }
}

// 2011-12-30
