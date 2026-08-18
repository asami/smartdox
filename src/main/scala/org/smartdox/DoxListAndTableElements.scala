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


case class Del(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Del => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object Del extends Del(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "del"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Del =
    Del(ensure_inline(body))

  def apply(element: Inline) = new Del(List(element))
}

case class Hyperlink(
  contents: List[Inline],
  href: URI,
  title: Option[I18NString] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  source: Option[PathName] = None,
  location: Option[ParseLocation] = None
) extends Inline {
  import Hyperlink._

  override val elements = contents
  override def showTerm = "a"
  override def showParams = List("href" -> href.toASCIIString())

  override def equals_Value(o: Dox) = o match {
    case m: Hyperlink => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, href, location = get_location(location, cs)))
  }

  def withTitle(p: String): Hyperlink = copy(title = Some(I18NString(p)))

  def getHtmlClass: Option[String] = attributes.get("class")

  def isLocalOrRelative: Boolean = linkKind match {
    case LinkKind.Local => true
    case LinkKind.Relative => true
    case _ => false
  }

  def isGlossary: Boolean = linkKind match {
    case LinkKind.Glossary => true
    case _ => false
  }

  lazy val linkKind: LinkKind = getHtmlClass match {
    case Some("glossary") => LinkKind.Glossary
    case _ => _link_kind_by_scheme
  }

  private def _link_kind_by_scheme = Option(href.getScheme) match {
    case Some(s) =>
      if (s == "file")
        LinkKind.Local
      else
        LinkKind.External
    case None => LinkKind.Relative
  }
}
object Hyperlink extends DoxFactory {
  val label = "a"

  sealed trait LinkKind
  object LinkKind {
    case object Local extends LinkKind
    case object Relative extends LinkKind
    case object External extends LinkKind
    case object Glossary extends LinkKind
    case object Bibliography extends LinkKind
  }

   def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Hyperlink =
     apply(ensure_inline(body), attrs.applyIgnoreCase("href"))

  def apply(c: Seq[Inline], href: String): Hyperlink =
    new Hyperlink(c.toList, new URI(href))

  def apply(c: Seq[Inline], href: String, location: Option[ParseLocation]): Hyperlink =
    new Hyperlink(c.toList, new URI(href), None, VectorMap.empty, None, location)

  def apply(label: Inline, href: URI): Hyperlink =
    new Hyperlink(List(label), href)

  def apply(label: Inline, href: URI, attrs: VectorMap[String, String]): Hyperlink =
    new Hyperlink(List(label), href, None, attrs)

  def apply(label: Seq[Inline], href: URI): Hyperlink =
    new Hyperlink(label.toList, href)

  def apply(label: Seq[Inline], href: URI, title: Option[I18NString]): Hyperlink =
    new Hyperlink(label.toList, href, title)

  def apply(label: Seq[Inline], href: URI, title: Option[I18NString], attrs: VectorMap[String, String]): Hyperlink =
    Hyperlink(label.toList, href, title, attrs, None)

  def create(body: String, href: String): Hyperlink =
    create(body, new URI(href))

  def create(body: String, href: URI): Hyperlink =
    apply(Dox.parseInlineContentsInclusion(body), href)

  def create(body: String, href: URI, alt: String): Hyperlink =
    apply(Dox.parseInlineContentsInclusion(body), href, Some(I18NString(alt)))

  def create(body: Inline, href: String): Hyperlink =
    Hyperlink(Dox.parseInlineContentsInclusion(body), new URI(href))

   def create(body: Seq[Inline], href: String): Hyperlink =
    Hyperlink(Dox.parseInlineContentsInclusion(body), new URI(href))

  def create(url: String): Hyperlink =
    Hyperlink(List(Text(url)), new URI(url))

  def createCategory(body: Inline, href: URI): Hyperlink =
    Hyperlink(body, href, VectorMap("class" -> "category"))

  def createArticle(body: I18NString, href: URI, source: PathName): Hyperlink =
    Hyperlink(List(Dox.toDox(body)), href, None, VectorMap("class" -> "article"), source = Some(source))

  def createArticle(body: I18NFragment, href: URI, tooltip: Option[I18NString], source: PathName): Hyperlink =
    createArticle(List(body), href, tooltip, source)

  def createArticle(body: String, href: URI, tooltip: Option[String], source: PathName): Hyperlink =
    createArticle(List(Text(body)), href, tooltip.map(I18NString.apply), source)

  def createArticle(body: InlineContents, href: URI, tooltip: Option[I18NString], source: PathName): Hyperlink =
    tooltip match {
      case Some(s) => createArticle(body, href, s, source)
      case None => createArticle(body, href, source)
    }

  def createArticle(body: InlineContents, href: URI, tooltip: I18NString, source: PathName): Hyperlink =
    Hyperlink(body, href, Some(tooltip), VectorMap("class" -> "article"), source = Some(source))

  def createArticle(body: InlineContents, href: URI, source: PathName): Hyperlink =
    Hyperlink(body, href, None, VectorMap("class" -> "article"), source = Some(source))

  def createArticle(body: String): Hyperlink = Hyperlink(List(Text(body)), new URI(body))

  def createGlossary(body: String, href: URI, title: String): Hyperlink =
    Hyperlink(List(Text(body)), href, Some(I18NString(title)), VectorMap("class" -> "glossary"))

  def createGlossary(body: List[Inline], href: URI, title: String): Hyperlink =
    Hyperlink(body, href, Some(I18NString(title)), VectorMap("class" -> "glossary"))

  def createGlossary(body: String, href: URI): Hyperlink =
    Hyperlink(List(Text(body)), href, None, VectorMap("class" -> "glossary"))

  def createGlossary(body: List[Inline], href: URI): Hyperlink =
    Hyperlink(body, href, None, VectorMap("class" -> "glossary"))
}

case class ReferenceImg(
  src: URI,
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Img {
  // def attributesUnified = {
  //   val a = VectorMap("src" -> src.toString)
  //   val b = VectorMap.create("alt" -> alt)
  //   a ++ b ++ attributes
  // }
  override def showParams = ListUtils.buildTupleList(
    List("src" -> src.toString),
    List("atl" -> alt)
  )

  override def equals_Value(o: Dox) = o match {
    case m: ReferenceImg => src == m.src && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }

  def withSrc(p: String): ReferenceImg = copy(src = new URI(p))
}
object ReferenceImg extends DoxFactory {
  val label = "img"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): ReferenceImg =
    ReferenceImg(attrs.applyIgnoreCase("src"))

  def apply(p: String): ReferenceImg = ReferenceImg(new URI(p))
}

trait TableBlock extends Block {
  val caption: Option[Caption]
  val label: Option[String]
}

case class Table(
  head: Option[THead],
  body: TBody,
  foot: Option[TFoot],
  side: Option[TSide],
  colgroup: Option[Colgroup],
  caption: Option[Caption],
  label: Option[String],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TableBlock {
  def getCaptionName: Option[String] = caption.map(_.toText)
  override val elements = List(caption, colgroup, head, side, body.some, foot).flatten
  override def showParams = label.toList.map(x => ("id", x))

  override def equals_Value(o: Dox) = o match {
    case m: Table => head == m.head && body == m.body && foot == m.foot && side == m.side && colgroup == m.colgroup && caption == m.caption && label == m.label && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) =
    if (cs.isEmpty) {
      Success(this)
    } else {
      val c = cs.collectFirst { case x: Caption => x }
      val h = cs.collectFirst { case x: THead => x }
      val b = cs.collectFirst { case x: TBody => x }
      val f = cs.collectFirst { case x: TFoot => x }
      val s = cs.collectFirst { case x: TSide => x }
      val cg = cs.collectFirst { case x: Colgroup => x }
      val others = cs.filter {
        case _: Caption | _: THead | _: TBody | _: TFoot | _: TSide | _: Colgroup => false
        case _ => true
      }
      if (b.isEmpty || others.nonEmpty) {
        to_failure(cs)
      } else {
        Success(copy(h, b.get, f, s, cg, c, label, location = get_location(location, cs)))
      }
    }

  def width: Int = {
    List(head, body.some, foot).flatten.map(_.width).max
  }

  def height: Int = {
    List(head, body.some, foot).flatten.map(_.height).sum
  }

  def getKey: Option[String] = caption.map(_.toText.toLowerCase)

  override def isEmpty: Boolean = body.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def withCaption(p: String): Table = {
    val c = Caption(p)
    copy(caption = Some(c))
  }

  def toOption: Option[Table] = if (body.isEmpty) None else Some(this)

  def toVectorMapStringVector: Vector[VectorMap[String, String]] =
    head.map(_to_vector_map_string_vector(_)).getOrElse(_to_vector_map_string_vector())

  private def _to_vector_map_string_vector(): Vector[VectorMap[String, String]] = {
    val w = body.records.map(_.length).max
    val a = (1 to w).map(_.toString)
    _to_vector_map_string_vector(a)
  }

  private def _to_vector_map_string_vector(h: THead): Vector[VectorMap[String, String]] =
    _to_vector_map_string_vector(h.columns)

  private def _to_vector_map_string_vector(hs: Seq[String]): Vector[VectorMap[String, String]] =
    body.records.toVector.map(_to_vector_map_string(hs, _))

  private def _to_vector_map_string(hs: Seq[String], p: TRecord): VectorMap[String, String] =
    hs.zip(p.fields).toVector.foldMap {
      case (column, field) => VectorMap(column -> field.text)
    }
}
object Table {
  val empty = Table(None, TBody.empty, None, None, None, None, None)

  sealed trait Align extends Showable
  object Align {
    case object Left extends Align {
      def print = "left"
    }
    case object Center extends Align {
      def print = "center"
    }
    case object Right extends Align {
      def print = "right"
    }
  }

  def apply(h: THead, b: TBody): Table = Table(Some(h), b, None, None, None, None, None)

  def apply(b: TBody): Table = Table(None, b, None, None, None, None, None)

  def create(h: Seq[String], data: Seq[IRecord]): Table = {
    val head = THead.create(h)
    val body = TBody.create(head, data)
    Table(head, body)
  }

  // def create(h: Seq[String], data: VectorMap[String, String]): Table = {
  //   val head = THead.create(h)
  //   val body = TBody.create(head, data)
  //   Table(head, body)
  // }

  def createC(p: ITable): Consequence[Table] = Consequence(create(p))

  def create(p: ITable): Table =
    p.head.fold(_create(p.data))(_create(_, p.data))

  private def _create(head: RTable.Head, data: RTable.Data): Table = {
    val hs = head.names.map(_.text)
    val thead = THead.create(hs)
    val tbody = TBody.create(data)
    Table(thead, tbody)
  }

  private def _create(data: RTable.Data): Table = {
    val tbody = TBody.create(data)
    Table(tbody)
  }

  def create(p: LxsvSequence): Table = {
    case class Z(
      header: Vector[String] = Vector.empty,
      data: Vector[VectorMap[String, String]] = Vector.empty
    ) {
      def r = create(header, data.map(IRecord.create))

      def +(rhs: Lxsv) = {
        val h = rhs.keyNames.foldLeft(header)((z, x) =>
          if (z.contains(x))
            z
          else
            z :+ x
        )
        val d = data :+ rhs.toStringStringVectorMap
        Z(h, d)
      }
    }
    p.vector.foldLeft(Z())(_+_).r
  }

  class Builder() {
    private var _header: Vector[Inline] = Vector.empty
    private var _data: Vector[Vector[Dox]] = Vector.empty
    private var _caption: Option[Inline] = None
    private var _id: Option[String] = None

    def withCaption(p: String) = {
      _caption = Some(Dox.text(p))
      this
    }

    def withHeader(ps: Seq[Inline]) = {
      _header = ps.toVector
      this
    }

    def withHeaderString(p: Seq[String]): Builder = {
      _header = Dox.vector(p)
      this
    }

    def withHeaderString(p: String, ps: String*): Builder = withHeaderString(p +: ps)

    def append(p: String, ps: String*): Builder = appendString(p +: ps)

    def append(p: Dox, ps: Dox*): Builder = append(p +: ps)

    def append(p: I18NString, ps: I18NString*): Builder =
      append((p +: ps).map(I18NFragment.create))

    def appendString(ps: Seq[String]): Builder = append(Dox.vector(ps))

    def append(ps: Seq[Dox]): Builder = {
      _data = _data :+ ps.toVector
      this
    }

    def apply(): Table = {
      val thead: Option[THead] =
        if (_header.isEmpty) {
          None
        } else {
          val h = TR(_header.map(x => TH(x)).toList)
          Some(THead(List(h)))
        }
      val b = for (r <- _data.toList) yield {
        TR(for (f <- r.toList) yield {
          TD(f)
        })
      }
      val c = _caption.map(x => Caption(x))
      val tbody = TBody(b)
      Table(thead, tbody, None, None, None, c, _id)
    }
  }
  object Builder {
    def captionHeaderString(caption: String, header: Seq[String]): Builder =
      headerString(header).withCaption(caption)

    def headerString(p: String, ps: String*): Builder = headerString(p +: ps)

    def headerString(header: Seq[String]): Builder = {
      new Builder().withHeaderString(header)
    }

    def header(header: Seq[Inline]): Builder = {
      new Builder().withHeader(header)
    }
  }
}

trait TableCompartment extends Block {
  val records: List[TRecord]
  override val elements = records

  def width: Int = records.map(_.length) match {
    case Nil => 0
    case xs => xs.max
  }
  def height: Int = records.length

  def getText(x: Int, y: Int): String = {
    if (records.length > y) {
      val r = records(y)
      if (r.fields.length > x) {
        val f = r.fields(x)
        return f.toText
      }
    }
    return ""
  }

  def getField(x: Int, y: Int): Option[TField] = {
    if (records.length > y) {
      val r = records(y)
      if (r.fields.length > x) {
        return r.fields(x).some
      }
    }
    return None
  }

  def getContent(x: Int, y: Int): Option[List[Dox]] = {
    getField(x, y).map(_.contents)
  }

  def getData(x: Int, y: Int): String = {
    getField(x, y).map(_.toData) | ""
  }
}

trait TRecord extends Block {
  val fields: List[TField]
  def length: Int
}

case class THead(
  records: List[TRecord],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TableCompartment {
  override def equals_Value(o: Dox) = o match {
    case m: THead => records == m.records && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy(_, location = get_location(location, cs)))
  }

  def columns: List[String] = records.headOption.map(_.fields.map(_.toText)).orZero
}
object THead {
  def create(names: Seq[String]): THead = {
    val ths = names.map(TH.apply).toList
    val tr = TR(ths)
    THead(List(tr))
  }

  def data(name: String, names: String*): THead = create(name +: names)
}

case class TBody(
  records: List[TRecord],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TableCompartment {
  override def equals_Value(o: Dox) = o match {
    case m: TBody => records == m.records && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy(_, location = get_location(location, cs)))
  }

}
object TBody {
  val empty = TBody(Nil)

  def create(h: THead, data: Seq[IRecord]): TBody = create(h.columns, data)

  def create(keys: Seq[String], data: Seq[IRecord]): TBody = {
    val trs = for (row <- data) yield {
      val tds = for (k <- row.keySymbols) yield {
        row.get(k).map(x => TD(AnyUtils.toString(x))).getOrElse(TD.empty)
      }
      TR(tds)
    }
    TBody(trs.toList)
  }

  def create(data: RTable.Data): TBody = {
    val keys = data.columns.map(x => x.key.name)
    val rs = data.toRecordVector
    create(keys, rs)
  }

  // def create(keys: Seq[String], data: VectorMap[String, String]): TBody = {
  //   val trs = for (row <- data.vector) yield {
  //     val tds = for (k <- row.keySymbols) yield {
  //       row.get(k).map(x => TD(AnyUtils.toString(x))).getOrElse(TD.empty)
  //     }
  //     TR(tds)
  //   }
  //   TBody(trs.toList)
  // }
}

case class TFoot(
  records: List[TRecord],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None) extends TableCompartment {
  override def equals_Value(o: Dox) = o match {
    case m: TFoot => records == m.records && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy(_, location = get_location(location, cs)))
  }
}

case class TSide(
  top: Option[TH],
  columns: List[TH],
  bottom: Option[TH],
  location: Option[ParseLocation] = None
) extends Block {
  override val attributes = VectorMap.empty
  override val elements = List()

  override def equals_Value(o: Dox) = o match {
    case m: TSide => top == m.top && columns == m.columns && bottom == m.bottom
    case _ => false
  }
}

case class Colgroup(
  cols: List[Col],
  location: Option[ParseLocation] = None
) extends Block {
  override val attributes = VectorMap.empty
  override val elements = cols

  override def equals_Value(o: Dox) = o match {
    case m: Colgroup => cols == m.cols
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    cs.foldRight(Success(Nil): ValidationNel[String, List[Col]]) {
      case (d: Col, Success(a)) => Success(d :: a)
      case (d: Col, e: Failure[_]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }.map(copy(_, location = get_location(location, cs)))
  }
}

case class Col(
  span: Option[Int] = None,
  align: Option[Table.Align] = None,
  location: Option[ParseLocation] = None
) extends Block {
  override val attributes = VectorMap.empty

  override def showParams = ListUtils.buildTupleList(
    "span" -> span.map(AnyUtils.toString),
    "align" -> align.map(_.print)
  )

  override def equals_Value(o: Dox) = o match {
    case m: Col => span == m.span && align == m.align
    case _ => false
  }
}
object Col {
  def apply(p: Table.Align): Col = Col(align = Some(p))
}

case class TR(
  fields: List[TField],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TRecord {
  override val elements = fields

  override def equals_Value(o: Dox) = o match {
    case m: TR => fields == m.fields && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_tfield(cs).map(copy(_, location = get_location(location, cs)))
  }
  def length = fields.length
}

trait TField extends Block {
  val contents: List[Dox]
  lazy val text: String = Dox.toText(contents)
  override val elements = contents
}

case class TD(
  contents: List[Dox],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TField {
  override def equals_Value(o: Dox) = o match {
    case m: TD => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline_force(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object TD {
  val empty = TD(Nil)

  def apply(p: Dox): TD = TD(List(p))
  def apply(p: String): TD = TD(List(Dox.text(p)))
}

case class TH(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TField {
  override def equals_Value(o: Dox) = o match {
    case m: TH => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object TH {
  def apply(p: Inline): TH = TH(List(p))
  def apply(p: String): TH = TH(List(Dox.text(p)))
}

case class TTable(
  uri: String,
  params: List[String],
  caption: Option[Caption] = None,
  label: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TableBlock with TRecord { // with TField { // 2012-07-04
//  override val contents = Nil
  val contents = Nil
  override val elements = List(caption).flatten
  override def showParams = ("uri", uri) :: params.flatMap {
    _.trim.split(":").toList match {
      case Nil => Nil
      case "" :: Nil => Nil
      case x :: Nil => List(x.trim -> "true")
      case x :: xs => List(x.trim -> xs.mkString(":").trim)
    }
  }

  val fields = Nil
  def length = 0

  override def equals_Value(o: Dox) = o match {
    case m: TTable => uri == m.uri && params == m.params && caption == m.caption && label == m.label && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
//    println("TTable copyV = " + cs)
    Success(this) // currently do nothing
  }
}
case class Space(
  location: Option[ParseLocation] = None
) extends Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""
  override def show_Contents(buf: StringBuilder) {
    buf.append(" ")
  }
  override def to_Text(buf: StringBuilder) {
    buf.append(" ")
  }

  override def equals_Value(o: Dox) = o match {
    case m: Space => true
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

case class Dl(
  contents: List[(Dt, Dd)],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements: List[Dox] = contents flatMap {
    case (dt, dd) => List(dt, dd)
  }

  override protected def equals_Value(o: Dox) = o match {
    case m: Dl =>
      val r = contents == m.contents && attributes == m.attributes
      // println(s"Dl: ${r}")
      r
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_dtdd(cs).map(copy(_))
  }
}
object Dl extends Dl(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dl"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dl = Dl(ensure_dtdd(body))

  def create(p: Seq[(String, String)]): Dl = Dl(p.toList.map {
    case (t, d) => (Dt(t), Dd(d))
  })
}

case class Dt(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override protected def equals_Value(o: Dox) = o match {
    case m: Dt =>
      val r = contents.equals(m.contents) && location == m.location
      // println(s"Dt: ${r}")
      r
    case _ => false
  }

  override def copyV(cs: List[Dox]) = to_inline(cs).map(copy(_))
}
object Dt extends Dt(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dt"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dt = Dt(ensure_inline(body))

  def apply(p: String): Dt = Dt(Dox.text(p))

  def apply(p: Inline): Dt = Dt(List(p))

  def unapply(x: XNode): Option[Dt] = x.label.toLowerCase match {
    case "dt" => Some(PureParser.buildDt(x))
    case _ => None
  }

  def make(p: Dox): Dt = p match {
    case m: Inline => apply(m)
    case m => apply(Error(s"Dt: Illegal Dt content = $m"))
  }
}

case class Dd(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Dd => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_))
  }
}
object Dd extends Dd(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dd"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dd =
    Dd(ensure_inline(body))

  def unapply(x: XNode): Option[Dd] = x.label.toLowerCase match {
    case "dd" => Some(PureParser.buildDd(x))
    case _ => None
  }

  def create(p: I18NString): Dd = Dd(List(I18NFragment.create(p)))
}

case class Fragment(
  contents: List[Dox],
  location: Option[ParseLocation] = None
) extends Dox with Block with Inline with ListContent {
  override def isVisialBlock: Boolean = false
  def attributes: VectorMap[String, String] = VectorMap.empty
  override val elements = contents
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""

  override protected def show_Contents(buf: StringBuilder) {
    // println(s"Fragment#show_Contents: ${showContentsElements}")
    showContentsElements.foreach(_.toString(buf))
  }

  override def equals_Value(o: Dox) = o match {
    case m: Fragment => contents == m.contents
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(normalize_fragment(cs)))
//    Success(copy(contents ::: normalize_fragment(cs)))
  }

  def toInlines: List[Inline] = Dox.toInlineContents(contents)

  def append(p: String): Fragment = {
    contents.lastOption.map {
      case m: Text => copy(contents = contents.init :+ m.append(p))
      case m => append(Text(p))
    }.getOrElse(append(Text(p)))
  }

  def append(p: Dox): Fragment = copy(contents = contents :+ p)
}
object Fragment extends DoxFactory {
  val label = "fragment"

  val empty = new Fragment(Nil)

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Fragment =
    _create(body)

  def apply(p: Dox, ps: Dox*): Fragment = _create((p +: ps))

  def apply(p: String, ps: String*): Fragment = apply(Text((p +: ps).mkString))

  def apply(ps: Seq[Dox]): Fragment = _create(ps)

  private def _create(ps: Seq[Dox]): Fragment = {
    require (ps.forall(_ != null), "contents should not be null")
    ps.flatMap(_normalize).toList match {
      case Nil => empty
      case xs => new Fragment(xs)
    }
  }

  private def _normalize(p: Dox): List[Dox] = p match {
    case m: Fragment => m.contents.flatMap(_normalize)
    case m => List(m)
  }
}
