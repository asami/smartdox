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


case class Program private(
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None,
  callouts: Dox.Callouts = Dox.Callouts.empty
) extends Block with Preserve {
  override val elements = List(new Text(contents))
  override def showTerm = "pre"
  override def showParams = attributes.list ++ List("class" -> "program")

  def kind: Option[String] = attributes.get("kind")
  def caption: Option[String] = attributes.get("caption")

  override protected def print_Open(buf: StringBuilder): Unit = {
    print_open_tag(buf, "program", attributes)
  }

  override protected def print_Close(buf: StringBuilder): Unit = {
    print_close_tag(buf, "program")
  }

  override def equals_Value(o: Dox) = o match {
    case m: Program => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs).map(_ => this)
  }
}
object Program {
  def create(p: String): Program =
    create(p, VectorMap.empty[String, String], None)

  def create(p: String, attrs: Map[String, String]): Program =
    create(p, VectorMap(attrs), None)

  def create(p: String, attrs: Seq[(String, String)]): Program =
    create(p, VectorMap(attrs), None)

  def create(p: String, attrs: Map[String, String], location: Option[ParseLocation]): Program = {
    val (s, cs) = _extract_callouts(_normalize(p))
    Program(s, VectorMap(attrs), location, cs)
  }

  def create(p: String, attr: (String, String), attrs: (String, String)*): Program =
    create(p, VectorMap(attr +: attrs))

  def create(p: Seq[String], attr: (String, String), attrs: (String, String)*): Program =
    create(p.mkString("\n"), VectorMap(attr +: attrs))


  def create(p: String, kind: Option[String], caption: Option[String]): Program =
    create(p, ListUtils.buildTupleList("kind" -> kind, "caption" -> caption))

  private def _normalize(p: String): String =
    if (p.endsWith("\n") || p.endsWith("\r"))
      p
    else
      p + "\n"

  private def _extract_callouts(p: String): (String, Dox.Callouts) = {
    val Explicit = """<(\d+)>\s*(.*)""".r
    val Auto = """<>\s*(.*)""".r
    val callouts = new Dox.Callouts.Builder()
    val taken = scala.collection.mutable.Set[Int]()
    val lines = p.linesIterator.toVector
    val result = new StringBuilder
    var auto = 1

    for (line <- lines) {
      val commentIndex = line.indexOf("//")
      if (commentIndex >= 0) {
        val code = line.substring(0, commentIndex)
        val comment = line.substring(commentIndex + 2).trim
        comment match {
          case Explicit(n, desc) =>
            val num = n.toInt
            taken += num
            callouts.add(num, desc.trim)
            result.append(code + s"<$num>")
          case Auto(desc) =>
            while (taken.contains(auto)) auto += 1
            val num = auto
            taken += num
            callouts.add(num, desc.trim)
            result.append(code + s"<$num>")
          case _ =>
            result.append(line)
        }
      } else {
        result.append(line)
      }
      result.append("\n")
    }

    (_normalize(result.toString), callouts.build())
  }
}

case class Console(
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block with Preserve {
  override val elements = List(new Text(contents))
  override def showTerm = "pre"
  override def showParams = attributes.list ++ List("class" -> "console")

  override def equals_Value(o: Dox) = o match {
    case m: Console => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs).map(_ => this)
  }
}

// 2011-01-18
case class SmartDoc(
  name: String,
  attributes: VectorMap[String, String],
  contents: List[Dox],
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents
  override def showTerm = name
  override def showParams = attributes.list

  override def equals_Value(o: Dox) = o match {
    case m: SmartDoc => name == m.name && attributes == m.attributes && contents == m.contents
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(name, attributes, cs))
  }
}

// 2011-01-20
case class SmCsvImg(
  src: URI,
  contents: String,
  params: List[String] = Nil,
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends EmbeddedImg {
  override def equals_Value(o: Dox) = o match {
    case m: SmCsvImg => src == m.src && contents == m.contents && params == m.params && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2012-02-15
object EmptyDox extends Dox with DoxFactory with Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty
  def location: Option[ParseLocation] = None

  val label = "empty"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dox = EmptyDox

  override def toString(buf: StringBuilder, maxlength: Option[Int] = None) {
  }

  override def equals_Value(o: Dox) = o eq this
}

// 2012-04-24
case class Tt(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Tt => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, contents)))
  }
}

object Tt extends Tt(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "tt"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Tt =
    Tt(ensure_inline(body))

  def apply(element: Inline) = new Tt(List(element))
}

// 2012-06-05
case class Span(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "span"

  override def children = contents

  override def equals_Value(o: Dox) = o match {
    case m: Span => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map { x =>
      val r = copy(x, location = get_location(location, contents))
      // println(s"Span#copyV: $r")
      r
    }
  }
}

object Span extends Span(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "span"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Span =
    Span(ensure_inline(body), attrs)

  def apply(element: Inline) = new Span(List(element))

  def create(locale: Locale, p: String): Span = Span(
    List(Text(p)),
    VectorMap("lang" -> locale.toLanguageTag)
  )

  def create(locale: Locale, p: List[Inline]): Span = Span(
    p,
    VectorMap("lang" -> locale.toLanguageTag)
  )

  def createEn(p: String) = create(LocaleUtils.en, p)
  def createJa(p: String) = create(LocaleUtils.ja, p)

  def build(elem: XNode): Span = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Span(cs, attrs)
  }
}

// 2025-03-03
case class Dfn(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "dfn"
  override def showParams = ListUtils.buildTupleList("id" -> attributes.get("id"))

  override def equals_Value(o: Dox) = o match {
    case m: Dfn => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, contents)))
  }
}

object Dfn extends Dfn(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dfn"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dfn =
    Dfn(ensure_inline(body), attrs)

  def apply(element: Inline) = new Dfn(List(element))
}

// 2026-08-18
case class Term(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "term"

  override def equals_Value(o: Dox) = o match {
    case m: Term => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object Term extends Term(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "term"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Term =
    Term(ensure_inline(body), attrs)

  def apply(element: Inline) = new Term(List(element))
}

// 2026-08-18
case class NoTerm(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "noterm"

  override def equals_Value(o: Dox) = o match {
    case m: NoTerm => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object NoTerm extends NoTerm(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "noterm"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): NoTerm =
    NoTerm(ensure_inline(body), attrs)

  def apply(element: Inline) = new NoTerm(List(element))
}

// 2025-03-03
case class Abbr(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "abbr"

  override def equals_Value(o: Dox) = o match {
    case m: Abbr => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, contents)))
  }
}

object Abbr extends Abbr(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "abbr"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Abbr =
    Abbr(ensure_inline(body))

  def apply(element: Inline) = new Abbr(List(element))
}

// 2012-11-23
case class IncludeDoc(filename: String) extends Block {
  def attributes: VectorMap[String, String] = VectorMap.empty
  def location: Option[ParseLocation] = None

  override def showParams = List(("filename", filename))

  override def equals_Value(o: Dox) = o match {
    case m: IncludeDoc => filename == m.filename
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
//    println("IncludeDoc#copyV: " + cs)
    Div(cs).success
  }
}

// 2020-09-21
case class BinaryImg(
  name: String,
  mime: MimeType,
  chunk: ChunkBag,
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Img {
  val src: URI = {
    import org.apache.commons.codec.binary.Base64
    val binary = Base64.encodeBase64String(chunk.toByteArray)
//    val a = s"""data:${name}/${suffix};base64,${binary}"""
    val a = s"""data:${mime.name};base64,${binary}"""
    new URI(a)
  }

  override def equals_Value(o: Dox) = o match {
    case m: BinaryImg => name == m.name && mime == m.mime && chunk == m.chunk && attributes == m.attributes
    case _ => false
  }
}
object BinaryImg {
//   def apply(name: String, mime: MimeType, chunk: ChunkBag): BinaryImg = {
// //    val suffix = MimeType.getSuffix(mime).getOrElse(RAISE.invalidArgumentFault(s"Not image: ${mime}"))
//     BinaryImg(name, mime, chunk)
//   }
}

// 2020-09-22
case class UnresolvedLink(
  contents: List[Inline],
  data: Any,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "a"
  override def showParams = List("href" -> AnyUtils.toString(data))

  override def equals_Value(o: Dox) = o match {
    case m: UnresolvedLink => contents == m.contents && data == m.data && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, data, attributes, location = get_location(location, cs)))
  }
}
object UnresolvedLink {
  def apply(label: String, data: Any): UnresolvedLink =
    UnresolvedLink(List(Dox.text(label)), data)
}

// 2025-07-17
case class Include(
  directive: BlockMacro.Include,
  location: Option[ParseLocation] = None
) extends Directive {
  def attributes: VectorMap[String, String] = VectorMap.empty
  override def isVisialBlock: Boolean = false
  override def equals_Value(o: Dox) = o == this

  def target = directive.target
  def parameters = directive.parameters
}
object Include {
  def create(p: String): Include = Include(BlockMacro.Include(p))
  def create(p: URI): Include = Include(BlockMacro.Include.create(p))
}

// 2025-07-17
case class Error(
  conclusion: Conclusion,
  location: Option[ParseLocation] = None
) extends Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty
  override def equals_Value(o: Dox) = o == this
  override def isOpenClose = false

  override def show_Contents(buf: StringBuilder): Unit = {
    buf.append(conclusion.message)
  }

  def message = conclusion.message
}
object Error {
  def apply(msg: String): Error = Error(Conclusion.syntaxErrorFault(msg))
}

case class DiagnosticBlock(
  title: String,
  message: String,
  sourceLabel: Option[String] = None,
  source: Option[String] = None,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = Nil
  def attributes: VectorMap[String, String] = VectorMap.empty
  override def showTerm = "diagnostic"
  override def isOpenClose = false

  override protected def equals_Value(o: Dox) = o == this

  override protected def show_Contents(buf: StringBuilder) {
    _append_contents(buf)
  }

  override protected def print_Contents(buf: StringBuilder) {
    _append_contents(buf)
  }

  override protected def to_Text(buf: StringBuilder) {
    _append_contents(buf)
  }

  override protected def to_Plain_Text(buf: StringBuilder) {
    _append_contents(buf)
  }

  private def _append_contents(buf: StringBuilder) {
    buf.append(title)
    buf.append("\n")
    buf.append(message)
    buf.append("\n")
    for (label <- sourceLabel; value <- source) {
      buf.append(label)
      buf.append(":\n")
      buf.append(value)
      if (!value.endsWith("\n"))
        buf.append("\n")
    }
  }
}
object DiagnosticBlock {
  def error(title: String, message: String): DiagnosticBlock =
    DiagnosticBlock(title, message)

  def error(title: String, message: String, sourceLabel: String, source: String): DiagnosticBlock =
    DiagnosticBlock(title, message, Some(sourceLabel), Some(source))
}

// 2025-09-01
sealed trait Value extends Inline {
  def toI18NHangar: I18NHangar[String]
  def values: Vector[String]
//  def toInlineContentsList: List[InlineContents]
}
object Value {
  val empty = Single("")

  case class Single(v: String) extends Value {
    def attributes: VectorMap[String, String] = VectorMap.empty
    def location: Option[ParseLocation] = None
    override def equals_Value(o: Dox) = o == this

//    def toInlineContentsList: List[InlineContents] = List(List(this))

    override protected def to_Text(buf: StringBuilder) {
      buf.append(v)
    }

    override protected def to_Plain_Text(buf: StringBuilder) {
      buf.append(v)
    }

    override def to_Data(buf: StringBuilder) {
      buf.append(v)
    }

    def toI18NHangar: I18NHangar[String] = I18NHangar.createCommons(v)
    def values: Vector[String] = Vector(v)
  }

  case class Multiple(vs: Vector[String]) extends Value {
    def attributes: VectorMap[String, String] = VectorMap.empty
    def location: Option[ParseLocation] = None
    override def equals_Value(o: Dox) = o == this

//    def toInlineContentsList: List[InlineContents] = vs.map(x => List(Single(x))).toList

    private def _values = vs.mkString(", ")

    override protected def to_Text(buf: StringBuilder) {
      buf.append(_values)
    }

    override protected def to_Plain_Text(buf: StringBuilder) {
      buf.append(_values)
    }

    override def to_Data(buf: StringBuilder) {
      buf.append(_values)
    }

    def toI18NHangar: I18NHangar[String] = I18NHangar.createCommons(vs)
    def values: Vector[String] = vs
  }

  case class I18N(hangar: I18NHangar[String]) extends Value {
    def attributes: VectorMap[String, String] = VectorMap.empty
    def location: Option[ParseLocation] = None
    override def equals_Value(o: Dox) = o == this

    def distill(locale: Locale): Option[Value] = {
      hangar.get(locale).map(Value.apply)
    }

    def toI18NHangar: I18NHangar[String] = hangar
    def values: Vector[String] = hangar.valueVector // XXX
  }

  def apply(p: String): Value = Single(p)

  def apply(ps: Seq[String]): Value = Multiple(ps.toVector)

  def create(ps: Seq[Dox]): Value = {
    case class Z(
      map: Map[Locale, Vector[String]] = Map.empty,
      common: Vector[String] = Vector.empty
    ) {
      def r =
        if (map.isEmpty)
          Single(common.mkString)
        else if (common.isEmpty)
          I18N(I18NHangar.createOne(map.mapValues(_.mkString)))
        else
          I18N(I18NHangar.createOne(map.mapValues(_.mkString), common.mkString))

      def +(rhs: Dox) = rhs match {
        case m: I18NFragment => copy(map |+| m.toVectorMapStringVector)
        case m: Value.I18N => copy(
          map |+| m.hangar.map,
          common ++ m.hangar.commons
        )
        case m => m.getLanguage match {
          case Some(l) => copy(map |+| Map(l -> Vector(m.toPlainText)))
          case None => copy(common = common :+ m.toPlainText)
        }
      }
    }
    ps.foldLeft(Z())(_+_).r
  }

  def createMulti(ps: Seq[Seq[Dox]]): Value = {
    val a = ps.map(create)
    case class Z(builder: I18NHangar.Builder[String] = I18NHangar.Builder()) {
      def r = I18N(builder.build())

      def +(rhs: Value) = rhs match {
        case m: I18NFragment => copy(builder.add(m.toVectorMapString))
        case m: Value.I18N => copy(builder.add(m.hangar))
        case m => m.getLanguage match {
          case Some(l) => copy(builder.add(Map(l -> m.toPlainText)))
          case None => copy(builder.add(m.toPlainText))
        }
      }
    }
    a.foldLeft(Z())(_+_).r
  }

  def buildValueOrValuesI18N(p: Block): Value = {
    val ss = p.sections
    ss match {
      case Nil => Dox.toValueOrValues(p.elements)
      case xs =>
        // TODO common
        val a = xs.map(x => (Locale.forLanguageTag(x.nameForModel), Dox.toValueOrValues(x).values))
        val b = I18NHangar.create(a.toMap)
        I18N(b)
    }
  }
}

// 2025-11-05
case class HorizontalRule(
  location: Option[ParseLocation] = None
) extends Block {
  override def showTerm = "hr"
  def attributes: VectorMap[String, String] = VectorMap.empty
  override def equals_Value(o: Dox) = o == this
  override def isOpenClose = true

  override def show_Contents(buf: StringBuilder): Unit = {
  }
}

// 2025-11-05
sealed trait Quotation extends Block {
  override def showTerm = "blockquote"
}
object Quotation {
  case class SimpleQuote(
    contents: List[Dox],
    location: Option[ParseLocation] = None
  ) extends Quotation {
    override val elements = contents

    def attributes: VectorMap[String, String] = VectorMap.empty

    override def equals_Value(o: Dox) = o match {
      case m: SimpleQuote => contents.equals(m.contents)
      case _ => false
    }

    override def copyV(cs: List[Dox]) = {
      val r = copy(cs, location = get_location(location, cs))
      // println(s"Paragraph#copyV: $cs => $r")
      Success(r)
    }
  }

  case class BlockQuote(
    contents: List[Dox],
    author: Option[InlineContents],
    source: Option[InlineContents],
    location: Option[ParseLocation] = None
  ) extends Quotation {
    override val elements = contents

    def attributes: VectorMap[String, String] = VectorMap.empty

    override def equals_Value(o: Dox) = o match {
      case m: BlockQuote => contents.equals(m.contents) && author.equals(m.author) && source.equals(m.source)
      case _ => false
    }

    override def copyV(cs: List[Dox]) = {
      val r = copy(cs, location = get_location(location, cs))
      // println(s"Paragraph#copyV: $cs => $r")
      Success(r)
    }
  }
}

// 2025-11-05
case class Admonition(
  contents: List[Dox],
  kind: Admonition.Kind,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents
  override def showTerm = kind.name

  def attributes: VectorMap[String, String] = VectorMap.empty

  override def equals_Value(o: Dox) = o match {
    case m: Admonition => kind == m.kind && contents.equals(m.contents)
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    val r = copy(cs, location = get_location(location, cs))
    // println(s"Paragraph#copyV: $cs => $r")
    Success(r)
  }
}
object Admonition {
  sealed trait Kind extends NamedValueInstance {
  }
  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(Note, Tip, Important, Caution, Warning)

    case object Note extends Kind {
      val name = "note"
    }
    case object Tip extends Kind {
      val name = "tip"
    }
    case object Important extends Kind {
      val name = "important"
    }
    case object Caution extends Kind {
      val name = "caution"
    }
    case object Warning extends Kind {
      val name = "warning"
    }
  }
}

// // 2025-10-16
// case class XmlElement(
//   name: String,
//   contents: List[Dox],
//   attributes: VectorMap[String, String] = VectorMap.empty,
//   location: Option[ParseLocation] = None
// ) extends Inline {
//   override val elements = contents
//   override def showTerm = name

//   override def equals_Value(o: Dox) = o match {
//     case m: XmlElement => name == m.name && contents == m.contents && attributes == m.attributes
//     case _ => false
//   }
// }

// object XmlElement {
// }

// 2025-09-09
// case class Verbatim(
//   contents: List[Inline],
//   attributes: VectorMap[String, String] = VectorMap.empty,
//   location: Option[ParseLocation] = None
// ) extends Inline {
//   override val elements = contents

//   override def equals_Value(o: Dox) = o match {
//     case m: Verbatim => contents == m.contents && attributes == m.attributes
//     case _ => false
//   }

//   override def copyV(cs: List[Dox]) = {
//     to_inline(cs).map(copy(_, location = get_location(location, cs)))
//   }
// }

// object Verbatim extends Verbatim(Nil, VectorMap.empty, None) with DoxFactory {
//   val label = "verbatim"

//   def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Verbatim =
//     Verbatim(body.toList)

//   def apply(element: Inline) = new Verbatim(List(element))

//   def build(elem: XNode): Verbatim = {
//     val cs = PureParser.buildInline(elem)
//     val attrs = PureParser.getAttributes(elem)
//     Verbatim(cs, attrs)
//   }
// }
