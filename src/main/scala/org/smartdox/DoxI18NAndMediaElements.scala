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


case class I18NFragment(
  contents: I18NContainer[List[Dox]],
  location: Option[ParseLocation] = None
) extends Dox with Block with Inline with ListContent {
  override def isVisialBlock: Boolean = false
  def attributes: VectorMap[String, String] = VectorMap.empty

  override def equals_Value(o: Dox) = o match {
    case m: I18NFragment => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def showTerm = "i18n"

  override def isOpenClose = false

  override def show_Contents(buf: StringBuilder) = print_Contents(buf)

  override def printDox(buf: StringBuilder): Unit =
    contents.getIfNoLocale match {
      case Some(s) => _print_contents(buf, s)
      case None => super.printDox(buf)
    }

  override def print_Contents(buf: StringBuilder): Unit =
    for ((locale, xs) <- contents.localeVector) {
      buf.append("<")
      buf.append(locale)
      buf.append(">")
      _print_contents(buf, xs)
      buf.append("</")
      buf.append(locale)
      buf.append(">")
    }

  private def _print_contents(buf: StringBuilder, xs: Seq[Dox]): Unit =
    for (x <- xs) {
      x.printDox(buf)
    }

  def isSimple: Boolean = contents.getIfNoLocale.isDefined

  def distill(locale: Option[Locale]): List[Dox] =
    locale.fold(contents.default)(distill)

  def distill(locale: Locale): List[Dox] = contents.apply(locale)

  def distillInline(locale: Locale): List[Inline] =
    distill(locale).flatMap(Dox.distillInline)

  def distillI18NFragment(locale: Option[Locale]): I18NFragment =
    locale.fold(distillI18NFragmentDefault)(distillI18NFragment)

  def distillI18NFragment(locale: Locale): I18NFragment =
    I18NFragment(I18NContainer.make(distill(locale)))

  def distillI18NFragmentDefault: I18NFragment =
    I18NFragment(I18NContainer.make(contents.default))

  def distillString(locale: Locale): String = Dox.toPlainText(distillInline(locale))

  def distillStringDefault: String = Dox.toPlainText(contents.default)

  def distillInlineContentsDefault: InlineContents = contents.default.map {
    case m: Inline => m
  }

  def toI18NString: I18NString = I18NString(
    Dox.toPlainText(contents.c),
    Dox.toPlainText(contents.en),
    Dox.toPlainText(contents.ja),
    contents.map.mapValues(Dox.toPlainText)
  )

  def makeInlines: List[Inline] =
    contents.getIfNoLocale match {
      case Some(s) => Dox.toInlineContents(s)
      case None => _make_inlines
    }

  private def _make_inlines = {
    case class Z(ls: Map[Locale, List[Inline]] = Map.empty) {
      def r = {
        ls.toList map {
          case (l, xs) => Span.create(l, xs)
        }
      }

      def +(rhs: (Locale, List[Dox])) = {
        val v = Dox.toInlineContents(rhs._2)
        copy(ls + (rhs._1 -> v))
      }
    }
    contents.localeVector.foldLeft(Z())(_+_).r
  }

  def toInlines: List[Inline] =
    contents.getIfNoLocale match {
      case Some(s) => Dox.toInlineContents(s)
      case None => List(this)
    }

  def makeParagraphs: List[Paragraph] = contents.getIfNoLocale match {
    case Some(s) => Dox.toParagraphs(s)
    case None => _make_paragraphs
  }

  private def _make_paragraphs: List[Paragraph] = {
    case class Z(ls: Map[Locale, List[Dox]] = Map.empty) {
      def r = {
        ls.toList map {
          case (l, xs) => Paragraph.create(l, xs)
        }
      }

      def +(rhs: (Locale, List[Dox])) = {
        copy(ls + rhs)
      }
    }
    contents.localeVector.foldLeft(Z())(_+_).r
  }

  def +:(p: Char): I18NFragment = copy(contents = contents.mapValues {
    case Nil => List(Text(p))
    case x :: xs => x match {
      case m: Text => m.prepend(p) :: xs
      case m => Text(p) :: x :: xs
    }
  })

  def +:(p: String): I18NFragment = copy(contents = contents.mapValues {
    case Nil => List(Text(p))
    case x :: xs => x match {
      case m: Text => m.prepend(p) :: xs
      case m => Text(p) :: x :: xs
    }
  })

  def mapValues(f: List[Dox] => List[Dox]): I18NFragment =
    I18NFragment(contents.mapValues(f))

  def trimSingleLine: I18NFragment = {
    val lv = contents.localeVector
    val r = lv.map(_trim_single_line)
    copy(contents = I18NContainer.create(r))
  }

  private def _trim_single_line(p: (Locale, List[Dox])): (Locale, List[Dox]) = {
    val (k, vs) = p
    case class Z(
      xs: Vector[Dox] = Vector.empty,
      isdone: Boolean = false
    ) {
      def r = xs.toList

      def +(rhs: Dox) =
        if (isdone)
          this
        else
          rhs match {
            case m: Text =>
              val s = m.contents
              if (s.contains("\n") || s.contains("\r"))
                copy(xs = xs :+ Text(DoxUtils.trimSingleLine(s)), isdone = true)
              else
                copy(xs = xs :+ m)
            case m: Paragraph =>
              if (xs.isEmpty)
                copy(xs = xs :+ rhs, isdone = true)
              else
                copy(isdone = true)
            case m => copy(xs = xs :+ rhs)
          }
    }
    val r = vs.foldLeft(Z())(_+_).r
    // val r = _find_text(v) match {
    //   case Some(s) => List(Text(DoxUtils.trimSingleLine(s.contents)))
    //   case None => Nil
    // }
    k -> r
  }

  private def _find_text(ps: List[Dox]): Option[Text] = {
    def _go_(x: Dox): Option[Text] = x match {
      case m: Text => if (m.isBlank) None else Some(m)
      case m => x.children.toStream.flatMap(_go_).headOption
    }
    ps.toStream.flatMap(_go_).headOption
  }

  def toVectorMapStringVector: VectorMap[Locale, Vector[String]] = {
    val a: Vector[(Locale, List[Dox])] = contents.localeVector
    val b = a.map {
      case (l, vs) => l -> vs.flatMap(toStringVector).toVector
    }
    VectorMap(b)
  }

  def toVectorMapString: VectorMap[Locale, String] =
    toVectorMapStringVector.mapValues(_.mkString)

  def toStringVector(p: Dox): Vector[String] = p match {
    case m: Value.Multiple => m.vs
    case m: Value.Single => Vector(m.v)
    case m => Vector(m.toText)
  }
}
object I18NFragment {
  def create(ps: Seq[Dox]): I18NFragment = ps.toList match {
    case Nil => _create_distill(Nil)
    case x :: Nil => x match {
      case m: I18NFragment => m
      case _ => _create_distill(ps)
    }
    case xs => _create_distill(xs)
  }

  def create(p: Dox): I18NFragment = p match {
    case m: I18NFragment => m
    case m: Fragment => create(m.contents)
    case m => create(List(m))
  }

  case class Z(
    xs: Vector[Dox] = Vector.empty,
    ls: Map[Locale, Vector[Dox]] = Map.empty
  ) {
    def r = if (ls.isEmpty)
      I18NFragment(I18NContainer.make(xs.toList))
    else
      I18NFragment(I18NContainer.createSeq(ls))

    def +(rhs: Dox) = rhs.getLanguage match {
      case Some(l) =>
        ls.get(l) match {
          case Some(v) =>
            copy(ls = ls + (l -> (v :+ rhs)))
          case None =>
            copy(ls = ls + (l -> (xs :+ rhs)))
        }
      case None =>
        val a = _make_locales(rhs)
        a match {
          case Some(lrs) =>
            lrs.foldLeft(ZZ(Z.this))(_+_).r
          case None =>
            copy(
              xs = xs :+ rhs,
              ls = ls.keys.foldLeft(ls)((z, x) =>
                ls |+| Map(x -> Vector(rhs)))
            )
        }
    }
  }

  case class ZZ(z: Z) {
    def r = z

    def +(rhs: (Locale, Vector[Dox])) = {
      val (locale, rs) = rhs
      z.ls.get(locale) match {
        case Some(v) => copy(z = z.copy(ls = z.ls + (locale -> (v ++ rs))))
        case None => copy(z = z.copy(ls = z.ls + (locale -> (z.xs ++ rs))))
      }
    }
  }

  private def _create_distill(ps: Seq[Dox]): I18NFragment = {
    ps.foldLeft(Z())(_+_).r
  }

  private def _make_locales(p: Dox): Option[Vector[(Locale, Vector[Dox])]] = {
    val a = _create_distill(p.elements)
    if (a.isSimple) {
      None
    } else {
      val b: Vector[(Locale, List[Dox])] = a.contents.localeVector
      val c = b.map {
        case (k, vs) => k -> Vector(p.copyV(vs).toOption.get)
      }
      Some(c)
    }
  }

  def createList(ps: List[List[Dox]]): I18NFragment = {
    val a = ps.map(create)
    val b: List[Vector[(Locale, List[Dox])]] = a.map(_.contents.localeVector)
    val locales: List[Locale] = b.flatMap(_.toList).map(_._1).distinct
    case class Z(xs: Vector[(Locale, List[Dox])] = Vector.empty) {
      def r = I18NFragment.createDox(xs)

      def +(rhs: Locale) = {
        val x: List[Dox] = b.flatMap(_.filter(_._1 == rhs).flatMap(_._2))
        copy(xs = xs :+ (rhs -> x))
      }
    }
    locales.foldLeft(Z())(_+_).r
  }

  def create(p: String): I18NFragment = create(List(Text(p)))

  def create(p: I18NString): I18NFragment = {
    val a = p.localeVector.map {
      case (k, v) => k -> List(Text(v))
    }
    I18NFragment(I18NContainer.create(a))
  }

  def create[A <: Dox](p: I18NHangar[A]): I18NFragment = {
    I18NFragment(I18NContainer.createList(p))
  }

  def create[A <: Dox](p: I18NContainer[A]): I18NFragment =
    I18NFragment(p.mapValues(x => List(x)))

  def createString(p: Map[Locale, String]): I18NFragment = {
    val a = p.mapValues(x => List(Text(x)))
    I18NFragment(I18NContainer.createSeq(a))
  }

  def createDox(p: Seq[(Locale, Seq[Dox])]) =
    I18NFragment(I18NContainer.createSeq(p))

  def enja(en: String, ja: String) = I18NFragment(
    I18NContainer.enja(List(Text(en)), List(Text(ja)))
  )

  def parseInclusion(p: String): I18NFragment =
    Dox2Parser.parseI18NFragmentC(p).foldConclusion(c => I18NFragment.create(c.message))

  def parseOptionInclusion(p: String): Option[I18NFragment] =
    Dox2Parser.parseI18NFragmentOptionC(p).foldConclusion(c => Some(I18NFragment.create(c.message)))

  def getC(name: String, p: XNode): Consequence[Option[I18NFragment]] =
    for {
      a <- XmlUtils.getI18NContainerC(p, name)
      r <- a.traverse(x => _make_i18nfragment(x))
    } yield r

  private def _make_i18nfragment(p: I18NContainer[List[XNode]]): Consequence[I18NFragment] =
  Consequence {
    val a = p.mapValues(xs => xs.map(PureParser.build))
    I18NFragment(a)
  }

  def buildDescription(p: Block): I18NFragment = {
    val commons = p.elements.takeWhile {
      case m: Section => false
      case _ => true
    }
    val ss = p.sections
    ss match {
      case Nil => create(p.elements)
      case xs =>
        val a = xs.map(x => (Locale.forLanguageTag(x.nameForModel), commons ++ x.contents))
        createDox(a)
    }
  }

  def buildValueOrValues(p: Block): I18NFragment = {
    val ss = p.sections
    ss match {
      case Nil => create(p.elements)
      case xs =>
        val a = xs.map(x => (Locale.forLanguageTag(x.nameForModel), List(Dox.toValueOrValues(x))))
        createDox(a)
    }
  }
}

case class Caption(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Caption => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_))
  }
}
object Caption {
  def apply(p: Inline): Caption = Caption(List(p))
  def apply(p: String): Caption = Caption(List(Dox.text(p)))
}

// 2011-12-31
case class Figure(
  img: Img,
  caption: Figcaption,
  label: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = List(img, caption)
  override def showParams = List("id" -> label).flatMap(_.sequence)

  override def equals_Value(o: Dox) = o match {
    case m: Figure => img == m.img && caption == m.caption && label == m.label && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_figure(cs).map { case (i, c) =>
      copy(i | img, c | caption, label)
    }
  }
}
object Figure {
  def apply(img: Img, name: String): Figure = Figure(img, Figcaption(name))

  def apply(img: Img, caption: InlineContents): Figure =
    Figure(img, Figcaption(caption))
}

case class Figcaption(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Figcaption => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_))
  }

  def nonEmpty = contents.nonEmpty
}
object Figcaption {
  def apply(name: String): Figcaption = Figcaption(List(Text(name)))
}

case class EmptyLine(
  location: Option[ParseLocation] = None
) extends Block {
  def attributes: VectorMap[String, String] = VectorMap.empty

  override def equals_Value(o: Dox) = o match {
    case m: EmptyLine => true
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2011-01-01
case class Newline(
  location: Option[ParseLocation] = None
) extends Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty

  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""
  override def show_Contents(buf: StringBuilder) {
    buf.append("\n")
  }
  override def to_Text(buf: StringBuilder) {
    buf.append("\n")
  }
  override def equals_Value(o: Dox) = o match {
    case m: Newline => true
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

trait Img extends Inline {
  val src: URI
  val alt: Option[String]
  def attributes: VectorMap[String, String]
  override val elements = Nil
  override def showTerm = "img"
  // override def showParams = List(
  //   "src" -> src.toASCIIString(),
  //   "width" -> "640" // TODO
  // )
  override def showParams = {
    val a = List(
      "src" -> src.toASCIIString()
    )
    val b = ListUtils.buildTupleList("alt" -> alt)
    val c = attributes.toList
    a ++ b ++ c
  }
}

trait EmbeddedImg extends Img {
  val contents: String
  val params: List[String]
}

case class DotImg(
  src: URI,
  contents: String,
  params: List[String] = Nil,
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends EmbeddedImg {
  override def equals_Value(o: Dox) = o match {
    case m: DotImg => src == m.src && contents == m.contents && params == m.params && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

case class DitaaImg(
  src: URI,
  contents: String,
  params: List[String] = Nil,
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends EmbeddedImg {
  override def equals_Value(o: Dox) = o match {
    case m: DitaaImg => src == m.src && contents == m.contents && params == m.params && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2011-01-16
case class Html5(
  name: String,
  attributes: VectorMap[String, String],
  contents: List[Dox],
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents
  override def showTerm = name
  override def showParams = Nil

  override def equals_Value(o: Dox) = o match {
    case m: Html5 => name == m.name && attributes == m.attributes && contents == m.contents
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(name, attributes, cs))
  }
  override protected def print_Open(buf: StringBuilder): Unit = {
    print_open_tag(buf, name, attributes)
  }

  override protected def print_Close(buf: StringBuilder): Unit = {
    print_close_tag(buf, name)
  }
}

// 2025-10-17
case class Html5Inline(
  name: String,
  attributes: VectorMap[String, String],
  contents: List[Dox],
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = name
  override def showParams = Nil

  override def equals_Value(o: Dox) = o match {
    case m: Html5Inline => name == m.name && attributes == m.attributes && contents == m.contents
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(name, attributes, cs))
  }
  override protected def print_Open(buf: StringBuilder): Unit = {
    print_open_tag(buf, name, attributes)
  }

  override protected def print_Close(buf: StringBuilder): Unit = {
    print_close_tag(buf, name)
  }
}

// 2011-01-17
