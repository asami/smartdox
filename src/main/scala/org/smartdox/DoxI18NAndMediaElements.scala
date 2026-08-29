package org.smartdox

/*
 * @since   Aug. 19, 2026
 * @version Aug. 29, 2026
 * @author  ASAMI, Tomoharu
 */

import java.net.URI
import scalaz.{Value => _, _}, Scalaz._, WriterT._, Show._, Validation._
import org.goldenport.collection.VectorMap
import org.goldenport.parser.ParseLocation
import org.goldenport.util.ListUtils

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
