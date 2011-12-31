package org.smartdox

import scalaz._
import Scalaz._
import java.net.URI

/*
 * derived from SNode.java since Sep. 17, 2006
 * derived from SDoc.scala since Sep.  1, 2008
 *
 * @since   Dec. 24, 2011
 * @version Dec. 31, 2011
 * @author  ASAMI, Tomoharu
 */
trait Dox {
  val elements: List[Dox] = Nil
  def showTerm = getClass.getSimpleName().toLowerCase()
  def showParams: Map[String, String] = Map.empty
  lazy val showParamsText = showParams.map {
    case (k, v) => """%s="%s"""".format(k, v) 
  } mkString(" ")
  def showOpenCloseText = {
    val params = showParamsText.isEmpty ? "" | " " + showParamsText
    "<" + showTerm + params + "/>"
  }
  def showOpenText = {
    val params = showParamsText.isEmpty ? "" | " " + showParamsText
    "<" + showTerm + params + ">"
  }
  def showCloseText = "</" + showTerm + ">"
  def showContentsElements = elements
  def isOpenClose = showContentsElements.isEmpty

  def toString(buf: StringBuilder) {
    if (isOpenClose) {
      show_Open_Close(buf)
    } else {
      show_Open(buf)
      show_Contents(buf)
      show_Close(buf)
    }
  }
  
  override def toString() = {
    val buf = new StringBuilder
    toString(buf)
    buf.toString()
  }

  protected def show_Open_Close(buf: StringBuilder) {
    buf.append(showOpenCloseText)
  }

  protected def show_Open(buf: StringBuilder) {
    buf.append(showOpenText)
  }

  protected def show_Contents(buf: StringBuilder) {
    showContentsElements.foreach(_.toString(buf))
  }

  protected def show_Close(buf: StringBuilder) {
    buf.append(showCloseText)
  }

  def toText(): String = {
    val buf = new StringBuilder
    to_Text(buf)
    buf.toString
  }

  protected def to_Text(buf: StringBuilder) {
    showContentsElements.foreach(_.to_Text(buf))
  }
}

trait Block extends Dox {
}

trait Inline extends Dox with ListContent {
}

trait ListContent extends Dox {  
}

object Dox {
  implicit def toFragment[T <: Dox](contents: List[T]): Fragment = {
    new Fragment(contents)
  }
}

case class Document(head: Head, body: Body) extends Dox {
  override val elements = List(head, body)
  override def showTerm = "html"
  override def showOpenText = "<!DOCTYPE html><html>"
  override def showCloseText = "</html>"
}

case class Head(
    title: InlineContents = Nil,
    author: InlineContents = Nil) extends Dox {
}

object Head {
  def builder() = new Builder

  class Builder {
    var title: InlineContents = Nil
    var author: InlineContents = Nil

    def build() = new Head(title, author)
  }
}

case class Body(contents: List[Dox]) extends Dox {
  override val elements = contents
}

case class Section(title: List[Inline], contents: List[Dox], level: Int = 1) extends Dox {
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
}

case class Div() extends Block {
}

case class Paragraph() extends Block {
  override def showTerm = "p"
}

case class Text(contents: String) extends Inline {
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""
  override def show_Contents(buf: StringBuilder) {
    buf.append(contents) // TODO escape html5
  }
  override def to_Text(buf: StringBuilder) {
    buf.append(contents)
  }
}

case class Bold(contents: List[Inline]) extends Inline {
  override val elements = contents
  override def showTerm = "b"
}

// 2011-12-26
case class Italic(contents: List[Inline]) extends Inline {
  override val elements = contents
  override def showTerm = "i"
}

case class Underline(contents: List[Inline]) extends Inline {
  override val elements = contents
  override def showTerm = "u"
}

case class Code(contents: List[Inline]) extends Inline {
  override val elements = contents
}

case class Pre(contents: List[Inline]) extends Inline {
  override val elements = contents
}

case class Ul(contents: List[Li]) extends Block with ListContent {
  override val elements = contents
}

case class Ol(contents: List[Li]) extends Block with ListContent {
  override val elements = contents
}

case class Li(contents: List[ListContent]) extends Block {
  override val elements = contents

  def :+(elem: ListContent): Li = {
    Li(contents :+ elem)
  }
}

// 2011-12-30
case class Del(contents: List[Inline]) extends Inline {
  override val elements = contents
}

case class Hyperlink(contents: List[Inline], href: URI) extends Inline {
  override val elements = contents
  override def showTerm = "a"
  override def showParams = Map("href" -> href.toASCIIString())
}

case class Img(href: URI) extends Inline {
  override val elements = Nil
  override def showParams = Map("src" -> href.toASCIIString())  
}

case class Table(head: Option[THead], body: TBody, foot: Option[TFoot], 
    caption: Option[Caption], label: Option[String]) extends Block {
  override val elements = List(caption, head, body.some, foot).flatten
}

trait TableCompartment extends Block {
  val records: List[TR]
  override val elements = records
}

case class THead(records: List[TR]) extends TableCompartment {
}

case class TBody(records: List[TR]) extends TableCompartment {
}

case class TFoot(records: List[TR]) extends TableCompartment {
}

case class TR(fields: List[TField]) extends Block {
  override val elements = fields
}

trait TField extends Block {
  val contents: List[Inline]
  override val elements = contents
}

case class TD(contents: List[Inline]) extends TField {
}

case class TH(contents: List[Inline]) extends TField {  
}

case class Space() extends Inline {
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""
  override def show_Contents(buf: StringBuilder) {
    buf.append(" ")
  }
  override def to_Text(buf: StringBuilder) {
    buf.append(" ")
  }
}

case class Dl(contents: List[(Dt, Dd)]) extends Block {
  override val elements: List[Dox] = contents flatMap {
    case (dt, dd) => List(dt, dd)
  }
}

case class Dt(contents: String) extends Block {
  override val elements = List(Text(contents))
}

case class Dd(contents: List[Inline]) extends Block {
  override val elements = contents
}

case class Fragment(contents: List[Dox]) extends Dox {
  override val elements = contents
}

case class Caption(contents: List[Inline]) extends Block {
  override val elements = contents
}

// 2011-12-31
case class Figure(img: Img, caption: Figcaption, label: Option[String] = None) extends Block {
  override val elements = List(img, caption)
}

case class Figcaption(contents: List[Inline]) extends Block {
  override val elements = contents
}
