package org.smartdox

import scalaz._
import Scalaz._

/*
 * derived from SNode.java since Sep. 17, 2006
 * derived from SDoc.scala since Sep.  1, 2008
 *
 * @since   Dec. 24, 2011
 * @version Dec. 28, 2011
 * @author  ASAMI, Tomoharu
 */
abstract class Dox {
  val elements: List[Dox] = Nil
  def showTerm = getClass.getSimpleName().toLowerCase()
  def showOpenText = "<" + showTerm + ">"
  def showCloseText = "</" + showTerm + ">"
  def showContentsElements = elements

  def toString(buf: StringBuilder) {
    show_Open(buf)
    show_Contents(buf)
    show_Close(buf)
  }
  
  override def toString() = {
    val buf = new StringBuilder
    toString(buf)
    buf.toString()
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
}

trait Block extends Dox {
}

trait Inline extends Dox with ListContent {
}

trait ListContent extends Dox {  
}

object Dox {
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
}

case class Div() extends Block {
}

case class P() extends Block {
}

case class Text(contents: String) extends Inline {
  override def showOpenText = ""
  override def showCloseText = ""
  override def show_Contents(buf: StringBuilder) {
    buf.append(contents) // TODO escape html5
  }
}

case class Bold(contents: List[Inline]) extends Inline {
  override val elements = contents
}

// 2011-12-26
case class Italic(contents: List[Inline]) extends Inline {
  override val elements = contents
}

case class Underline(contents: List[Inline]) extends Inline {
  override val elements = contents
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

case class Li(contents: List[ListContent]) extends Block {
  override val elements = contents

  def :+(elem: ListContent): Li = {
    Li(contents :+ elem)
  }
}
