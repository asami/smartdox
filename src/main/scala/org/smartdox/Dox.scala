package org.smartdox

import scalaz._
import Scalaz._

/*
 * derived from SNode.java since Sep. 17, 2006
 * derived from SDoc.scala since Sep.  1, 2008
 *
 * @since   Dec. 24, 2011
 * @version Dec. 25, 2011
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

trait Inline extends Dox {
}

object Dox {
}

case class Document(head: Head, body: Body) extends Dox {
  override val elements = List(head, body)
  override def showTerm = "html"
  override def showOpenText = "<!DOCTYPE html><html>"
  override def showCloseText = "</html>"
}

case class Head() extends Dox {
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
