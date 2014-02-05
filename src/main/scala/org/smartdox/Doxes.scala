package org.smartdox

import scala.language.implicitConversions
import scalaz._, Scalaz._
import scala.xml.Elem
import com.asamioffice.goldenport.xml.XmlUtil
import org.goldenport.Strings.blankp

/*
 * @since   Dec.  5, 2012
 *  version Jan. 16, 2014
 * @version Feb.  5, 2014
 * @author  ASAMI, Tomoharu
 */
trait Doxes {
  implicit def inlineListWrapper(s: String): List[Inline] = dox_inline(s)
  implicit def string2InlinesWrapper(s: String): Inlines = Inlines(dox_inline(s))
  implicit def string2BlocksWrapper(s: String): Blocks = Blocks(dox_block(s))
  implicit def doxes2InlinesWrapper(xs: Seq[Dox]): Inlines = dox_inline(xs)
  implicit def doxes2BlocksWrapper(xs: Seq[Dox]): Blocks = Blocks(xs.toList)

  protected def dox_table(header: Seq[String], body: Seq[Seq[Any]],
                caption: String = null, id: String = null): Table = {
    val h = TR(header.map(x => TH(dox_text(x))).toList)
    val b = for (r <- body.toList) yield {
      TR(for (f <- r.toList) yield {
        TD(dox_text(f.toString))
      })
    }
    val c = Option(caption).map(x => Caption(dox_text(x)))
    val l = Option(id)
    val thead = THead(List(h))
    val tbody = TBody(b)
    Table(thead.some, tbody, None, c, l)
  }

  protected def dox_table_tuple(header: Product, body: Seq[Product],
                      caption: String = null, id: String = null): Table = {
    val h: Seq[String] = header.productIterator.map(_.toString).toList
    val b: Seq[Seq[Any]] = body.map(_.productIterator.toList)
    dox_table(h, b, caption, id)
  }

  protected def dox_section(depth: Int, title: String, contents: Seq[Dox]): Section = {
    Section(dox_text(title), contents.toList, depth)
  }

  protected def dox_text(s: String) = List(Text(s))

  protected def dox_program(title: String, xml: Elem, id: String = null): Program = {
    val attrs = List(("title", title).some,
                     Option(id).map(x => ("id", x))).flatten
    Program(XmlUtil.prettyString(xml.toString), attrs)
  }

  protected def dox_p(s: String, args: Any*): Paragraph = {
    Paragraph(dox_text(s.format(args: _*)))
  }

  protected def dox_inline(d: Dox): List[Inline] = {
    d match {
      case x: Inline => List(x)
      case Document(_, body) => dox_inline(body.contents)
    }
  }

  protected def dox_inline(xs: Seq[Dox]): List[Inline] = {
    xs.toList match {
      case List(p: Paragraph) => dox_inline(p.contents)
      case z => for (x <- z) yield {
        x match {
          case i: Inline => i
          case e => sys.error("No inline = " + e)
        }
      }
    }
  }

  protected def dox_block(d: Dox): List[Dox] = {
    d match {
      case x: Inline => List(x)
      case Document(_, body) => body.contents
    }
  }

  import parser.DoxParser

  protected def dox_inline(s: String): List[Inline] = {
    DoxParser.parseOrgmodeZ(s) match {
      case Success(d) => dox_inline(d)
      case Failure(msgs) => sys.error(msgs.list.mkString("/"))
    }
  }

  protected def dox_block(s: String): List[Dox] = {
    DoxParser.parseOrgmodeZ(s) match {
      case Success(d) => dox_block(d)
      case Failure(msgs) => sys.error(msgs.list.mkString("/"))
    }
  }

  protected def dox_desc(
    title: String = null,
    summary: String = null,
    content: String = null,
    concat: Boolean = true
  ): Description = {
    def toi(s: String) = {
      if (blankp(s)) Inlines(Nil) else string2InlinesWrapper(s)
    }
    def tob(s: String) = {
      if (blankp(s)) Blocks(Nil) else string2BlocksWrapper(s)
    }
    val sm = toi(summary)
    val c = if (concat) {
      tob(content).toDox match {
        case Fragment(xs) => Fragment(sm.toParagraph :: xs)
        case EmptyDox => sm.toParagraph
        case x => Fragment(List(sm.toParagraph, x))
      } 
    } else tob(content).toDox
    Description(
      title = toi(title).toDox,
      summary = sm.toDox,
      content = c
    )
  }
}

object Doxes extends Doxes

case class Inlines(inlines: List[Inline]) {
  def toDox = {
    inlines match {
      case Nil => EmptyDox
      case List(x) => x
      case xs => Fragment(xs)
    }
  }

  def toParagraph = {
    inlines match {
      case Nil => EmptyDox
      case xs => Paragraph(inlines)
    }
  }
}
case class Blocks(blocks: List[Dox]) {
  def toDox = {
    blocks match {
      case Nil => EmptyDox
      case List(x) => x
      case xs => Fragment(xs)
    }
  }
}
