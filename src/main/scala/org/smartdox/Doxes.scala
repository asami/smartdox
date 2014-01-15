package org.smartdox

import scalaz._, Scalaz._
import scala.xml.Elem
import com.asamioffice.goldenport.xml.XmlUtil

/*
 * @since   Dec.  5, 2012
 * @version Jan. 15, 2014
 * @author  ASAMI, Tomoharu
 */
trait Doxes {
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
}

object Doxes extends Doxes

case class Inlines(inlines: List[Inline])
case class Blocks(blocks: List[Dox])
