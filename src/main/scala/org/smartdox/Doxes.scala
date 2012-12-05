package org.smartdox

import scalaz._, Scalaz._
import scala.xml.Elem
import com.asamioffice.goldenport.xml.XmlUtil

/*
 * @since   Dec.  5, 2012
 * @version Dec.  5, 2012
 * @author  ASAMI, Tomoharu
 */
trait Doxes {
  def dox_table(header: Seq[String], body: Seq[Seq[Any]],
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

  def dox_table_tuple(header: Product, body: Seq[Product],
                      caption: String = null, id: String = null): Table = {
    val h: Seq[String] = header.productIterator.map(_.toString).toList
    val b: Seq[Seq[Any]] = body.map(_.productIterator.toList)
    dox_table(h, b, caption, id)
  }

  def dox_section(depth: Int, title: String, contents: Seq[Dox]): Section = {
    Section(dox_text(title), contents.toList, depth)
  }

  def dox_text(s: String) = List(Text(s))

  def dox_program(title: String, xml: Elem, id: String = null): Program = {
    val attrs = List(("title", title).some,
                     Option(id).map(x => ("id", x))).flatten
    Program(XmlUtil.prettyString(xml.toString), attrs)
  }

  def dox_p(s: String, args: Any*): Paragraph = {
    Paragraph(dox_text(s.format(args: _*)))
  }
}

object Doxes extends Doxes
