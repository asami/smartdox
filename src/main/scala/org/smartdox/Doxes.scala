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
  def dox_table(header: Seq[String], body: Seq[Seq[Any]]): Table = {
    val thead = THead(Nil)
    val tbody = TBody(Nil)
    Table(thead.some, tbody, None, None, None)
  }

  def dox_table(header: Product, body: Seq[Product]): Table = {
    val h: Seq[String] = header.productIterator.map(_.toString).toList
    val b: Seq[Seq[Any]] = body.map(_.productIterator.toList)
    dox_table(h, b)
  }

  def dox_section(title: String, contents: Seq[Dox]): Section = {
    Section(dox_text(title), contents.toList)
  }

  def dox_text(s: String) = List(Text(s))

  def dox_program(xml: Elem): Program = {
    Program(XmlUtil.prettyString(xml.toString))
  }
}

object Doxes extends Doxes
