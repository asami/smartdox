package org.smartdox.converters

import scalaz._, Scalaz._
import java.net.URI
import org.goldenport.RAISE
import org.goldenport.tree._
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.converter._

/*
 * @since   Apr. 18, 2025
 *  version Apr. 29, 2025
 * @version Jun. 20, 2025
 * @author  ASAMI, Tomoharu
 */
class Dox2AsciidocConverter(
  context: Context
) extends Dox2TextDocConverter {
  import Dox2AsciidocConverter._

  protected def section_Mark = "="
  protected def unorderd_List_Mark = "*"
  protected def orderd_List_Mark = "."

  override protected def enter_Head(p: Head): Unit =
    p.title match {
      case Nil => // do nothing
      case xs => enter_asciidoc_section(to_text(xs))
    }

  protected final def enter_asciidoc_section(title: String): Unit = {
    section_up()
    sb_section_title(title)
  }

  protected final def leave_asciidoc_section(): Unit = {
    section_down()
  }

  override protected def enter_Table(p: Table): Unit = {
    val header = p.head map { s =>
      val d = s"""[%autowidth, options="header"]"""
      sb_println(d)
      s.records
    }
    sb_println("|===")
    header.foreach(_print_records)
    _print_records(p.body.records)
    sb_println("|===")
  }

  private def _print_records(ps: List[TRecord]): Unit =
    ps.foreach(_print_record)

  private def _print_record(p: TRecord): Unit = {
    val s = p.fields.map(_.text).mkString("|", "|", "")
    sb_println(s)
  }
}

object Dox2AsciidocConverter {
}
