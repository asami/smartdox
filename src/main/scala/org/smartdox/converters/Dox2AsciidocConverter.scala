package org.smartdox.converters

import scalaz._, Scalaz._
import java.net.URI
import org.goldenport.RAISE
import org.goldenport.tree._
import org.goldenport.util.ListUtils
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.converter._

/*
 * @since   Apr. 18, 2025
 *  version Apr. 29, 2025
 *  version Jun. 20, 2025
 *  version Jul. 28, 2025
 * @version Aug. 22, 2025
 * @author  ASAMI, Tomoharu
 */
class Dox2AsciidocConverter(
  context: Context
) extends Dox2TextDocConverter {
  import Dox2AsciidocConverter._

  protected override def is_ignore_img_in_figure = true
  protected def section_Mark = "="
  protected def unorderd_List_Mark = "*"
  protected def orderd_List_Mark = "."
  protected def definition_List_Term_Mark = ""
  protected def definition_List_Definition_Mark = "::"
  protected def bold_open = "*"
  protected def bold_close = "*"
  protected def italic_open = "_"
  protected def italic_close = "_"
  protected def bolditalic_open = "*_"
  protected def bolditalic_close = "_*"

  override protected def enter_Head(p: Head): Unit =
    p.titleDefault match {
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

  override protected def enter_Hyperlink(p: Hyperlink) = {
    sb_print(s"link:${p.href.toString}[")
  }

  override protected def leave_Hyperlink(p: Hyperlink) = {
    p.getTitle foreach { x =>
      sb_print(""" ,title="""")
      sb_print(x)
      sb_print("\"")
    }
    p.getHtmlClass foreach { x =>
      sb_print(""" ,role="""")
      sb_print(x)
      sb_print("\"")
    }
    sb_print("]")
  }

  override protected def enter_Figure(p: Figure): Unit = {
    val attrs = ListUtils.buildTupleList(
      List("role" -> "img-figure"),
      List(
        "alt" -> get_text(p.caption.contents),
        "title" -> get_text(p.caption.contents)
      )
    )
    sb_print(".")
    sb_print(p.caption.contents)
    sb_println("")
    sb_print("image::")
    if (false)
      sb_print("_")
    sb_print(_normalize_src(p.img.src))
    sb_print("[")
    sb_print(_build_attrs(attrs))
    sb_println("]")
    sb_println()
  }

  private def _normalize_src(p: URI): String = {
    val s = p.toString
    if (s.startsWith("images/"))
      s.substring("images/".length)
    else
      s
  }

  private def _build_attrs(ps: Seq[(String, String)]): String = 
    ps.map {
      case (k, v) => s"$k=$v" // TODO escape
    }.mkString(",")

  override protected def leave_Figure(p: Figure): Unit = {
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

  override protected def enter_Program(p: Program): Unit = {
    val caption = p.caption
    val kind = p.kind getOrElse "text"
    val directive = s"[source,$kind]"
    caption.foreach { x =>
      sb_print(".")
      sb_println(x)
    }
    sb_println(directive)
    sb_println("----")
  }

  override protected def leave_Program(p: Program): Unit = {
    sb_println("----")
  }
}

object Dox2AsciidocConverter {
}
