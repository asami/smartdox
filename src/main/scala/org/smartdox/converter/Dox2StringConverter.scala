package org.smartdox.converter

import scalaz.{Value => _, _}
import Scalaz._
import java.net.URI
import org.goldenport.context.Consequence
import org.goldenport.tree._
import org.goldenport.util.StringBuildFeature
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData
import Dox._

/*
 * @since   Jan. 12, 2012
 *  version Apr. 27, 2025
 *  version Jun. 18, 2025
 *  version Jul. 15, 2025
 *  version Aug. 31, 2025
 *  version Sep. 14, 2025
 *  version Oct. 26, 2025
 *  version Nov. 30, 2025
 * @version Jun.  3, 2026
 * @author  ASAMI, Tomoharu
 */
trait Dox2StringConverter extends DoxTreeVisitor with StringBuildFeature {
  // private var _head: Option[Head] = None
  // protected final def dox_metadata: Option[DocumentMetaData] = _head.map(_.metadata)

  def convert(dox: Dox): Consequence[String] = Consequence {
//    _head = Dox.getHead(dox)
    val tree = Dox.toTree(dox)
    tree.traverse(this)
    sb_to_string()
  }

  protected def normalize_text(p: String): String = {
    val s1 = if (is_in_dt) normalize_Text_Dt(p) else p
    normalize_Text(s1)
  }

  protected def normalize_Text_Dt(p: String): String = p

  protected def normalize_Text(p: String): String = p

  override protected def enter_Text(p: Text): Unit = {
    sb_print(normalize_text(to_text(p)))
  }

  override protected def enter_Value(p: Value.Single): Unit = 
    sb_print(to_text(p))

  override protected def enter_Value(p: Value.Multiple): Unit =
    sb_print(to_text(p))

  override protected def enter_Error(p: Error): Unit = {
    sb_print("Error[")
    sb_print(p.message)
    sb_print("]")
  }

  override protected def leave_Error(p: Error): Unit = {}

  override protected def enter_DiagnosticBlock(p: DiagnosticBlock): Unit = {
    sb_println(p.title)
    sb_println(p.message)
    for (label <- p.sourceLabel; source <- p.source) {
      sb_println(s"$label:")
      sb_println(source)
    }
  }

  override protected def enter_Html_Element(p: Dox): Unit =
    sb_print(p.showOpenText)

  override protected def leave_Html_Element(p: Dox): Unit =
    sb_print(p.showCloseText)

  protected final def sb_section_title(mark: String, title: String): Unit = {
    sb_println(s"${section_bar(mark)} $title")
    sb_println()
  }

  protected final def sb_section_title(mark: String, title: String, attachment: Seq[String]): Unit = {
    sb_println(s"${section_bar(mark)} $title")
    for (s <- attachment)
      sb_println(s)
    sb_println()
  }

  protected final def sb_print(p: InlineContents): Unit =
    sb_print(to_text(p))

  protected final def sb_println(p: InlineContents): Unit =
    sb_println(to_text(p))
}
