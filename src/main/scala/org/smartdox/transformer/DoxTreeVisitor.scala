package org.smartdox.transformer

import org.goldenport.RAISE
import org.goldenport.tree._
import org.smartdox._

/*
 * @since   Apr. 25, 2025
 *  version Apr. 29, 2025
 * @version Jun. 12, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxTreeVisitor extends ContentTreeVisitor[Dox] {
  private var _section: Int = 0

  protected final def section_up(): Int = {
    _section = _section + 1
    _section
  }

  protected final def section_down(): Int = {
    _section = _section - 1
    _section
  }

  protected final def section_bar(tile: String): String =
    tile * _section

  protected final def to_text(ps: Seq[Dox]): String = Dox.toText(ps)

 override final  protected def enter_Content(node: TreeNode[Dox], content: Dox): Unit =
    content match {
      case m: Text => enter_Text(m)
      case m: Paragraph => enter_Paragraph(m)
      case m: Div => enter_Div(m)
      case m: Span => enter_Span(m)
      case m: Section => enter_section(node, m)
      case m: Document => enter_Document(m)
      case m: Head => enter_head(node, m)
      case m: Body => enter_Body(m)
      case m => RAISE.notImplementedYetDefect(s"Dox2StringTransformer#start: $m")
    }

  protected def enter_section(node: TreeNode[Dox], p: Section): Unit = {
    section_up()
    enter_Section(p)
  }

  protected def enter_head(node: TreeNode[Dox], p: Head): Unit =
    enter_Head(p)

  protected def enter_Text(p: Text): Unit = {}
  protected def enter_Paragraph(p: Paragraph): Unit = {}
  protected def enter_Div(p: Div): Unit = {}
  protected def enter_Span(p: Span): Unit = {}
  protected def enter_Section(p: Section): Unit = {}
  protected def enter_Document(p: Document): Unit = {}
  protected def enter_Head(p: Head): Unit = {}
  protected def enter_Body(p: Body): Unit = {}

  override final protected def leave_Content(node: TreeNode[Dox], content: Dox): Unit =
    content match {
      case m: Text => leave_Text(m)
      case m: Paragraph => leave_Paragraph(m)
      case m: Div => leave_Div(m)
      case m: Span => leave_Span(m)
      case m: Section => leave_section(node, m)
      case m: Document => leave_Document(m)
      case m: Head => leave_head(node, m)
      case m: Body => leave_Body(m)
      case m => RAISE.notImplementedYetDefect(s"Dox2StringTransformer#start: $m")
    }

  protected def leave_section(node: TreeNode[Dox], p: Section): Unit = {
    leave_Section(p)
    section_down()
  }

  protected def leave_head(node: TreeNode[Dox], p: Head): Unit =
    leave_Head(p)

  protected def leave_Text(p: Text): Unit = {}
  protected def leave_Paragraph(p: Paragraph): Unit = {}
  protected def leave_Div(p: Div): Unit = {}
  protected def leave_Span(p: Span): Unit = {}
  protected def leave_Section(p: Section): Unit = {}
  protected def leave_Document(p: Document): Unit = {}
  protected def leave_Head(p: Head): Unit = {}
  protected def leave_Body(p: Body): Unit = {}
}
