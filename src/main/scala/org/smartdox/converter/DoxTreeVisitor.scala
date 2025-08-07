package org.smartdox.converter

import org.goldenport.RAISE
import org.goldenport.tree._
import org.smartdox._

/*
 * @since   Apr. 25, 2025
 *  version Apr. 29, 2025
 *  version Jun. 18, 2025
 *  version Jul. 26, 2025
 * @version Aug.  7, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxTreeVisitor extends ContentTreeVisitor[Dox] {
  protected def is_ignore_table_children: Boolean = false
  protected def is_ignore_img_in_figure: Boolean = false
  private var _section: Int = 0
  private var _list_depth = 0
  private var _is_in_figure: Boolean = false

  protected final def is_uninvoke_img = is_ignore_img_in_figure && _is_in_figure
  protected final def is_invoke_img = !is_uninvoke_img

  protected final def section_up(): Int = {
    _section = _section + 1
    _section
  }

  protected final def section_down(): Int = {
    _section = _section - 1
    _section
  }

  protected final def list_up(): Int = {
    _list_depth = _list_depth + 1
    _list_depth
  }

  protected final def list_down(): Int = {
    _list_depth = _list_depth - 1
    _list_depth
  }

  protected final def section_bar(tile: String): String =
    tile * _section

  protected final def list_bar(mark: String): String = list_bar(mark, _list_depth)

  protected final def list_bar(mark: String, depth: Int): String =
    mark * depth

  protected final def list_indent(mark: String, space: String): String = list_indent(mark, _list_depth, space)

  protected final def list_indent(mark: String, depth: Int, space: String): String =
    space * depth + mark

  protected final def to_text(ps: Seq[Dox]): String = Dox.toText(ps)

  protected final def get_text(ps: Seq[Dox]): Option[String] = Dox.getText(ps)

  override final protected def start_Content(node: TreeNode[Dox], content: Dox): Unit =
    _enter_content(node, content)

  override final protected def enter_Content(node: TreeNode[Dox], content: Dox): Unit =
    if (is_invoke_img)
      _enter_content(node, content)

  private def _enter_content(node: TreeNode[Dox], content: Dox): Unit =
    content match {
      case m: Text => enter_Text(m)
      case m: Paragraph => enter_Paragraph(m)
      case m: Div => enter_Div(m)
      case m: Span => enter_Span(m)
      case m: Bold => enter_Bold(m)
      case m: Italic => enter_Italic(m)
      case m: Ul => enter_ul(m)
      case m: Ol => enter_ol(m)
      case m: Li => enter_Li(m)
      case m: Dl => enter_dl(m)
      case m: Dt => enter_Dt(m)
      case m: Dd => enter_Dd(m)
      case m: Hyperlink => enter_Hyperlink(m)
      case m: Figure => enter_figure(m)
      case m: Figcaption => enter_figcaption(m)
      case m: Img => enter_img(m)
      case m: Table => enter_table(node, m)
      case m: THead => enter_Thead(m)
      case m: TBody => enter_Tbody(m)
      case m: TFoot => enter_Tfoot(m)
      case m: TR => enter_Tr(m)
      case m: TH => enter_Th(m)
      case m: TD => enter_Td(m)
      case m: Section => enter_section(node, m)
      case m: Program => enter_program(node, m)
      case m: Document => enter_Document(m)
      case m: Head => enter_head(node, m)
      case m: Body => enter_Body(m)
      case m => RAISE.notImplementedYetDefect(s"Dox2StringConverter#start: $m")
    }

  protected def enter_section(node: TreeNode[Dox], p: Section): Unit = {
    section_up()
    enter_Section(p)
  }

  protected def enter_program(node: TreeNode[Dox], p: Program): Unit = {
    enter_Program(p)
  }

  protected def enter_head(node: TreeNode[Dox], p: Head): Unit =
    enter_Head(p)

  protected def enter_ul(p: Ul): Unit = {
    list_up()
    enter_Ul(p)
  }

  protected def enter_ol(p: Ol): Unit = {
    list_up()
    enter_Ol(p)
  }

  protected def enter_dl(p: Dl): Unit = {
    list_up()
    enter_Dl(p)
  }

  protected def enter_figure(p: Figure): Unit = {
    enter_Figure(p)
    _is_in_figure = true
  }

  protected def enter_figcaption(p: Figcaption): Unit = {
    enter_Figcaption(p)
  }

  protected def enter_img(p: Img): Unit = {
    enter_Img(p)
  }

  protected def enter_table(node: TreeNode[Dox], p: Table): Unit = {
    enter_Table(p)
    if (is_ignore_table_children)
      done_traverse(node)
  }

  protected def enter_Text(p: Text): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Text: $p")
  protected def enter_Paragraph(p: Paragraph): Unit = {}
  protected def enter_Div(p: Div): Unit = {}
  protected def enter_Span(p: Span): Unit = {}
  protected def enter_Bold(p: Bold): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Bold: $p")
  protected def enter_Italic(p: Italic): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Italic: $p")
  protected def enter_Ul(p: Ul): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Ul: $p")
  protected def enter_Ol(p: Ol): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Ol: $p")
  protected def enter_Li(p: Li): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Li: $p")
  protected def enter_Dl(p: Dl): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Dl: $p")
  protected def enter_Dt(p: Dt): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Dt: $p")
  protected def enter_Dd(p: Dd): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Dd: $p")
  protected def enter_Hyperlink(p: Hyperlink): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Hyperlink: $p")
  protected def enter_Figure(p: Figure): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Figure: $p")
  protected def enter_Figcaption(p: Figcaption): Unit = {}
  protected def enter_Img(p: Img): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Img: $p")
  protected def enter_Table(p: Table): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Table: $p")
  protected def enter_Thead(p: THead): Unit = {}
  protected def enter_Tbody(p: TBody): Unit = {}
  protected def enter_Tfoot(p: TFoot): Unit = {}
  protected def enter_Tr(p: TR): Unit = {}
  protected def enter_Th(p: TH): Unit = {}
  protected def enter_Td(p: TD): Unit = {}
  protected def enter_Section(p: Section): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Section: $p")
  protected def enter_Program(p: Program): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Program: $p")
  protected def enter_Document(p: Document): Unit = {}
  protected def enter_Head(p: Head): Unit = {}
  protected def enter_Body(p: Body): Unit = {}

  override final protected def leaveEnd_Content(node: TreeNode[Dox], content: Dox): Unit =
    _leave_content(node, content)

  override final protected def leave_Content(node: TreeNode[Dox], content: Dox): Unit =
    if (is_invoke_img)
      _leave_content(node, content)
    else
      _leave_content_figure(node, content)

  private def _leave_content(node: TreeNode[Dox], content: Dox): Unit =
    content match {
      case m: Text => leave_Text(m)
      case m: Paragraph => leave_Paragraph(m)
      case m: Div => leave_Div(m)
      case m: Span => leave_Span(m)
      case m: Bold => leave_Bold(m)
      case m: Italic => leave_Italic(m)
      case m: Ul => leave_ul(m)
      case m: Ol => leave_ol(m)
      case m: Li => leave_Li(m)
      case m: Dl => leave_dl(m)
      case m: Dt => leave_Dt(m)
      case m: Dd => leave_Dd(m)
      case m: Hyperlink => leave_Hyperlink(m)
      case m: Figure => leave_figure(m)
      case m: Figcaption => leave_figcaption(m)
      case m: Img => leave_img(m)
      case m: Table => leave_table(m)
      case m: THead => leave_Thead(m)
      case m: TBody => leave_Tbody(m)
      case m: TFoot => leave_Tfoot(m)
      case m: TR => leave_Tr(m)
      case m: TH => leave_Th(m)
      case m: TD => leave_Td(m)
      case m: Section => leave_section(node, m)
      case m: Program => leave_program(node, m)
      case m: Document => leave_Document(m)
      case m: Head => leave_head(node, m)
      case m: Body => leave_Body(m)
      case m => RAISE.notImplementedYetDefect(s"Dox2StringConverter#start: $m")
    }

  private def _leave_content_figure(node: TreeNode[Dox], content: Dox): Unit =
    content match {
      case m: Figure => leave_figure(m)
      case m => {}
    }

  protected def leave_section(node: TreeNode[Dox], p: Section): Unit = {
    leave_Section(p)
    section_down()
  }

  protected def leave_program(node: TreeNode[Dox], p: Program): Unit = {
    leave_Program(p)
  }

  protected def leave_head(node: TreeNode[Dox], p: Head): Unit =
    leave_Head(p)

  protected def leave_ul(p: Ul): Unit = {
    leave_Ul(p)
    list_down()
  }

  protected def leave_ol(p: Ol): Unit = {
    leave_Ol(p)
    list_down()
  }

  protected def leave_dl(p: Dl): Unit = {
    leave_Dl(p)
    list_down()
  }

  protected def leave_figure(p: Figure): Unit = {
    leave_Figure(p)
    _is_in_figure = false
  }

  protected def leave_figcaption(p: Figcaption): Unit = {
    if (is_invoke_img)
      leave_Figcaption(p)
  }

  protected def leave_img(p: Img): Unit = {
    if (is_invoke_img)
      leave_Img(p)
  }

  protected def leave_table(p: Table): Unit = {
    leave_Table(p)
  }

  protected def leave_Text(p: Text): Unit = {}
  protected def leave_Paragraph(p: Paragraph): Unit = {}
  protected def leave_Div(p: Div): Unit = {}
  protected def leave_Span(p: Span): Unit = {}
  protected def leave_Bold(p: Bold): Unit = {}
  protected def leave_Italic(p: Italic): Unit = {}
  protected def leave_Ul(p: Ul): Unit = {}
  protected def leave_Ol(p: Ol): Unit = {}
  protected def leave_Li(p: Li): Unit = {}
  protected def leave_Dl(p: Dl): Unit = {}
  protected def leave_Dt(p: Dt): Unit = {}
  protected def leave_Dd(p: Dd): Unit = {}
  protected def leave_Hyperlink(p: Hyperlink): Unit = {}
  protected def leave_Figure(p: Figure): Unit = {}
  protected def leave_Figcaption(p: Figcaption): Unit = {}
  protected def leave_Img(p: Img): Unit = {}
  protected def leave_Table(p: Table): Unit = {}
  protected def leave_Thead(p: THead): Unit = {}
  protected def leave_Tbody(p: TBody): Unit = {}
  protected def leave_Tfoot(p: TFoot): Unit = {}
  protected def leave_Tr(p: TR): Unit = {}
  protected def leave_Th(p: TH): Unit = {}
  protected def leave_Td(p: TD): Unit = {}
  protected def leave_Section(p: Section): Unit = {}
  protected def leave_Program(p: Program): Unit = {}
  protected def leave_Document(p: Document): Unit = {}
  protected def leave_Head(p: Head): Unit = {}
  protected def leave_Body(p: Body): Unit = {}
}
