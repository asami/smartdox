package org.smartdox.converter

import org.goldenport.tree.TreeNode
import org.smartdox._

/*
 * @since   Jun. 12, 2025
 * @version Jun. 20, 2025
 * @author  ASAMI, Tomoharu
 */
trait Dox2TextDocConverter extends Dox2StringConverter {
  import Dox2TextDocConverter._

  protected def section_Mark: String
  protected def unorderd_List_Mark: String
  protected def orderd_List_Mark: String
  protected def list_Indent_Space: String = "  "

  private var _list_stack: List[ListKind] = Nil

  protected final def sb_section_title(title: String): Unit =
    sb_section_title(section_Mark, title)

  protected final def sb_list_bar(mark: String): Unit = {
    sb_print(list_bar(mark))
    sb_print_space
  }

  protected final def sb_list_indent(mark: String): Unit = {
    sb_print(list_indent(mark, list_Indent_Space))
    sb_print_space
  }

  override def stay(node: TreeNode[Dox], index: Int, prev: TreeNode[Dox], next: TreeNode[Dox]): Unit = {
    val a = _list_stack.isEmpty
    def b = (prev.getContent, next.getContent) match {
      case (Some(p), Some(n)) => p.isVisialBlock || n.isVisialBlock
      case _ => false
    }
    val r = a && b
    if (r)
      sb_println()
  }

  override protected def enter_Section(p: Section): Unit = {
    sb_section_title(to_text(p.title))
  }

  override def leave_Paragraph(p: Paragraph) = sb_println()

  override protected def enter_Ul(p: Ul) = {
    _list_stack = UlKind :: _list_stack
  }

  override protected def enter_Ol(p: Ol) = {
    _list_stack = OlKind :: _list_stack
  }

  override protected def enter_Li(p: Li) = {
    _list_stack.head match {
      case UlKind => sb_list_bar(unorderd_List_Mark)
      case OlKind => sb_list_bar(orderd_List_Mark)
    }
  }

  override protected def leave_Ul(p: Ul) = {
    _list_stack = _list_stack.tail
  }

  override protected def leave_Ol(p: Ol) = {
    _list_stack = _list_stack.tail
  }

  override protected def leave_Li(p: Li) = {
    if (p.contents.length <= 1)
      sb_println()
  }
}

object Dox2TextDocConverter {
  sealed trait ListKind
  case object UlKind extends ListKind
  case object OlKind extends ListKind
}
