package org.smartdox.converter

import org.goldenport.tree.TreeNode
import org.smartdox._

/*
 * @since   Jun. 12, 2025
 *  version Jun. 20, 2025
 *  version Jul. 15, 2025
 *  version Aug. 25, 2025
 *  version Sep.  9, 2025
 * @version Oct. 24, 2025
 * @author  ASAMI, Tomoharu
 */
trait Dox2TextDocConverter extends Dox2StringConverter {
  import Dox2TextDocConverter._

  override def is_ignore_table_children = true
  protected def is_newline_dt_dd: Boolean = false
  protected def section_Mark: String
  protected def unorderd_List_Mark: String
  protected def orderd_List_Mark: String
  protected def list_Indent_Space: String = "  "
  protected def definition_List_Term_Mark: String
  protected def definition_List_Definition_Mark: String
  protected def bold_open: String
  protected def bold_close: String
  protected def italic_open: String
  protected def italic_close: String
  protected def bolditalic_open: String
  protected def bolditalic_close: String
  protected def code_open: String
  protected def code_close: String

  private var _list_stack: List[ListKind] = Nil

  protected final def sb_section_title(title: String): Unit =
    sb_section_title(section_Mark, title)

  protected final def sb_section_title(title: String, attachment: Seq[String]): Unit =
    sb_section_title(section_Mark, title, attachment)

  protected final def sb_list_bar(mark: String): Unit = {
    sb_print(list_bar(mark))
    sb_print_space()
  }

  protected final def sb_list_indent(mark: String): Unit = {
    sb_print(list_indent(mark, list_Indent_Space))
    sb_print_space()
  }

  protected final def sb_list_term_bar(mark: String): Unit = {
  }

  protected final def sb_list_definition_bar(mark: String): Unit = {
    sb_print(mark)
    sb_print_space()
  }

  override def stay(node: TreeNode[Dox], index: Int, prev: TreeNode[Dox], next: TreeNode[Dox]): Unit =
    stay_list(node, index, prev, next) || stay_space(node, index, prev, next)

  protected def stay_list(node: TreeNode[Dox], index: Int, prev: TreeNode[Dox], next: TreeNode[Dox]): Boolean = {
    val a = _list_stack.isEmpty
    def b = (prev.getContent, next.getContent) match {
      case (Some(p), Some(n)) => (p, n) match {
        case (mp: Li, mn: Li) => false
        case (mp: Dt, mn: Dd) => is_newline_dt_dd
        case (mp: Dd, mn: Dt) => is_newline_dt_dd
        case _ => p.isVisialBlock || n.isVisialBlock
      }
      case _ => false
    }
    // val r = a && b
    val r = b
    if (r) {
      sb_println()
      true
    } else {
      false
    }
  }

  protected def stay_space(node: TreeNode[Dox], index: Int, prev: TreeNode[Dox], next: TreeNode[Dox]): Boolean =
    (prev.getContent, next.getContent) match {
      case (Some(p), Some(n)) =>
        if (is_space_required_in_stay(p) || is_space_required_in_stay(n)) {
          sb_print(" ")
          true
        } else {
          false
        }
      case _ => false
    }

  protected def is_space_required_in_stay(p: Dox): Boolean = false

  override final protected def enter_Body(p: Body): Unit =
    p.contents.headOption match {
      case Some(s) => s match {
        case m: Section => get_metadata match {
          case Some(meta) => meta.getEffectiveLead match {
            case Some(lead) => lead.traverse(this)
            case None => Unit
          }
          case None => Unit
        }
        case _ => get_metadata match {
          case Some(meta) => meta.getLead match {
            case Some(lead) => lead.traverse(this)
            case None => Unit
          }
          case None => Unit
        }
      }
      case None => Unit
    }

  override protected def enter_Section(p: Section): Unit = {
    sb_section_title(to_text(p.title))
  }

  override def leave_Paragraph(p: Paragraph) = sb_println()

  override protected def enter_Bold(p: Bold) = {
    sb_print(bold_open)
  }

  override protected def leave_Bold(p: Bold) = {
    sb_print(bold_close)
  }

  override protected def enter_Italic(p: Italic) = {
    sb_print(italic_open)
  }

  override protected def leave_Italic(p: Italic) = {
    sb_print(italic_close)
  }

  override protected def enter_Code(p: Code) = {
    sb_print(code_open)
  }

  override protected def leave_Code(p: Code) = {
    sb_print(code_close)
  }

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
      case DlKind => sb_list_bar(unorderd_List_Mark)
    }
  }

  override protected def leave_Ul(p: Ul) = {
    _list_stack = _list_stack.tail
    // if (_list_stack == 0)
    //   sb_println()
  }

  override protected def leave_Ol(p: Ol) = {
    _list_stack = _list_stack.tail
    // if (_list_stack == 0)
    //   sb_println()
  }

  override protected def leave_Li(p: Li) = {
    val a = p.contents match {
      case Nil => true
      case x :: Nil => true
      case x :: y :: _ => y match {
        case m: Ul => false
        case m: Ol => false
        case _ => true
      }
    }
    if (a)
      sb_println()
  }

  override protected def enter_Dl(p: Dl) = {
    _list_stack = DlKind :: _list_stack
  }

  override protected def enter_Dt(p: Dt) = {
    sb_list_term_bar(definition_List_Term_Mark)
  }

  override protected def enter_Dd(p: Dd) = {
    sb_list_definition_bar(definition_List_Definition_Mark)
  }

  override protected def leave_Dt(p: Dt) = {
  }

  override protected def leave_Dd(p: Dd) = {
    sb_println()
  }

  override protected def leave_Dl(p: Dl) = {
    _list_stack = _list_stack.tail
  }
}

object Dox2TextDocConverter {
  sealed trait ListKind
  case object UlKind extends ListKind
  case object OlKind extends ListKind
  case object DlKind extends ListKind
}
