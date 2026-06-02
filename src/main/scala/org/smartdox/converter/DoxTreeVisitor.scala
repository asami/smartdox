package org.smartdox.converter

import java.util.Locale
import org.goldenport.RAISE
import org.goldenport.tree._
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData

/*
 * @since   Apr. 25, 2025
 *  version Apr. 29, 2025
 *  version Jun. 18, 2025
 *  version Jul. 26, 2025
 *  version Aug. 31, 2025
 *  version Sep. 14, 2025
 *  version Oct. 26, 2025
 *  version Nov. 30, 2025
 *  version Apr. 16, 2026
 * @version Jun.  3, 2026
 * @author  ASAMI, Tomoharu
 */
trait DoxTreeVisitor extends ContentTreeVisitor[Dox] {
  protected def is_xml: Boolean = false
  protected def is_ignore_table_children: Boolean = false
  protected def is_ignore_img_in_figure: Boolean = false
  private var _section: Int = 0
  private var _list_depth = 0
  private var _is_in_figure: Boolean = false
  private var _metadata: Option[DocumentMetaData] = None
  private var _node_stack: List[TreeNode[Dox]] = Nil

  protected final def is_uninvoke_img = is_ignore_img_in_figure && _is_in_figure
  protected final def is_invoke_img = !is_uninvoke_img

  protected final def current_node: TreeNode[Dox] = _node_stack.head

  protected final def current_dox: Dox = current_node.content

  protected final def get_locale: Option[Locale] = Dox.getLocaleInContext(current_node)

  protected final def get_metadata: Option[DocumentMetaData] = _metadata

  protected final def get_directive: Option[DocumentMetaData.Directive] =
    get_metadata.map(_.directive)

  protected final def is_autowire_dt: Boolean = get_metadata.fold(true)(_.isAutoWireDt getOrElse true)

  protected final def is_in_dt: Boolean = _node_stack.exists(x =>
    x.getContent.fold(false) {
      case _: Dt => true
      case _ => false
    }
  )

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

  protected def to_text(p: Dox): String =
    if (is_xml)
      p.toPlainText
    else
      p.toText

  protected def to_text(p: Seq[Dox]): String =
    if (is_xml)
      Dox.toPlainText(p)
    else
      Dox.toText(p)

  protected final def get_text(ps: Seq[Dox]): Option[String] = Dox.getText(ps)

  override final protected def start_Content(node: TreeNode[Dox], content: Dox): Unit =
    _enter_content(node, content)

  override final protected def enter_Content(node: TreeNode[Dox], content: Dox): Unit =
    if (is_invoke_img)
      _enter_content(node, content)

  private def _enter_content(node: TreeNode[Dox], content: Dox): Unit = {
    _node_stack = node :: _node_stack
    content match {
      case m: Text => enter_Text(m)
      case m: Paragraph => enter_Paragraph(m)
      case m: Div => enter_Div(m)
      case m: Span => enter_Span(m)
      case m: Bold => enter_Bold(m)
      case m: Em => enter_Em(m)
      case m: Strong => enter_Strong(m)
      case m: Italic => enter_Italic(m)
      case m: Code => enter_Code(m)
//      case m: Verbatim => enter_Verbatim(m)
      case m: Ul => enter_ul(m)
      case m: Ol => enter_ol(m)
      case m: Li => enter_Li(m)
      case m: Dl => enter_dl(m)
      case m: Dt => enter_dt(m)
      case m: Dd => enter_Dd(m)
      case m: Hyperlink => enter_Hyperlink(m)
      case m: InlineMacro => enter_InlineMacro(m)
      case m: Figure => enter_figure(m)
      case m: Figcaption => enter_figcaption(m)
      case m: Img => enter_img(m)
      case m: Table => enter_table(node, m)
      case m: THead => enter_Thead(m)
      case m: TBody => enter_Tbody(m)
      case m: TFoot => enter_Tfoot(m)
      case m: Caption => enter_caption(m)
      case m: TR => enter_Tr(m)
      case m: TH => enter_Th(m)
      case m: TD => enter_Td(m)
      case m: Section => enter_section(node, m)
      case m: Fragment => enter_fragment(m)
      case m: I18NFragment => enter_i18nfragment(m)
      case m: Program => enter_program(node, m)
      case m: DiagnosticBlock => enter_DiagnosticBlock(m)
      case m: Document => enter_Document(m)
      case m: Head => enter_head(node, m)
      case m: Body => enter_Body(m)
      case m: Foot => enter_Foot(m)
      case m: HorizontalRule => enter_HorizontalRule(m)
      case m: Quotation.SimpleQuote => enter_Quotation_SimpleQuote(m)
      case m: Quotation.BlockQuote => enter_Quotation_BlockQuote(m)
      case m: Value.Single => enter_Value(m)
      case m: Value.Multiple => enter_Value(m)
      case m: Html5Inline => enter_Html5Inline(m)
      case m: Html5 => enter_Html5(m)
      case m: Error => enter_Error(m)
      case m => RAISE.notImplementedYetDefect(s"Dox2TreeVisitor[${getClass.getSimpleName}]#start: $m")
    }
  }

  protected def enter_section(node: TreeNode[Dox], p: Section): Unit = {
    section_up()
    enter_Section(p)
  }

  protected def enter_fragment(p: Fragment): Unit = {
  }

  protected def enter_i18nfragment(p: I18NFragment): Unit = {
    enter_I18NFragment(p)
  }

  protected def enter_program(node: TreeNode[Dox], p: Program): Unit = {
    enter_Program(p)
  }

  protected def enter_head(node: TreeNode[Dox], p: Head): Unit = {
    _metadata = Some(p.metadata)
    enter_Head(p)
  }

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

  protected def enter_dt(p: Dt): Unit = {
    enter_Dt(p)
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

  protected def enter_caption(p: Caption): Unit = {
    enter_Caption(p)
  }

  protected def enter_Text(p: Text): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Text: $p")
  protected def enter_Paragraph(p: Paragraph): Unit = {}
  protected def enter_Div(p: Div): Unit = {}
  protected def enter_Span(p: Span): Unit = {}
  protected def enter_Bold(p: Bold): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Bold: $p")
  protected def enter_Em(p: Em): Unit = enter_Html_Element(p)
  protected def enter_Strong(p: Strong): Unit = enter_Html_Element(p)
  protected def enter_Italic(p: Italic): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Italic: $p")
  protected def enter_Code(p: Code): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Code: $p")
//  protected def enter_Verbatim(p: Verbatim): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Verbatim: $p")
  protected def enter_Ul(p: Ul): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Ul: $p")
  protected def enter_Ol(p: Ol): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Ol: $p")
  protected def enter_Li(p: Li): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Li: $p")
  protected def enter_Dl(p: Dl): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Dl: $p")
  protected def enter_Dt(p: Dt): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Dt: $p")
  protected def enter_Dd(p: Dd): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Dd: $p")
  protected def enter_Hyperlink(p: Hyperlink): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Hyperlink: $p")
  protected def enter_InlineMacro(p: InlineMacro): Unit = {}
  protected def enter_Figure(p: Figure): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Figure: $p")
  protected def enter_Figcaption(p: Figcaption): Unit = {}
  protected def enter_Img(p: Img): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Img: $p")
  protected def enter_Table(p: Table): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Table: $p")
  protected def enter_Thead(p: THead): Unit = {}
  protected def enter_Tbody(p: TBody): Unit = {}
  protected def enter_Tfoot(p: TFoot): Unit = {}
  protected def enter_Caption(p: Caption): Unit = {}
  protected def enter_Tr(p: TR): Unit = {}
  protected def enter_Th(p: TH): Unit = {}
  protected def enter_Td(p: TD): Unit = {}
  protected def enter_Section(p: Section): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Section: $p")
  protected def enter_I18NFragment(p: I18NFragment): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] I18NFragment: $p")
  protected def enter_Program(p: Program): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Program: $p")
  protected def enter_DiagnosticBlock(p: DiagnosticBlock): Unit = {}
  protected def enter_Document(p: Document): Unit = {}
  protected def enter_Head(p: Head): Unit = {}
  protected def enter_Body(p: Body): Unit = {}
  protected def enter_Foot(p: Foot): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Foot: $p")
  protected def enter_HorizontalRule(p: HorizontalRule): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] HorizontalRule: $p")
  protected def enter_Quotation_SimpleQuote(p: Quotation.SimpleQuote): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Quotation.SimpleQuote: $p")
  protected def enter_Quotation_BlockQuote(p: Quotation.BlockQuote): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Quotation.BlockQuote: $p")
  protected def enter_Value(p: Value.Single): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Value.Single: $p")
  protected def enter_Value(p: Value.Multiple): Unit = RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] Value.Multiple: $p")
  protected def enter_Html5(p: Html5): Unit = enter_Html_Element(p)
  protected def enter_Html5Inline(p: Html5Inline): Unit = enter_Html_Element(p)
  protected def enter_Error(p: Error): Unit = enter_Html_Element(p)

  protected def enter_Html_Element(p: Dox): Unit = {
    RAISE.notImplementedYetDefect(s"Dox2StringConverter[${getClass.getSimpleName}] HtmlElement: $p")
  }

  override final protected def leaveEnd_Content(node: TreeNode[Dox], content: Dox): Unit =
    _leave_content(node, content)

  override final protected def leave_Content(node: TreeNode[Dox], content: Dox): Unit =
    if (is_invoke_img)
      _leave_content(node, content)
    else
      _leave_content_figure(node, content)

  private def _leave_content(node: TreeNode[Dox], content: Dox): Unit = {
    content match {
      case m: Text => leave_Text(m)
      case m: Paragraph => leave_Paragraph(m)
      case m: Div => leave_Div(m)
      case m: Span => leave_Span(m)
      case m: Bold => leave_Bold(m)
      case m: Em => leave_Em(m)
      case m: Strong => leave_Strong(m)
      case m: Italic => leave_Italic(m)
      case m: Code => leave_Code(m)
//      case m: Verbatim => leave_Verbatim(m)
      case m: Ul => leave_ul(m)
      case m: Ol => leave_ol(m)
      case m: Li => leave_Li(m)
      case m: Dl => leave_dl(m)
      case m: Dt => leave_dt(m)
      case m: Dd => leave_Dd(m)
      case m: Hyperlink => leave_Hyperlink(m)
      case m: InlineMacro => leave_InlineMacro(m)
      case m: Figure => leave_figure(m)
      case m: Figcaption => leave_figcaption(m)
      case m: Img => leave_img(m)
      case m: Table => leave_table(m)
      case m: THead => leave_Thead(m)
      case m: TBody => leave_Tbody(m)
      case m: TFoot => leave_Tfoot(m)
      case m: Caption => leave_caption(m)
      case m: TR => leave_Tr(m)
      case m: TH => leave_Th(m)
      case m: TD => leave_Td(m)
      case m: Section => leave_section(node, m)
      case m: Fragment => leave_fragment(m)
      case m: I18NFragment => leave_i18nfragment(m)
      case m: Program => leave_program(node, m)
      case m: DiagnosticBlock => leave_DiagnosticBlock(m)
      case m: Document => leave_Document(m)
      case m: Head => leave_head(node, m)
      case m: Body => leave_Body(m)
      case m: Foot => leave_Foot(m)
      case m: HorizontalRule => leave_HorizontalRule(m)
      case m: Quotation.SimpleQuote => leave_Quotation_SimpleQuote(m)
      case m: Quotation.BlockQuote => leave_Quotation_BlockQuote(m)
      case m: Value.Single => leave_Value(m)
      case m: Value.Multiple => leave_Value(m)
      case m: Html5Inline => leave_Html5Inline(m)
      case m: Html5 => leave_Html5(m)
      case m: Error => leave_Error(m)
      case m => RAISE.notImplementedYetDefect(s"Dox2StringConverter#start: $m")
    }
    _node_stack = _node_stack.tail
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

  protected def leave_fragment(p: Fragment): Unit = {
  }

  protected def leave_i18nfragment(p: I18NFragment): Unit = {
    leave_I18NFragment(p)
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

  protected def leave_dt(p: Dt): Unit = {
    leave_Dt(p)
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

  protected def leave_caption(p: Caption): Unit = {
    leave_Caption(p)
  }

  protected def leave_Text(p: Text): Unit = {}
  protected def leave_Paragraph(p: Paragraph): Unit = {}
  protected def leave_Div(p: Div): Unit = {}
  protected def leave_Span(p: Span): Unit = {}
  protected def leave_Bold(p: Bold): Unit = {}
  protected def leave_Em(p: Em): Unit = leave_Html_Element(p)
  protected def leave_Strong(p: Strong): Unit = leave_Html_Element(p)
  protected def leave_Italic(p: Italic): Unit = {}
  protected def leave_Code(p: Code): Unit = {}
//  protected def leave_Verbatim(p: Verbatim): Unit = {}
  protected def leave_Ul(p: Ul): Unit = {}
  protected def leave_Ol(p: Ol): Unit = {}
  protected def leave_Li(p: Li): Unit = {}
  protected def leave_Dl(p: Dl): Unit = {}
  protected def leave_Dt(p: Dt): Unit = {}
  protected def leave_Dd(p: Dd): Unit = {}
  protected def leave_Hyperlink(p: Hyperlink): Unit = {}
  protected def leave_InlineMacro(p: InlineMacro): Unit = {}
  protected def leave_Figure(p: Figure): Unit = {}
  protected def leave_Figcaption(p: Figcaption): Unit = {}
  protected def leave_Img(p: Img): Unit = {}
  protected def leave_Table(p: Table): Unit = {}
  protected def leave_Thead(p: THead): Unit = {}
  protected def leave_Tbody(p: TBody): Unit = {}
  protected def leave_Tfoot(p: TFoot): Unit = {}
  protected def leave_Caption(p: Caption): Unit = {}
  protected def leave_Tr(p: TR): Unit = {}
  protected def leave_Th(p: TH): Unit = {}
  protected def leave_Td(p: TD): Unit = {}
  protected def leave_Section(p: Section): Unit = {}
  protected def leave_I18NFragment(p: I18NFragment): Unit = {}
  protected def leave_Program(p: Program): Unit = {}
  protected def leave_DiagnosticBlock(p: DiagnosticBlock): Unit = {}
  protected def leave_Document(p: Document): Unit = {}
  protected def leave_Head(p: Head): Unit = {}
  protected def leave_Body(p: Body): Unit = {}
  protected def leave_Foot(p: Foot): Unit = {}
  protected def leave_HorizontalRule(p: HorizontalRule): Unit = {}
  protected def leave_Quotation_SimpleQuote(p: Quotation.SimpleQuote): Unit = {}
  protected def leave_Quotation_BlockQuote(p: Quotation.BlockQuote): Unit = {}
  protected def leave_Value(p: Value.Single): Unit = {}
  protected def leave_Value(p: Value.Multiple): Unit = {}
  protected def leave_Html5(p: Html5): Unit = leave_Html_Element(p)
  protected def leave_Html5Inline(p: Html5Inline): Unit = leave_Html_Element(p)
  protected def leave_Error(p: Error): Unit = leave_Html_Element(p)

  protected def leave_Html_Element(p: Dox): Unit = {
  }
}
