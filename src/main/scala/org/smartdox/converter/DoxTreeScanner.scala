package org.smartdox.converter

import org.smartdox._

/*
 * @since   Nov. 18, 2025
 * @version Nov. 18, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxTreeScanner extends DoxTreeVisitor {
  override protected def enter_Text(p: Text): Unit = {}
  override protected def enter_Bold(p: Bold): Unit = {}
  override protected def enter_Italic(p: Italic): Unit = {}
  override protected def enter_Code(p: Code): Unit = {}
  override protected def enter_Ul(p: Ul): Unit = {}
  override protected def enter_Ol(p: Ol): Unit = {}
  override protected def enter_Li(p: Li): Unit = {}
  override protected def enter_Dl(p: Dl): Unit = {}
  override protected def enter_Dt(p: Dt): Unit = {}
  override protected def enter_Dd(p: Dd): Unit = {}
  override protected def enter_Hyperlink(p: Hyperlink): Unit = {}
  override protected def enter_Figure(p: Figure): Unit = {}
  override protected def enter_Img(p: Img): Unit = {}
  override protected def enter_Table(p: Table): Unit = {}
  override protected def enter_Section(p: Section): Unit = {}
  override protected def enter_I18NFragment(p: I18NFragment): Unit = {}
  override protected def enter_Program(p: Program): Unit = {}
  override protected def enter_Foot(p: Foot): Unit = {}
  override protected def enter_HorizontalRule(p: HorizontalRule): Unit = {}
  override protected def enter_Quotation_SimpleQuote(p: Quotation.SimpleQuote): Unit = {}
  override protected def enter_Quotation_BlockQuote(p: Quotation.BlockQuote): Unit = {}
  override protected def enter_Value(p: Value.Single): Unit = {}
  override protected def enter_Value(p: Value.Multiple): Unit = {}
  override protected def enter_Html_Element(p: Dox): Unit = {}
}
