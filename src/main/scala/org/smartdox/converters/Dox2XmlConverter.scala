package org.smartdox.converters

import org.w3c.dom.Node
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.converter.Dox2StringConverter
// import org.smartdox.transformers.Dox2DomHtmlTransformer
// import org.smartdox.transformers.HtmlTransformerBase

/*
 * @since   Jul. 23, 2025
 *  version Jul. 26, 2025
 * @version Aug. 25, 2025
 * @author  ASAMI, Tomoharu
 */
class Dox2XmlConverter(
  context: Context
) extends Dox2StringConverter {
  override protected def enter_Text(p: Text): Unit = sb_print(p.xmlString)

  override protected def enter_Paragraph(p: Paragraph): Unit = p.printOpen(sb_buffer)

  override protected def enter_Div(p: Div): Unit = p.printOpen(sb_buffer)

  override protected def enter_Span(p: Span): Unit = p.printOpen(sb_buffer)

  override protected def enter_Bold(p: Bold): Unit = p.printOpen(sb_buffer)

  override protected def enter_Italic(p: Italic): Unit = p.printOpen(sb_buffer)

  override protected def enter_Ul(p: Ul): Unit = p.printOpen(sb_buffer)

  override protected def enter_Ol(p: Ol): Unit = p.printOpen(sb_buffer)

  override protected def enter_Li(p: Li): Unit = p.printOpen(sb_buffer)

  override protected def enter_Dl(p: Dl): Unit = p.printOpen(sb_buffer)

  override protected def enter_Dt(p: Dt): Unit = p.printOpen(sb_buffer)

  override protected def enter_Dd(p: Dd): Unit = p.printOpen(sb_buffer)

  override protected def enter_Hyperlink(p: Hyperlink): Unit = p.printOpen(sb_buffer)

  override protected def enter_Figure(p: Figure): Unit = p.printOpen(sb_buffer)

  override protected def enter_Figcaption(p: Figcaption): Unit = p.printOpen(sb_buffer)

  override protected def enter_Img(p: Img): Unit = p.printOpen(sb_buffer)

  override protected def enter_Table(p: Table): Unit = p.printOpen(sb_buffer)

  override protected def enter_Thead(p: THead): Unit = p.printOpen(sb_buffer)

  override protected def enter_Tbody(p: TBody): Unit = p.printOpen(sb_buffer)

  override protected def enter_Tfoot(p: TFoot): Unit = p.printOpen(sb_buffer)

  override protected def enter_Caption(p: Caption): Unit = p.printOpen(sb_buffer)

  override protected def enter_Tr(p: TR): Unit = p.printOpen(sb_buffer)

  override protected def enter_Th(p: TH): Unit = p.printOpen(sb_buffer)

  override protected def enter_Td(p: TD): Unit = p.printOpen(sb_buffer)

  override protected def enter_Section(p: Section): Unit = p.printOpen(sb_buffer)

  override protected def enter_I18NFragment(p: I18NFragment): Unit = p.printOpen(sb_buffer)

  override protected def enter_Program(p: Program): Unit = p.printOpen(sb_buffer)

  override protected def enter_Document(p: Document): Unit = p.printOpen(sb_buffer)

  override protected def enter_Head(p: Head): Unit = p.printOpen(sb_buffer)

  override protected def enter_Body(p: Body): Unit = p.printOpen(sb_buffer)

  override protected def leave_Text(p: Text): Unit = {}
  override protected def leave_Paragraph(p: Paragraph): Unit = p.printClose(sb_buffer)
  override protected def leave_Div(p: Div): Unit = p.printClose(sb_buffer)
  override protected def leave_Span(p: Span): Unit = p.printClose(sb_buffer)
  override protected def leave_Bold(p: Bold): Unit = p.printClose(sb_buffer)
  override protected def leave_Italic(p: Italic): Unit = p.printClose(sb_buffer)
  override protected def leave_Ul(p: Ul): Unit = p.printClose(sb_buffer)
  override protected def leave_Ol(p: Ol): Unit = p.printClose(sb_buffer)
  override protected def leave_Li(p: Li): Unit = p.printClose(sb_buffer)
  override protected def leave_Dl(p: Dl): Unit = p.printClose(sb_buffer)
  override protected def leave_Dt(p: Dt): Unit = p.printClose(sb_buffer)
  override protected def leave_Dd(p: Dd): Unit = p.printClose(sb_buffer)
  override protected def leave_Hyperlink(p: Hyperlink): Unit = p.printClose(sb_buffer)
  override protected def leave_Figure(p: Figure): Unit = p.printClose(sb_buffer)
  override protected def leave_Figcaption(p: Figcaption): Unit = p.printClose(sb_buffer)
  override protected def leave_Img(p: Img): Unit = p.printClose(sb_buffer)
  override protected def leave_Table(p: Table): Unit = p.printClose(sb_buffer)
  override protected def leave_Thead(p: THead): Unit = p.printClose(sb_buffer)
  override protected def leave_Tbody(p: TBody): Unit = p.printClose(sb_buffer)
  override protected def leave_Tfoot(p: TFoot): Unit = p.printClose(sb_buffer)
  override protected def leave_Tr(p: TR): Unit = p.printClose(sb_buffer)
  override protected def leave_Th(p: TH): Unit = p.printClose(sb_buffer)
  override protected def leave_Td(p: TD): Unit = p.printClose(sb_buffer)
  override protected def leave_Caption(p: Caption): Unit = p.printClose(sb_buffer)
  override protected def leave_Section(p: Section): Unit = p.printClose(sb_buffer)
  override protected def leave_I18NFragment(p: I18NFragment): Unit = p.printClose(sb_buffer)
  override protected def leave_Program(p: Program): Unit = p.printClose(sb_buffer)
  override protected def leave_Document(p: Document): Unit = p.printClose(sb_buffer)
  override protected def leave_Head(p: Head): Unit = p.printClose(sb_buffer)
  override protected def leave_Body(p: Body): Unit = p.printClose(sb_buffer)

  // def isDocument: Boolean = true
  // def isPretty: Boolean = false

  // def convert(dox: Dox): Consequence[String] = {
  //   val domrule = Dox2DomHtmlTransformer.Rule.empty
  //   for {
  //     dom <- new Dox2DomHtmlTransformer(context, domrule).transformC(dox)
  //     r <- _to_xml(dom)
  //   } yield r
  // }

  // private def _to_xml(p: Node): Consequence[String] = Consequence(to_html(p))
}

object Dox2XmlConverter {
}
