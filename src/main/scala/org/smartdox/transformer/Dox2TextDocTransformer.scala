package org.smartdox.transformer

import org.smartdox._

/*
 * @since   Jun. 12, 2025
 * @version Jun. 12, 2025
 * @author  ASAMI, Tomoharu
 */
trait Dox2TextDocTransformer extends Dox2StringTransformer {
  protected def section_Mark: String

  protected final def sb_section_title(title: String): Unit =
    sb_section_title(section_Mark, title)

  override protected def enter_Section(p: Section): Unit =
    sb_section_title(to_text(p.title))

  override def leave_Paragraph(p: Paragraph) = sb_println()
}
