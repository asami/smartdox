package org.smartdox.converter

import scalaz._
import Scalaz._
import java.net.URI
import org.goldenport.context.Consequence
import org.goldenport.tree._
import org.goldenport.util.StringBuildFeature
import org.smartdox._
import Dox._

/*
 * @since   Jan. 12, 2012
 *  version Apr. 27, 2025
 *  version Jun. 18, 2025
 * @version Jul. 15, 2025
 * @author  ASAMI, Tomoharu
 */
trait Dox2StringConverter extends DoxTreeVisitor with StringBuildFeature {
  def convert(dox: Dox): Consequence[String] = Consequence {
    val tree = Dox.toTree(dox)
    tree.traverse(this)
    sb_to_string()
  }

  override protected def enter_Text(p: Text): Unit = {
    sb_print(p.contents)
  }

  protected final def sb_section_title(mark: String, title: String): Unit = {
    sb_println(s"${section_bar(mark)} $title")
    sb_println()
  }

  protected final def sb_print(p: InlineContents): Unit =
    sb_print(Dox.toPlainText(p))

  protected final def sb_println(p: InlineContents): Unit =
    sb_println(Dox.toPlainText(p))
}
