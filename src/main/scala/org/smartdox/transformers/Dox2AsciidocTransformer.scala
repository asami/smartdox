package org.smartdox.transformers

import scalaz._, Scalaz._
import java.net.URI
import org.goldenport.RAISE
import org.goldenport.tree._
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.transformer._

/*
 * @since   Apr. 18, 2025
 *  version Apr. 29, 2025
 * @version Jun. 12, 2025
 * @author  ASAMI, Tomoharu
 */
class Dox2AsciidocTransformer(
  context: Context
) extends Dox2TextDocTransformer {
  import Dox2AsciidocTransformer._

  protected def section_Mark = "="

  // override def enter_Content(node: TreeNode[Dox], content: Dox) = content match {
  //   case m: Text => sb_print(m.contents)
  //   case m: Paragraph => // do nothing
  //   case m: Div => // do nothing
  //   case m: Span => // do nothing
  //   case m: Section => enter_section(m)
  //   case m: Document => // do nothing
  //   case m: Head => enter_head(m)
  //   case m: Body => // do nothing
  //   case m => RAISE.notImplementedYetDefect(s"Dox2AsciidocTransformer#start: $m")
  // }

  override protected def enter_Head(p: Head): Unit =
    p.title match {
      case Nil => // do nothing
      case xs => enter_asciidoc_section(to_text(xs))
    }
  protected final def enter_asciidoc_section(title: String): Unit = {
    section_up()
    sb_section_title(title)
  }

  protected final def leave_asciidoc_section(): Unit = {
    section_down()
  }
}

object Dox2AsciidocTransformer {
}
