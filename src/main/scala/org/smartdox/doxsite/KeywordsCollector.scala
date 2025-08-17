package org.smartdox.doxsite

import java.net.URI
import org.goldenport.tree.TreeNode
import org.goldenport.collection.VectorMap
import org.smartdox.generator.Context
import org.smartdox.metadata._

/*
 * @since   Aug. 11, 2025
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
class KeywordsCollector(
  context: Context
) extends DoxSiteVisitor {
  import KeywordsCollector._

  private var _builder: Builder = Builder()

  override protected def enter_Content(node: TreeNode[Node], content: Node): Unit = {
    content match {
      case m: Page => ???
      case _ => {}
    }
  }
}

object KeywordsCollector {
  case class Builder() {
    def build(): Glossary = ???
  }
}
