package org.smartdox.doxsite

import java.net.URI
import org.goldenport.tree.TreeNode
import org.goldenport.collection.VectorMap
import org.smartdox.generator.Context
import org.smartdox.metadata._

/*
 * @since   Jun. 23, 2025
 * @version Jun. 24, 2025
 * @author  ASAMI, Tomoharu
 */
class CategoryCollector(
  context: Context
) extends DoxSiteVisitor {
  import CategoryCollector._

  private var _categories: VectorMap[String, Category] = VectorMap.empty

  def categories: CategoryCollection = CategoryCollection(_categories)

  override protected def enter_Content(node: TreeNode[Node], content: Node): Unit = {
    content match {
      case m: CategoryMetaData => _categories = _categories + (m.category.name.name -> m.category)
      case _ => {}
    }
  }
}

object CategoryCollector {
}
