package org.smartdox.doxsite

import java.net.URI
import org.goldenport.tree.Tree
import org.goldenport.tree.TreeNode
import org.goldenport.collection.VectorMap
import org.goldenport.values.PathName
import org.goldenport.util.StringUtils
import org.smartdox.generator.Context
import org.smartdox.metadata._

/*
 * @since   Aug. 11, 2025
 * @version Aug. 22, 2025
 * @author  ASAMI, Tomoharu
 */
class GlossaryCollector(
  context: Context
) extends DoxSiteVisitor {
  import GlossaryCollector._

  private val _builder: Glossary.Builder = new Glossary.Builder()

  override protected def enter_Content(node: TreeNode[Node], content: Node): Unit = {
    content match {
      case m: Page => _collect_glossary(node.pathnameValue, m)
      case _ => {}
    }
  }

  private def _collect_glossary(pathname: PathName, p: Page): Unit = {
    pathname.components match {
      case Nil => Unit
      case _ :: leaf :: Nil => _collect_glossary(leaf, p)
      case _ :: (xs :+ leaf) => _collect_glossary(xs, leaf, p)
    }
  }

  private def _collect_glossary(leaf: String, p: Page) = {
    val name = StringUtils.toPathnameBody(leaf)
    _builder.register(name, p.dox)
  }

  private def _collect_glossary(tagpath: List[String], leaf: String, p: Page) = {
    val name = StringUtils.toPathnameBody(leaf)
    val tag = Tag.TagName(tagpath)
    _builder.register(name, tag, p.dox)
  }

  def glossary(): Glossary = _builder.build()
}

object GlossaryCollector {
  val PROP_GLOSSARY_DIRECTORY = "glossary"

  def collect(context: Context, site: Tree[Node]): Glossary = {
    val gc = new GlossaryCollector(context)
    site.traverse(PROP_GLOSSARY_DIRECTORY, gc)
    gc.glossary()
  }
}
