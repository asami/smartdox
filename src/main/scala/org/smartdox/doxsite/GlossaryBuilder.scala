package org.smartdox.doxsite

import java.net.URI
import org.smartdox._
import org.smartdox.transformer._
import org.smartdox.metadata._
import org.goldenport.RAISE
import org.goldenport.tree._
import org.goldenport.values.CompactUuid

/*
 * @since   Mar.  7, 2025
 *  version Mar.  9, 2025
 *  version Apr. 28, 2025
 * @version Jun. 16, 2025
 * @author  ASAMI, Tomoharu
 */
class GlossaryBuilder(
  val context: DoxSiteTransformer.Context
) extends DoxSiteTransformer {
  import GlossaryBuilder._

  override protected def dox_Transformer(
    ctx: DoxSiteTransformer.Context,
    node: TreeNode[Node],
    p: Page
  ): HomoTreeTransformer[Dox] = RAISE.unsupportedOperationFault

  def glossary: Glossary = _glossaries
  private var _glossaries: Glossary = Glossary.empty

  override protected def make_Page(
    node: TreeNode[Node],
    p: Page
  ): TreeNode[Node] = {
    val ctx = TreeTransformer.Context.default[Dox]
    val path = new URI(node.pathname)
    val collector = new GlossaryCollector(ctx, path)
    val a = Dox.toTree(p.dox)
    // println(s"_collect_glossaries a: $a")
    val b = a.transform(collector)
    _glossaries = _glossaries + collector.glossary
    // println(s"_collect_glossaries b: ${b.show}")
    val c = Dox.toDox(b)
    // println(s"_collect_glossaries c: $c")
    TreeNode.create(node.name, Page(node.name, c))
  }
}

object GlossaryBuilder {
  class GlossaryCollector(
    context: TreeTransformer.Context[Dox],
    location: URI
  ) extends HomoTreeTransformer[Dox] {
    def treeTransformerContext = context

    def glossary: Glossary = _glossaries.build()
    private var _glossaries: Glossary.Builder = new Glossary.Builder()

    private def _generate_id: Dox.Id = Dox.Id(CompactUuid.generateString)

    override protected def make_Node(
      node: TreeNode[Dox],
      content: Dox
    ): TreeTransformer.Directive[Dox] = {
      // println(s"GlossaryCollector#make_Node: $node, $content")
      content match {
        case m: Dfn =>
          // println(s"match $m")
          val term = m.toPlainText
          m.attributes.get("id") match {
            case None =>
              val id = _generate_id
              val r = m.copy(attributes = m.attributes + ("id" -> id.id))
              val d = _make_description(node)
              _glossaries.add(term, location, id, d)
              TreeTransformer.Directive.ContainerContent(r)
            case Some(id) =>
              val d = _make_description(node)
              _glossaries.add(term, location, Dox.Id(id), d)
              TreeTransformer.Directive.ContainerContent(m)
          }
        case _ => TreeTransformer.Directive.Default[Dox]
      }
    }

    private def _make_description(node: TreeNode[Dox]): Dox = {
      val a = node.getParent
      a.flatMap(_.getContent) getOrElse Dox.text("Unknown")
    }
  }
}
