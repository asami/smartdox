package org.smartdox.doxsite

import scalaz.{Tree => ZTree}
import java.io.File
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.tree.Tree
import org.goldenport.tree.TreeNode
import org.goldenport.tree.TreeCursor
import org.goldenport.tree.TreeTransformer
import org.goldenport.tree.HomoTreeTransformer
import org.goldenport.tree.TreeVisitor
import org.goldenport.tree.ControlTreeNode
import org.goldenport.realm.Realm
import org.goldenport.realm.RealmTransformer
import org.goldenport.values.CompactUuid
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.smartdox.metadata.MetaData
import org.smartdox.metadata.Glossary
import org.smartdox.generator.Context
import org.smartdox.transformers.Dox2HtmlTransformer

/*
 * @since   Feb. 23, 2025
 *  version Feb. 25, 2025
 *  version Mar.  9, 2025
 * @version Apr.  3, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSite(
  space: Tree[Node],
  metadata: MetaData
) {
  import DoxSite._

  def toRealm(context: Context): Realm = {
    val ctx = TreeTransformer.Context.default[Realm.Data]
    val a = space.transform(new RealmBuilder(context, ctx))
    Realm(a)
  }
}

object DoxSite {
  class DoxSiteBuilder(
    context: TreeTransformer.Context[Node]
  ) extends TreeTransformer[Realm.Data, Node] {
    def treeTransformerContext = context
    override def rule = DoxSiteBuilder.Rule

    override protected def make_Node(
      oldname: String, // unused
      newname: String, // unused
      node: TreeNode[Realm.Data],
      content: Realm.Data
    ): TreeTransformer.Directive[Node] = {
      println("S: " + content)
      content match {
        case Realm.EmptyData => TreeTransformer.Directive.Default[Node]
        case m: Realm.StringData => _build_file(node, m)
        case m: Realm.UrlData => TreeTransformer.Directive.Default[Node]
        case m: Realm.FileData => TreeTransformer.Directive.Default[Node]
        case m: Realm.BagData => TreeTransformer.Directive.Default[Node]
        case m: Realm.ObjectData => TreeTransformer.Directive.Default[Node]
        case m: Realm.ApplicationData => TreeTransformer.Directive.Default[Node]
      }
    }

    private def _build_file(node: TreeNode[Realm.Data], p: Realm.StringData): TreeTransformer.Directive[Node] =
      _build_file(node, p.string)

    private def _build_file(node: TreeNode[Realm.Data], p: String): TreeTransformer.Directive[Node] =
      node.getNameSuffix match {
        case None => TreeTransformer.Directive.Default[Node]
        case Some(s) =>
          node.getContent match {
            case None => TreeTransformer.Directive.Default[Node]
            case Some(c) => 
              c match {
                case m: Realm.StringData =>
                  val name = node.name
                  val r: List[Node] = s match {
                    case "dox" => _dox_page(name, m.string)
                    case "org" => _org_page(name, m.string)
                    case "md" => _markdown_page(name, m.string)
                    case "markdown" => _markdown_page(name, m.string)
                    case _ => Nil
                  }
                  r match {
                    case Nil => TreeTransformer.Directive.Default[Node]
                    case m :: Nil => TreeTransformer.Directive.Node(TreeNode.create(m.name.name, m))
                    case ms => TreeTransformer.Directive.Nodes(ms.map(x => TreeNode.create(x.name.name, x)))
                  }
                case _ => TreeTransformer.Directive.Default[Node]
              }
          }
      }

    private def _dox_page(name: String, c: String) = {
      println(s"_dox_page: $c")
      val dox = Dox2Parser.parse(c)
      List(Page(name, dox))
    }

    private def _org_page(name: String, c: String) = {
      val dox = Dox2Parser.parse(c)
      List(Page(name, dox))
    }

    private def _markdown_page(name: String, c: String) = {
      val dox = Dox2Parser.parse(c)
      List(Page(name, dox))
    }
  }
  object DoxSiteBuilder {
    object Rule extends TreeTransformer.Rule[Realm.Data, Node] {
      def getTargetName(p: TreeNode[Realm.Data]): Option[String] = {
        p.getNameSuffix.collect {
          case "dox" => s"${p.nameBody}.dox"
          case "org" => s"${p.nameBody}.dox"
          case "md" => s"${p.nameBody}.dox"
          case "markdown" => s"${p.nameBody}.dox"
        }
      }
    }
  }

  class RealmBuilder(
    gcontext: Context,
    context: TreeTransformer.Context[Realm.Data]
  ) extends TreeTransformer[Node, Realm.Data] {
    def treeTransformerContext = context
    override def rule = RealmBuilder.Rule

    override protected def make_Node(
      node: TreeNode[Node],
      content: Node
    ): TreeTransformer.Directive[Realm.Data] = {
      content match {
        case m: Page => _to_html(m) // TreeTransformer.Directive.Content(m.toRealmData)
      }
    }

    private def _to_html(p: Page): TreeTransformer.Directive.LeafNode[Realm.Data] = {
      val rule = Dox2HtmlTransformer.Rule.noCss
      val s = Consequence.from(Dox2HtmlTransformer(gcontext, rule).transform(p.dox)).
        foldConclusion(_.message)
      val data = Realm.StringData(s)
      val name = StringUtils.changeSuffix(p.name.name, "html")
      TreeTransformer.Directive.LeafNode(name, data)
    }
  }
  object RealmBuilder {
    object Rule extends TreeTransformer.Rule[Node, Realm.Data] {
      def getTargetName(p: TreeNode[Node]): Option[String] = None
    }
  }

  def create(context: Context, file: File): DoxSite = {
    val a = Realm.create(file)
    create(context, a)
  }

  def create(context: Context, realm: Realm): DoxSite = {
    val nodectx = TreeTransformer.Context.default[Node]
    val doxctx = TreeTransformer.Context.default[Dox]
    val ctx = DoxSiteTransformer.Context(nodectx, doxctx)
    val a: Tree[Node] = realm.transformTree(new DoxSiteBuilder(nodectx))
    // println(a.show)
    val gb = new GlossaryBuilder(ctx)
    val b: Tree[Node] = a.transform(gb)
//    val b = a
    val glossary = gb.glossary
    // println(s"Glossary: $glossary")
    val metadata = MetaData(glossary = glossary)
    val ctx1 = ctx.withMetaData(metadata)
    val c: Tree[Node] = b.transform(new LinkEnabler(ctx1))
    // println(s"Z: ${c.print}")
    new DoxSite(c, metadata)
  }
}
