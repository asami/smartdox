package org.smartdox.generators

import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.{Data, ObjectData, StringData}
import org.goldenport.realm.RealmTransformer
import org.goldenport.tree.TreeNode
import org.smartdox._
import org.smartdox.generator._
import org.smartdox.transformers.Dox2BloggerTransformer

/*
 * @since   Jan. 11, 2021
 *  version Jan. 12, 2021
 *  version Feb.  5, 2021
 * @version Aug.  3, 2023
 * @author  ASAMI, Tomoharu
 */
class Dox2BloggerGenerator(
  val context: Context
) extends GeneratorBase {
  import Dox2BloggerGenerator._

  def generate(realm: Realm): Realm = {
    val transformer = new BloggerTransformer(context)
    val x = realm.transform(transformer)
    val r = Realm.create()
    r.merge("blogger.d", x)
  }
}

object Dox2BloggerGenerator {
  import org.goldenport.tree.{Tree, PlainTree, TreeNode}
  import org.goldenport.realm.Realm

  object Dox2BloggerRule extends RealmTransformer.Rule {
    def getTargetName(p: TreeNode[Realm.Data]): Option[String] = {
      p.getNameSuffix.collect {
        case "dox" => s"${p.nameBody}.blogger"
        case "org" => s"${p.nameBody}.blogger"
      }
    }

    def mapContent(p: Realm.Data): Realm.Data = p
  }

  class BloggerTransformer(val context: Context) extends RealmTransformer {
    def treeTransformerContext = RealmTransformer.Context.default
    def rule = Dox2BloggerRule

    override protected def make_Content(p: Data): Option[Data] = p match {
      case m: ObjectData => _make_object(m)
      case _ => None // println(s"Dox2BloggerGenerator#make_Content other: $p"); None
    }

    private def _make_object(p: ObjectData) = p.o match {
      case m: Dox =>
        val r = for {
          html <- Dox2BloggerTransformer(context).transform(m)
        } yield {
          StringData(html)
        }
        // println(s"Dox2HtmlGenerator#_make_object $r")
        r.toOption
      case _ => None
    }
  }
}
