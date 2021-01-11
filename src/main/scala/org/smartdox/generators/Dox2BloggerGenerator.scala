package org.smartdox.generators

import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.{Data, ObjectData, StringData}
import org.goldenport.realm.RealmTransformer
import org.goldenport.tree.TreeNode
import org.smartdox._
import org.smartdox.generator._
import org.smartdox.transformers.Dox2BloggerTransformer

/*
 * Derived from SmartDox2BloggerRealmGenerator.
 *
 * @since   Jan. 11, 2012
 *  version Sep.  8, 2012
 * @version Jan. 11, 2021
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
    r.merge("html.d", x)
  }
}

object Dox2BloggerGenerator {
  import org.goldenport.tree.{Tree, PlainTree, TreeNode}
  import org.goldenport.realm.Realm

  object Dox2BloggerRule extends RealmTransformer.Rule {
    def getTargetName(p: TreeNode[Realm.Data]): Option[String] = {
      p.getNameSuffix.collect {
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
      case _ => println(s"Dox2BloggerGenerator#make_Content other: $p"); None
    }

    private def _make_object(p: ObjectData) = p.o match {
      case m: Dox =>
        val r = for {
          html <- Dox2BloggerTransformer(context).transform(m)
        } yield {
          StringData(html)
        }
        println(s"Dox2HtmlGenerator#_make_object $r")
        r.toOption
      case _ => None
    }
  }
}
