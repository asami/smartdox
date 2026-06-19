package org.smartdox.generators

import java.io.File
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.{Data, ObjectData, StringData}
import org.goldenport.realm.RealmTransformer
import org.goldenport.tree.TreeNode
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.smartdox.generator._
import org.smartdox.doxsite.DoxSite
import org.smartdox.metadata.PublishMetadata
import org.smartdox.transformers.Dox2HtmlTransformer

/*
 * @since   Feb. 28, 2025
 *  version Mar. 12, 2025
 *  version Apr.  3, 2025
 *  version May. 24, 2025
 *  version Jun.  9, 2025
 *  version Oct. 25, 2025
 *  version May. 14, 2026
 * @version Jun. 19, 2026
 * @author  ASAMI, Tomoharu
 */
class DoxSiteGenerator(
  val context: Context,
  val config: DoxSite.Config,
  val publication: Option[File] = None,
  val publicationRepository: Option[File] = None,
  val publicationRdfMissingPolicy: String = "warn"
) extends GeneratorBase {
  import DoxSiteGenerator._

  def generate(realm: Realm): Realm = {
    val publishmetadata = PublishMetadata.load(publication)
    val videopublications = publishmetadata.map(_.videoPublications).getOrElse(Vector.empty)
    val publicationtriples = publishmetadata.map(_.videoRdfArtifactTriples(PublishMetadata.RdfMergeConfig(publicationRepository, publicationRdfMissingPolicy))).getOrElse(Vector.empty)
    val site = DoxSite.create(context, realm, Some("site"), config, Nil, videopublications, publicationtriples)
    val out = site.toRealm(context)
    val doxsite = PublishMetadata.publicRealm(publication).fold(out)(out + _)
    val r = Realm.create()
    r.merge("doxsite.d", doxsite)
  }

  // def generate0(realm: Realm): Realm = {
  //   val transformer = new DoxSiteTransformer(context)
  //   val x = realm.transform(transformer)
  //   val r = Realm.create()
  //   r.merge("doxsite.d", x)
  // }
}

object DoxSiteGenerator {
  // import org.goldenport.tree.{Tree, PlainTree, TreeNode}
  // import org.goldenport.realm.Realm

  // object DoxSiteRule extends RealmTransformer.Rule {
  //   def realmConfig = RealmTransformer.Config.empty
  //   override def getTargetName(p: TreeNode[Realm.Data]): Option[String] = {
  //     p.getNameSuffix.collect {
  //       case "dox" => s"${p.nameBody}.html"
  //       case "org" => s"${p.nameBody}.html"
  //       case "md" => s"${p.nameBody}.html"
  //       case "markdown" => s"${p.nameBody}.html"
  //     }
  //   }

  //   override def makeContent(oldname: String, newname: String, p: Realm.Data): Option[Realm.Data] = Some(p)
  // }

  // Unused
  // class DoxSiteTransformer(val context: Context) extends RealmTransformer {
  //   def realmTransformerContext = RealmTransformer.Context.default
  //   override def rule = DoxSiteRule

  //   override protected def make_Content(oldname: String, newname: String, p: Data): Option[Data] = p match {
  //     case m: StringData => _make_string(oldname, newname, m)
  //     case m: ObjectData => _make_object(m)
  //     case _ => None // println(s"DoxSiteGenerator#make_Content other: $p"); None
  //   }

  //   private def _make_string(oldname: String, newname: String, p: StringData) =
  //     StringUtils.getSuffixLowerCase(newname).flatMap {
  //       case "html" => _to_html(oldname, p)
  //       case _ => None
  //     }

  //   private def _to_html(oldname: String, p: StringData): Option[StringData] = {
  //     val dox = Dox2Parser.parseWithFilename(oldname, p.string)
  //     _make_html(dox)
  //   }

  //   private def _make_object(p: ObjectData) = p.o match {
  //     case m: Dox => _make_html(m)
  //     case _ => None
  //   }

  //   private def _make_html(p: Dox): Option[StringData] = {
  //     val rule = Dox2HtmlTransformer.Rule.noCss
  //     val r = for {
  //       html <- Dox2HtmlTransformer(context, rule).transform(p)
  //     } yield {
  //       StringData(html)
  //     }
  //     // println(s"Dox2HtmlGenerator#_make_object $r")
  //     r.toOption
  //   }
  // }
}
