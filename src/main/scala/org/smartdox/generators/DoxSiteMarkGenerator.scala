package org.smartdox.generators

import scala.collection.JavaConverters._
import java.io.File
import java.util.Locale
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document => JDocument, Element => JElement}
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.{Data, ObjectData, StringData}
import org.goldenport.realm.RealmTransformer
import org.goldenport.tree.TreeNode
import org.goldenport.tree.TreeTransformer
import org.goldenport.xml.dom.DomUtils
import org.goldenport.i18n.LocaleUtils
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.generator._
import org.smartdox.doxsite.DoxSite

/*
 * @since   Oct. 25, 2025
 * @version Nov.  6, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSiteMarkGenerator(
  val context: Context,
  val config: DoxSite.Config
) extends GeneratorBase {
  import DoxSiteMarkGenerator._

  def generate(realm: Realm): Realm = {
    val site = DoxSite.create(context, realm, "site", config)
    val source = site.toRealm(context)
    val target = Realm.create(new File("website.d"))
    val realmcontext = RealmTransformer.Context.default
    val marked = target.transform(new DoxSiteMarker(realmcontext, source))
    val r = Realm.create()
    r.merge("website.d", marked)
  }
}

object DoxSiteMarkGenerator {
  class DoxSiteMarker(
    val realmTransformerContext: RealmTransformer.Context,
    val source: Realm
  ) extends RealmTransformer {
    override protected def make_Node(
      node: TreeNode[Realm.Data],
      content: Realm.Data
    ): TreeTransformer.Directive[Realm.Data] = {
      val pathname = node.pathname
      StringUtils.getSuffix(pathname) match {
        case Some(s) => s match {
          case "html" =>
//            println(pathname)
            source.get(pathname) match {
              case Some(s) =>
//                println(s" => $pathname")
                val locale = _locale(pathname)
                _mark(locale, content, s) match {
                  case Some(ss) => directive_leaf(ss)
                  case None => directive_empty()
                }
              case None => directive_empty()
            }
          case _ => directive_empty()
        }
        case None => directive_default()
      }
    }

    private def _locale(pathname: String) =
      if (pathname.contains("/ja/"))
        Some(LocaleUtils.ja)
      else if (pathname.contains("/en/"))
        Some(LocaleUtils.en)
      else
        None

    private def _mark(
      locale: Option[Locale],
      content: Realm.Data,
      source: Realm.Data
    ): Option[Realm.Data] =
      (content, source) match {
        case (c: Realm.StringData, s: Realm.StringData) => _mark(locale, c.string, s.string)
        case _ => None
      }

    private def _mark(
      locale: Option[Locale],
      target: String,
      source: String
    ): Option[Realm.StringData] = {
      // --- Parse both documents ---
      val sourcedoc = Jsoup.parse(source)
      val targetdoc = Jsoup.parse(target)

      locale.foreach { loc =>
        val langCode = loc.getLanguage
        val htmlEl = targetdoc.selectFirst("html")
        if (htmlEl != null) {
          htmlEl.attr("lang", langCode)
        } else {
          targetdoc.prependElement("html").attr("lang", langCode)
        }
      }

      // --- Extract all <script type="application/ld+json"> from source ---
      val jsonldscripts =
        sourcedoc.select("script[type=application/ld+json]").asScala.toSeq.map(_.clone())

      if (jsonldscripts.isEmpty)
        None
      else
        Some(_mark(targetdoc, jsonldscripts))
    }

    private def _mark(targetdoc: JDocument, jsonldscripts: Seq[JElement]) = {
      // --- Get or create <head> in target ---
      val head = Option(targetdoc.head()).getOrElse(targetdoc.appendElement("head"))

      // --- Append all extracted scripts ---
      jsonldscripts.foreach(head.appendChild)

      // --- Pretty XML-style output (for safety) ---
      targetdoc.outputSettings()
        .syntax(JDocument.OutputSettings.Syntax.html)
        .prettyPrint(true)
        .indentAmount(2)

      val r = targetdoc.outerHtml()
      Realm.StringData(r)
    }
  }
}
