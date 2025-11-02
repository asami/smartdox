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
 *  version Oct. 25, 2025
 * @version Nov.  3, 2025
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
//   val extractJsonldXsl =
//   """<?xml version="1.0" encoding="UTF-8"?>
//     |<xsl:stylesheet version="1.0"
//     |  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
//     |  <xsl:output method="xml" encoding="UTF-8" omit-xml-declaration="yes" indent="yes"/>
//     |  <xsl:template match="/">
//     |    <xsl:for-each select="//*[local-name()='script'
//     |       and contains(translate(@type,
//     |         'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
//     |         'abcdefghijklmnopqrstuvwxyz'),'ld+json')]">
//     |      <xsl:copy-of select="."/>
//     |      <xsl:text>&#10;</xsl:text>
//     |    </xsl:for-each>
//     |  </xsl:template>
//     |</xsl:stylesheet>
//     |""".stripMargin

//   val extractJsonldXsl0 = """<?xml version="1.0" encoding="UTF-8"?>
// <xsl:stylesheet version="1.0"
//   xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

//   <!-- Output raw HTML fragment -->
//   <xsl:output method="xml" encoding="UTF-8" omit-xml-declaration="yes"/>

//   <!-- Collect all JSON-LD <script> blocks from <head> -->
//   <xsl:template match="/">
//     <xsl:for-each select="
//       //*[local-name()='head']
//         /*[local-name()='script'
//            and translate(@type,
//              'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
//              'abcdefghijklmnopqrstuvwxyz')='application/ld+json']">
//       <xsl:text>&#10;</xsl:text>
//       <xsl:text>&lt;script type="application/ld+json"&gt;</xsl:text>
//       <xsl:value-of select="." disable-output-escaping="yes"/>
//       <xsl:text>&lt;/script&gt;</xsl:text>
//     </xsl:for-each>
//   </xsl:template>
// </xsl:stylesheet>
// """

//   val insertJsonldXsl = """<?xml version="1.0" encoding="UTF-8"?>
// <xsl:stylesheet version="1.0"
//   xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

//   <!-- File path to the JSON-LD fragment -->
//   <xsl:param name="jsonld-fragment" select="'jsonld-snippet.html'"/>

//   <xsl:output method="xml" encoding="UTF-8" indent="yes"/>

//   <!-- Copy everything by default -->
//   <xsl:template match="@*|node()">
//     <xsl:copy>
//       <xsl:apply-templates select="@*|node()"/>
//     </xsl:copy>
//   </xsl:template>

//   <!-- When we reach <head>, inject the fragment -->
//   <xsl:template match="*[local-name()='head']">
//     <xsl:copy>
//       <xsl:apply-templates select="@*|node()"/>
//       <!-- Import JSON-LD fragment -->
//       <xsl:text>&#10;</xsl:text>
//       <xsl:copy-of select="document($jsonld-fragment)/*"/>
//     </xsl:copy>
//   </xsl:template>

// </xsl:stylesheet>
// """

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
