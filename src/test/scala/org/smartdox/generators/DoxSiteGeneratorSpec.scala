package org.smartdox.generators

import scalaz._, Scalaz._
import java.io.File
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.StringData
import org.smartdox.parser.UseDoxParser
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator._
import org.smartdox.semanticweb.Site.SiteMetadata

/*
 * @since   Mar.  2, 2025
 *  version Mar. 11, 2025
 *  version May.  2, 2025
 *  version Jun.  8, 2025
 *  version Aug. 16, 2025
 *  version May. 14, 2026
 * @version Jun. 19, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxSiteGeneratorSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDoxParser {
  "DoxSiteGenerator" should {
    val env = Environment.createJaJp()
    val cliconfig = CliConfig.buildJaJp()
    val config = Config(cliconfig)
    val ctx = new Context(env, config, env.contextFoundation)
    "typical" which {
      "adds configured site metadata to site.jsonld" in {
        val in = Realm.create(new File("src/test/resources/site-metadata"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default)
        val r = g.generate(in)
        val jsonld = r.get("doxsite.d/site.jsonld").collect {
          case m: StringData => m.string
        }.getOrElse("")
        jsonld should include ("schema:WebSite")
        jsonld should include (""""schema:name": "Example Site"""")
        jsonld should include (""""schema:alternateName": "Example"""")
        jsonld should include (""""schema:description": "Example site metadata."""")
        jsonld should include (""""schema:keywords": ["SmartDox", "Site Metadata"]""")
        jsonld should include (""""schema:datePublished": "2026-05-14"""")
      }

      "emits dashboard metadata" in {
        val in = Realm.create(new File("src/test/resources/site-single-locale-root"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default)
        val r = g.generate(in)
        val json = r.get("doxsite.d/metadata/dashboard/site.json").collect {
          case m: StringData => m.string
        }.getOrElse("")

        json should include ("\"counts\"")
        json should include ("\"rdf\"")
        json should include ("\"triple_count\"")
        json should include ("\"categories\"")
      }

      "keeps site metadata when configs are merged" in {
        val metadata = SiteMetadata(name = Some("Example Site"))
        val lhs = DoxSite.Config.default.copy(siteMetadata = metadata)
        val rhs = DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full)
        val merged = lhs + rhs
        merged.siteMetadata shouldBe metadata
        merged.strategy shouldBe DoxSite.Strategy.Full
      }

      "keeps simplemodelingorg antora output compatible with multi locale subdirectories" in {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new AntoraGenerator(ctx, DoxSite.Config.default, publication = Some(new File("src/test/resources/publish-fixture")))
        val r = g.generate(in)
        r.get("antora.d/en/antora-playbook.yml") should not be empty
        r.get("antora.d/ja/antora-playbook.yml") should not be empty
        r.get("antora.d/antora-playbook.yml") shouldBe empty
        val playbook = r.get("antora.d/ja/antora-playbook.yml").collect {
          case m: StringData => m.string
        }.getOrElse("")
        playbook should include ("title: SimpleModeling")
        playbook should include ("url: https://www.simplemodeling.org/ja/")
        playbook should include ("smartdox-site-navigation-mode: simplemodeling")
        playbook should include ("smartdox-site-language-toggle: 'true'")
      }

      "uses general defaults for category navigation metadata and single locale root antora output" in {
        val in = Realm.create(new File("src/test/resources/site-single-locale-root"))
        val g = new AntoraGenerator(ctx, DoxSite.Config.default)
        val r = g.generate(in)
        withClue(r.print) {
        r.get("antora.d/antora-playbook.yml") should not be empty
        r.get("antora.d/ja/antora-playbook.yml") shouldBe empty
        r.get("antora.d/en/antora-playbook.yml") shouldBe empty
        r.get("antora.d/supplemental-ui/partials/header-content.hbs") should not be empty
        }
        val playbook = r.get("antora.d/antora-playbook.yml").collect {
          case m: StringData => m.string
        }.getOrElse("")
        playbook should include ("title: KnowledgeHub BoK")
        playbook should include ("url: https://www.asamioffice.com/kokubunji/knowledgehub")
        playbook should not include ("/ja/")
        playbook should include ("smartdox-site-navigation-mode: category")
        playbook should include ("smartdox-site-language-toggle: 'false'")
        playbook should include ("supplemental_files: ./supplemental-ui")
        val header = r.get("antora.d/supplemental-ui/partials/header-content.hbs").collect {
          case m: StringData => m.string
        }.getOrElse("")
        header should include ("{{#each site.components}}")
        header should not include ("Overview")
        header should not include ("lang-btn")
      }

      "mini" ignore {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default)
        val r = g.generate(in)
        println(r.print)
      }
      "plain" ignore {
        val in = Realm.create(new File("src/test/resources/site1"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default)
        val r = g.generate(in)
        println(r.print)
      }
      "dfn" ignore {
        val in = Realm.create(new File("src/test/resources/site-dfn"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default)
        val r = g.generate(in)
        println(r.print)
      }
    }
    "publication registry" which {
      val publication = Some(new File("src/test/resources/publish-fixture"))

      "generates public metadata from publication bundles" in {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new DoxSiteGenerator(ctx, DoxSite.Config.default, publication)
        val r = g.generate(in)
        r.get("doxsite.d/metadata/catalog/projects/textus-tutorial.json") should not be empty
        r.get("doxsite.d/metadata/samples/textus-tutorial/metadata.json") should not be empty
        r.get("doxsite.d/metadata/artifacts/repository/textus-core.json") should not be empty
        r.get("doxsite.d/metadata/source-manifest/textus-tutorial.json") should not be empty
        r.get("doxsite.d/textus-tutorial.json") shouldBe empty
      }

      "generates antora pages from publication paths and repository fallback" in {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new AntoraGenerator(ctx, DoxSite.Config.default, publication)
        val r = g.generate(in)
        r.get("antora.d/en/docs/textus/modules/tutorial/pages/index.adoc") should not be empty
        r.get("antora.d/en/docs/textus/modules/tutorial/pages/textus-tutorial/index.adoc") should not be empty
        r.get("antora.d/en/docs/textus/modules/tutorial/pages/textus-tutorial/downloads.adoc") should not be empty
        val entutorial = r.get("antora.d/en/docs/textus/modules/tutorial/pages/index.adoc").collect {
          case m: StringData => m.string
        }.getOrElse("")
        entutorial should include ("Tutorial collections for learning Textus.")
        val jatutorial = r.get("antora.d/ja/docs/textus/modules/tutorial/pages/index.adoc").collect {
          case m: StringData => m.string
        }.getOrElse("")
        jatutorial should include ("Textus を学ぶためのチュートリアル一覧です。")
        jatutorial should include ("link:textus-tutorial/index.html[Textus チュートリアル]")
        val jatutorialdetail = r.get("antora.d/ja/docs/textus/modules/tutorial/pages/textus-tutorial/index.adoc").collect {
          case m: StringData => m.string
        }.getOrElse("")
        jatutorialdetail should include ("最小の実行可能な Textus サンプルです。")
        jatutorialdetail should include ("== 個々のチュートリアル")
        jatutorialdetail should not include ("This page is the entry point")
        jatutorialdetail should not include ("|Kind|")
        jatutorialdetail should not include ("|Resource path|")
        val jadownloads = r.get("antora.d/ja/docs/textus/modules/tutorial/pages/textus-tutorial/downloads.adoc").collect {
          case m: StringData => m.string
        }.getOrElse("")
        jadownloads should include ("全体をまとめて使う場合は全体ダウンロード")
        jadownloads should include ("== 全体ダウンロード")
        jadownloads should include ("== 個々の項目のダウンロード")
        jadownloads should include ("|パッケージ|バージョン|ダウンロード")
        jadownloads should include ("|項目|バージョン|ダウンロード")
        jadownloads should include ("link:/repository/download/textus/tutorial/textus-tutorial/0.1.0/textus-tutorial-0.1.0.zip[ダウンロード]")
        jadownloads should include ("link:/repository/download/textus/tutorial/textus-tutorial/0.1.0/01-minimal/01-minimal-0.1.0.zip[ダウンロード]")
        jadownloads should not include ("Choose the complete tutorial package")
        val jatextusnav = r.get("antora.d/ja/docs/textus/modules/tutorial/nav.adoc").collect {
          case m: StringData => m.string
        }.getOrElse("")
        jatextusnav should include ("Textus チュートリアル")
        jatextusnav should not include ("T T")
        r.get("antora.d/en/docs/catalog/modules/textus-tutorial/pages/index.adoc") should not be empty
        r.get("antora.d/en/docs/catalog/modules/textus-tutorial/pages/metadata.adoc") should not be empty
        r.get("antora.d/ja/docs/catalog/modules/textus-tutorial/pages/source-manifest.adoc") should not be empty
        r.get("antora.d/en/docs/repository/modules/textus-core/pages/index.adoc") should not be empty
        r.get("antora.d/en/docs/catalog/modules/textus-core/pages/artifacts.adoc") should not be empty
        r.get("antora.d/en/docs/catalog/modules/textus-core/pages/releases.adoc") should not be empty
        r.get("antora.d/en/docs/repository/modules/09-b-aggregate-relation-boundary-model/pages/index.adoc") shouldBe empty
      }

      "renders .video source package index as slug article" in {
        val in = Realm.create(new File("src/test/resources/video-package-site"))
        val g = new AntoraGenerator(ctx, DoxSite.Config.default, Some(new File("src/test/resources/video-publication-fixture")))
        val r = g.generate(in)
        val article = r.get("antora.d/docs/concepts/modules/ROOT/pages/tutorial.adoc").collect {
          case m: StringData => m.string
        }.getOrElse("")
        article should include ("Video Tutorial")
        article should include ("This is a video article.")
        article should include ("pass:[<video")
        article should include ("src=\"/repository/video/tutorial/0.1.0/tutorial-0.1.0.mp4\"")
        r.get("antora.d/docs/concepts/modules/ROOT/pages/tutorial.video/index.adoc") shouldBe empty
      }

      "fails on invalid metadata syntax" in {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new AntoraGenerator(ctx, DoxSite.Config.default, Some(new File("src/test/resources/publish-invalid-fixture")))
        intercept[IllegalArgumentException] {
          g.generate(in)
        }.getMessage should include ("broken.json")
      }

      "fails on invalid publication path" in {
        val in = Realm.create(new File("src/test/resources/site-mini"))
        val g = new AntoraGenerator(ctx, DoxSite.Config.default, Some(new File("src/test/resources/publish-invalid-path-fixture")))
        intercept[IllegalArgumentException] {
          g.generate(in)
        }.getMessage should include ("publication.path")
      }
    }
  }
}
