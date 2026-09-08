package org.smartdox.generators

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Locale
import org.yaml.snakeyaml.Yaml
import scala.collection.JavaConverters._
import org.junit.runner.RunWith
import org.goldenport.cli.{Config => CliConfig, Environment}
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.StringData
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.goldenport.collection.VectorMap
import org.smartdox._
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator.{Config, Context}
import org.smartdox.metadata.PublishMetadata

/*
 * @since   Aug.  4, 2026
 * @version Aug. 29, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ArticleMediaProjectionSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Article media projection" should {
    "locale, schema, and availability" which {
      "project exact English and Japanese published external variants into matching article and Notice URLs" in {
      Given("one locale-specific article-media registry and a published article")
      val input = Realm.create(DoxSite.realmConfig, new File("src/test/resources/article-media-projection-site"))
      val publication = Some(new File("src/test/resources/article-media-projection-publication"))

      When("DoxSite and Antora generate the localized projections")
      val site = new DoxSiteGenerator(_context, DoxSite.Config.default, publication).generate(input)
      val antora = new AntoraGenerator(_context, DoxSite.Config.default, publication).generate(input)
      val englishnotice = _notice(site, "doxsite.d/WEB-INF/data/en", "development-process/example.html")
      val englishcategorynotice = _notice(site, "doxsite.d/WEB-INF/data/en/development-process", "development-process/example.html")
      val japanesenotice = _notice(site, "doxsite.d/WEB-INF/data/ja", "development-process/example.html")
      val japanesecategorynotice = _notice(site, "doxsite.d/WEB-INF/data/ja/development-process", "development-process/example.html")

      Then("each exact locale receives only its own PDF and video controls with verbatim or local-default labels")
      val englisharticle = _string(site, "doxsite.d/en/development-process/example.html")
      val japanesearticle = _string(site, "doxsite.d/ja/development-process/example.html")
      englisharticle should include ("/en/development-process/pdf/example-article.pdf")
      englisharticle should include ("Article PDF")
      englisharticle should not include ("  Read the full English article  ")
      englisharticle should include ("/en/development-process/pdf/example-summary-slides.pdf")
      englisharticle should include ("Summary slides PDF")
      englisharticle should include ("smartdox-article-header")
      englisharticle should include ("smartdox-article-header-metadata")
      englisharticle should include ("smartdox-article-header-actions")
      englisharticle should not include ("smartdox-article-media")
      englisharticle should include ("https://example.com/watch-en")
      englisharticle should include ("Watch video")
      englisharticle should include ("smartdox-article-header-action")
      englisharticle.indexOf("https://example.com/watch-en") should be < englisharticle.indexOf("/en/development-process/pdf/example-summary-slides.pdf")
      englisharticle.indexOf("/en/development-process/pdf/example-summary-slides.pdf") should be < englisharticle.indexOf("/en/development-process/pdf/example-article.pdf")
      englisharticle.indexOf("/en/development-process/pdf/example-article.pdf") should be < englisharticle.indexOf("#smartdox-article-infographic")
      englisharticle should include ("smartdox-article-infographic")
      englisharticle should include ("modeling")
      englisharticle should include ("ai-collaboration")
      japanesearticle should include ("/ja/development-process/pdf/example-article.pdf")
      japanesearticle should include ("記事 PDF")
      japanesearticle should not include ("  記事 PDF  ")
      japanesearticle should not include ("summary-slides.pdf")
      japanesearticle should not include ("要約スライド PDF")
      japanesearticle should include ("https://example.com/watch-ja")
      japanesearticle should include ("動画を見る")
      japanesearticle should include ("インフォグラフィックを見る")
      japanesearticle should include ("smartdox-article-header")
      japanesearticle should not include ("smartdox-article-media")
      japanesearticle should not include ("watch-en")
      _string(antora, "antora.d/docs/development-process/modules/ROOT/pages/example.adoc") should include ("https://example.com/watch-ja")
      _string(antora, "antora.d/docs/development-process/modules/ROOT/pages/example.adoc") should not include ("https://example.com/watch-en")
      _string(antora, "antora.d/docs/development-process/modules/ROOT/pages/example.adoc") should include ("/ja/development-process/pdf/example-article.pdf")
      _string(antora, "antora.d/docs/development-process/modules/ROOT/pages/example.adoc") should include ("  記事 PDF  ")

      And("Notice media preserves the exact role maps and shares each locale result globally and by category")
      _media(englishnotice) shouldBe Some(Map(
        "infographic" -> Map(
          "public_path" -> "/en/development-process/images/example.png",
          "media_type" -> "image/png",
          "alt" -> "English infographic"
        ),
        "article_pdf" -> Map(
          "public_path" -> "/en/development-process/pdf/example-article.pdf",
          "media_type" -> "application/pdf",
          "label" -> "  Read the full English article  "
        ),
        "summary_slides_pdf" -> Map(
          "public_path" -> "/en/development-process/pdf/example-summary-slides.pdf",
          "media_type" -> "application/pdf"
        ),
        "video" -> Map(
          "presentation" -> "external-link",
          "status" -> "published",
          "provider" -> "youtube",
          "watch_url" -> "https://example.com/watch-en"
        )
      ))
      _media(japanesenotice) shouldBe Some(Map(
        "infographic" -> Map(
          "public_path" -> "/ja/development-process/images/example.png",
          "alt" -> "日本語インフォグラフィック"
        ),
        "article_pdf" -> Map(
          "public_path" -> "/ja/development-process/pdf/example-article.pdf",
          "media_type" -> "application/pdf",
          "label" -> "  記事 PDF  "
        ),
        "video" -> Map(
          "presentation" -> "external-link",
          "status" -> "published",
          "watch_url" -> "https://example.com/watch-ja"
        )
      ))
      _media(englishnotice) shouldBe _media(englishcategorynotice)
      _media(japanesenotice) shouldBe _media(japanesecategorynotice)
      _media(japanesenotice).flatMap(_.get("summary_slides_pdf")) shouldBe empty
    }

      "project the synthetic Phase 27 Part 5 article through localized article-top and Notice projections" in {
      Given("a neutral Part 5 article and exact English and Japanese media variants")
      val site = _site()

      When("the localized article and global/category Notices are generated")
      val englisharticle = _string(site, "doxsite.d/en/development-process/part-5.html")
      val japanesearticle = _string(site, "doxsite.d/ja/development-process/part-5.html")
      val englishglobal = _notice(site, "doxsite.d/WEB-INF/data/en", "development-process/part-5.html")
      val englishcategory = _notice(site, "doxsite.d/WEB-INF/data/en/development-process", "development-process/part-5.html")
      val japaneseglobal = _notice(site, "doxsite.d/WEB-INF/data/ja", "development-process/part-5.html")
      val japanesecategory = _notice(site, "doxsite.d/WEB-INF/data/ja/development-process", "development-process/part-5.html")

      Then("each article-top projection exposes only its exact locale projectable video and labels")
      englisharticle should include ("/en/development-process/part-5/_images/summary.png")
      englisharticle should include ("https://youtu.be/Part5MediaEn1")
      englisharticle should include ("View infographic")
      englisharticle should include ("Watch video")
      englisharticle should not include ("https://youtu.be/Part5MediaJa1")
      japanesearticle should include ("/ja/development-process/part-5/_images/summary.png")
      japanesearticle should include ("https://youtu.be/Part5MediaJa1")
      japanesearticle should include ("インフォグラフィックを見る")
      japanesearticle should include ("動画を見る")
      japanesearticle should not include ("https://youtu.be/Part5MediaEn1")

      And("global and category Notices expose matching normalized locale media maps")
      val expectedenglish = Some(Map(
        "infographic" -> Map(
          "public_path" -> "/en/development-process/part-5/_images/summary.png",
          "media_type" -> "image/png",
          "alt" -> "Part 5 infographic"
        ),
        "video" -> Map(
          "presentation" -> "external-link",
          "status" -> "published",
          "provider" -> "youtube",
          "watch_url" -> "https://youtu.be/Part5MediaEn1"
        )
      ))
      val expectedjapanese = Some(Map(
        "infographic" -> Map(
          "public_path" -> "/ja/development-process/part-5/_images/summary.png",
          "media_type" -> "image/png",
          "alt" -> "第5回インフォグラフィック"
        ),
        "video" -> Map(
          "presentation" -> "external-link",
          "status" -> "published",
          "provider" -> "youtube",
          "watch_url" -> "https://youtu.be/Part5MediaJa1"
        )
      ))
      _media(englishglobal) shouldBe expectedenglish
      _media(englishcategory) shouldBe expectedenglish
      _media(japaneseglobal) shouldBe expectedjapanese
      _media(japanesecategory) shouldBe expectedjapanese
      _media(englishglobal) shouldBe _media(englishcategory)
      _media(japaneseglobal) shouldBe _media(japanesecategory)
    }

      "keep draft and withdrawn video out of the article while retaining its infographic in Notice" in {
      Given("published articles whose registered videos are draft or withdrawn")
      val site = _site()

      When("the site projection is generated")
      val englishnotice = _notice(site, "doxsite.d/WEB-INF/data/en", "development-process/draft.html")
      val japanesenotice = _notice(site, "doxsite.d/WEB-INF/data/ja", "development-process/draft.html")

      Then("the unavailable videos have no header video action while the infographic remains available")
      _string(site, "doxsite.d/en/development-process/draft.html") should not include ("https://example.com/draft")
      _string(site, "doxsite.d/en/development-process/draft.html") should include ("smartdox-article-infographic")
      _string(site, "doxsite.d/ja/development-process/draft.html") should not include ("https://example.com/withdrawn")
      _string(site, "doxsite.d/ja/development-process/draft.html") should include ("smartdox-article-infographic")
      _media(englishnotice) shouldBe Some(Map(
        "infographic" -> Map("public_path" -> "/en/development-process/images/draft.png")
      ))
      _media(japanesenotice) shouldBe Some(Map(
        "infographic" -> Map("public_path" -> "/ja/development-process/images/draft.png")
      ))
    }

      "project a site-hosted player only from its content URL and preserve ordinary outputs without media" in {
      Given("a site-hosted published variant and an article with no registry entry")
      val site = _site()

      When("the site is generated")
      val hosted = _string(site, "doxsite.d/en/development-process/hosted.html")
      val plain = _string(site, "doxsite.d/en/development-process/plain.html")
      val hostednotice = _notice(site, "doxsite.d/WEB-INF/data/en", "development-process/hosted.html")
      val plainnotice = _notice(site, "doxsite.d/WEB-INF/data/en", "development-process/plain.html")

      Then("the hosted article has a native header action and the unregistered article remains unchanged")
      hosted should include ("smartdox-article-header-actions")
      hosted should include ("href=\"/en/development-process/videos/hosted.mp4\"")
      hosted should include ("Watch video")
      hosted should not include ("<video")
      _string(site, "doxsite.d/ja/development-process/hosted.html") should not include ("/en/development-process/videos/hosted.mp4")
      plain should not include ("smartdox-article-header-actions")
      plain should include ("Plain article lead.")
      _media(hostednotice) shouldBe Some(Map(
        "video" -> Map(
          "presentation" -> "site-hosted",
          "status" -> "published",
          "content_url" -> "/en/development-process/videos/hosted.mp4"
        )
      ))
      _media(plainnotice) shouldBe empty
      }

      "retain PDF controls when a legacy source page suppresses only the projected video duplicate" in {
      Given("a legacy video source page and an exact English resolved PDF role with a projectable video")
      val source = Document(Head.empty, Body(List(
        Paragraph(List(Text("Legacy article lead."))),
        Html5("div", VectorMap("class" -> "smartdox-video-publication"), Nil)
      )))
      val media = PublishMetadata.ArticleMediaVariant(
        locale = "en",
        video = Some(PublishMetadata.VideoReference(
          PublishMetadata.VideoPresentation.ExternalLink,
          PublishMetadata.VideoStatus.Published,
          watchUrl = Some(new URI("https://example.com/duplicate-video"))
        )),
        articlePdf = Some(PublishMetadata.PdfDocumentReference(
          new URI("/en/development-process/pdf/legacy-article.pdf"),
          "application/pdf"
        ))
      )

      When("the ordinary article media projection is applied")
      val projected = DoxSite.projectArticleMedia(source, Some(media), Locale.ENGLISH)
      val pdfcallout = projected.body.contents.collectFirst {
        case html: Html5 if html.attributes.get("class").contains("smartdox-article-media") => html
      }.getOrElse(fail("Missing PDF article-media callout"))

      Then("the established legacy player remains once while a PDF-only callout remains available")
      projected.body.contents.count {
        case html: Html5 if html.attributes.get("class").contains("smartdox-video-publication") => true
        case _ => false
      } shouldBe 1
      pdfcallout.attributes.get("class") shouldBe Some("smartdox-article-media")
      pdfcallout.elements.collectFirst {
        case html: Html5 if html.attributes.get("class").contains("smartdox-article-media-pdf") => html
      } should not be empty
      pdfcallout.toPlainText should include ("Article PDF")
      pdfcallout.toPlainText should not include ("Watch video")
      pdfcallout.elements.exists {
        case html: Html5 if html.attributes.get("class").contains("smartdox-article-media-video") => true
        case _ => false
      } shouldBe false
      }

      "preserve fixed site outputs when publication metadata is absent" in {
        Given("the committed article source and no publication metadata")
        val source = new File("src/test/resources/article-media-projection-site")

        When("the ordinary site is generated once")
        val site = new DoxSiteGenerator(_context, DoxSite.Config.default, None).
          generate(Realm.create(DoxSite.realmConfig, source))
        val article = _string(site, "doxsite.d/en/development-process/example.html")
        val globalnotice = _notice_string(site, "doxsite.d/WEB-INF/data/en", "development-process/example.html")
        val categorynotice = _notice_string(site, "doxsite.d/WEB-INF/data/en/development-process", "development-process/example.html")
        val globalnoticemap = _yaml_map(globalnotice)
        val categorynoticemap = _yaml_map(categorynotice)

        Then("the article and Notices retain their fixed source-derived content without projected media")
        article should include ("Article Media Example")
        article should include ("The effective lead paragraph.")
        article should include ("First body section")
        article should include ("The first section body.")
        article should not include ("smartdox-article-media-video")
        globalnoticemap.get("notice.uri") shouldBe Some("development-process/example.html")
        globalnoticemap.get("notice.title") shouldBe Some("Article Media Example")
        globalnoticemap.get("notice.status") shouldBe Some("published")
        globalnoticemap shouldBe categorynoticemap
        globalnotice should not include ("notice.media")
        categorynotice should not include ("notice.media")
        _media(globalnoticemap) shouldBe empty
        _media(categorynoticemap) shouldBe empty

        And("dashboard and machine metadata expose fixed schema and article identity evidence")
        val dashboard = _string(site, "doxsite.d/metadata/dashboard/site.json")
        dashboard should include ("\"article_count\"")
        dashboard should include ("\"category_count\"")
        dashboard should include ("development-process")
        Vector(
          "doxsite.d/site.jsonld",
          "doxsite.d/site.ttl",
          "doxsite.d/metadata/documents/fragments.json"
        ).foreach { path =>
          val metadata = _string(site, path)
          metadata should include ("Article Media Example")
          metadata should include ("development-process/example")
          metadata should not include ("smartdox-article-media-video")
        }
        _string(site, "doxsite.d/metadata/documents/fragments.json") should include ("development-process/example.dox")

        And("all generated feeds contain the fixed article entry and locale-specific path evidence")
        Vector(
          "doxsite.d/atom.xml" -> "/ja/development-process/example.html",
          "doxsite.d/en/atom.xml" -> "/en/development-process/example.html",
          "doxsite.d/ja/atom.xml" -> "/ja/development-process/example.html"
        ).foreach { case (path, articlepath) =>
          val feed = _string(site, path)
          feed should include ("<title>Article Media Example</title>")
          feed should include (articlepath)
          feed should not include ("smartdox-article-media-video")
        }
      }
    }

      "preserve SimpleModeling.org Notice slots and card identity when media is projected" in {
      Given("the same SimpleModeling.org publication source with and without its downstream media registry")
      val source = new File("src/test/resources/article-media-projection-site")
      val publication = new File("src/test/resources/article-media-projection-publication")

      When("the numbered Notice projections are generated for both locales")
      val baseline = new DoxSiteGenerator(_context, DoxSite.Config.default, None).
        generate(Realm.create(DoxSite.realmConfig, source))
      val projected = new DoxSiteGenerator(_context, DoxSite.Config.default, Some(publication)).
        generate(Realm.create(DoxSite.realmConfig, source))

      Then("media changes no Notice order or non-media card identity in global or category directories")
      Vector("en", "ja").foreach { locale =>
        val globaldirectory = s"doxsite.d/WEB-INF/data/$locale"
        val categorydirectory = s"$globaldirectory/development-process"
        val baselinenotices = Vector(
          _notice_entries(baseline, globaldirectory),
          _notice_entries(baseline, categorydirectory)
        )
        val projectednotices = Vector(
          _notice_entries(projected, globaldirectory),
          _notice_entries(projected, categorydirectory)
        )

        projectednotices.map(_.map { case (slot, notice) => slot -> _without_media(notice) }) shouldBe
          baselinenotices.map(_.map { case (slot, notice) => slot -> _without_media(notice) })

        val targeturi = "development-process/why-reconstruct-software-development-methodology.html"
        val targetindices = projectednotices.map(_notice_index(_, targeturi))
        targetindices.foreach { index => index should be >= 0 }
        targetindices shouldBe baselinenotices.map(_notice_index(_, targeturi))
        val targetslots = projectednotices.map(_notice_slot(_, targeturi))
        targetslots.foreach { slot => slot should be >= 0 }
        targetslots shouldBe baselinenotices.map(_notice_slot(_, targeturi))
        val targetnotices = projectednotices.zip(targetindices).map { case (notices, index) => notices(index)._2 }
        val baselinetargetnotices = baselinenotices.zip(targetindices).map { case (notices, index) => notices(index)._2 }
        baselinetargetnotices.foreach { notice => _media(notice) shouldBe empty }
        val expectedtitle = if (locale == "en")
          "Why Reconstruct Software Development Methodology?"
        else
          "なぜソフトウェア開発方法論を再構築するのか"
        val expectedsummary = if (locale == "en")
          "Why software development methodology must be reconstructed around modeling in the AI era."
        else
          "AI時代に、なぜソフトウェア開発方法論をモデリング中心に再構築する必要があるのかを考えます。"
        val expecteddescription = if (locale == "en")
          "Development process articles."
        else
          "開発プロセスの記事です。"
        targetnotices.foreach { notice =>
          notice.get("notice.uri") shouldBe Some(targeturi)
          notice.get("notice.title") shouldBe Some(expectedtitle)
          notice.get("notice.summary") shouldBe Some(expectedsummary)
          notice.get("notice.status") shouldBe Some("published")
          notice.get("notice.published") shouldBe Some("2026-07-20")
          notice.get("notice.title_image") shouldBe Some("https://plus.unsplash.com/premium_photo-1664297541167-9fd8e28c888d?q=80&w=2172&auto=format&fit=crop&ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D")
          val category = notice.get("notice.category") match {
            case Some(value: Map[_, _]) => value.asInstanceOf[Map[String, Any]]
            case other => fail(s"Missing Notice category: $other")
          }
          _map_value(category, "name") shouldBe Some("development-process")
          _map_value(category, "title") shouldBe Some("Development Process")
          _map_value(category, "uri") shouldBe Some("development-process/index.html")
          _map_value(category, "description") shouldBe Some(expecteddescription)
        }

        And("the target receives exact locale media in both global and category Notices")
        val expectedmedia = Map(
          "infographic" -> Map(
            "public_path" -> s"/$locale/development-process/why-reconstruct-software-development-methodology/_images/video-$locale.png",
            "media_type" -> "image/png",
            "alt" -> (if (locale == "en") "English detailed infographic" else "日本語詳細インフォグラフィック")
          ),
          "video" -> Map(
            "presentation" -> "external-link",
            "status" -> "published",
            "provider" -> "youtube",
            "watch_url" -> (if (locale == "en") "https://youtu.be/R8EhV6qLeUU" else "https://youtu.be/OSNCFSS-sh8")
          )
        )
        targetnotices.foreach { notice => _media(notice) shouldBe Some(expectedmedia) }
        _media(targetnotices.head) shouldBe _media(targetnotices.last)

        And("the existing media-free Plain article remains in its numbered slot and has no media")
        val plainuri = "development-process/plain.html"
        baselinenotices.zip(projectednotices).foreach { case (baselinenotice, projectednotice) =>
          val baselineplainindex = _notice_index(baselinenotice, plainuri)
          val projectedplainindex = _notice_index(projectednotice, plainuri)
          baselineplainindex should be >= 0
          projectedplainindex shouldBe baselineplainindex
          _notice_slot(projectednotice, plainuri) shouldBe _notice_slot(baselinenotice, plainuri)
          val baselineplain = baselinenotice(baselineplainindex)._2
          val projectedplain = projectednotice(projectedplainindex)._2
          _without_media(projectedplain) shouldBe _without_media(baselineplain)
          _media(baselineplain) shouldBe empty
          _media(projectedplain) shouldBe empty
        }
      }
      }

    "placement, identity, and compatibility" which {
      "place the header before the lead and the infographic after the lead, share Notice media with the category projection, and avoid a legacy duplicate" in {
      Given("the localized site, registry, and a legacy video-package source")
      val site = _site()
      val globalnotice = _notice(site, "doxsite.d/WEB-INF/data/en", "development-process/example.html")
      val categorynotice = _notice(site, "doxsite.d/WEB-INF/data/en/development-process", "development-process/example.html")
      val legacyinput = Realm.create(new File("src/test/resources/video-package-site"))
      val legacy = new AntoraGenerator(_context, DoxSite.Config.default, Some(new File("src/test/resources/video-publication-fixture"))).generate(legacyinput)

      When("article and Notice projections are rendered")
      val article = _string(site, "doxsite.d/en/development-process/example.html")
      val legacyarticle = _string(legacy, "antora.d/docs/concepts/modules/ROOT/pages/tutorial.adoc")

      Then("the title, header, lead, infographic, and section retain their required order")
      val titleindex = article.indexOf("<h1>Article Media Example</h1>")
      val headerindex = article.indexOf("smartdox-article-header")
      val leadindex = article.indexOf("The effective lead paragraph.")
      val mediaindex = article.indexOf("smartdox-article-header-actions")
      val figureindex = article.indexOf("smartdox-article-infographic")
      val bodysectionindex = article.lastIndexOf("First body section")
      titleindex should be >= 0
      headerindex should be >= 0
      leadindex should be >= 0
      mediaindex should be >= 0
      figureindex should be >= 0
      bodysectionindex should be >= 0
      titleindex should be < headerindex
      headerindex should be < mediaindex
      mediaindex should be < leadindex
      mediaindex should be < figureindex
      figureindex should be < bodysectionindex

      And("global and category Notices encode the same resolved media")
      _media(globalnotice) shouldBe _media(categorynotice)

      And("a rewritten legacy source page retains exactly its established player")
      _occurrences(legacyarticle, "class=\"smartdox-video-publication\"") shouldBe 1
      legacyarticle should not include ("smartdox-article-media-video")
    }

      "preserve ordinary api, dev, and web path segments while projecting their exact media through site, Notice, and Antora" in {
      Given("registered article identities whose first segment resembles a language code")
      val site = _site()
      val antora = new AntoraGenerator(
        _context,
        DoxSite.Config.default,
        Some(new File("src/test/resources/article-media-projection-publication"))
      ).generate(Realm.create(DoxSite.realmConfig, new File("src/test/resources/article-media-projection-site")))

      When("the source paths are resolved without generated locale reconstruction")
      val api = _notice(site, "doxsite.d/WEB-INF/data/en", "api/identity.html")
      val dev = _notice(site, "doxsite.d/WEB-INF/data/en", "dev/identity.html")
      val web = _notice(site, "doxsite.d/WEB-INF/data/en", "web/identity.html")

      Then("each first segment remains part of the canonical article identity")
      _string(site, "doxsite.d/en/api/identity.html") should include ("https://example.com/api-identity")
      _string(site, "doxsite.d/en/dev/identity.html") should include ("https://example.com/dev-identity")
      _string(site, "doxsite.d/en/web/identity.html") should include ("https://example.com/web-identity")
      _media(api).flatMap(_.get("video")).flatMap(value => _map_value(value, "watch_url")) shouldBe Some("https://example.com/api-identity")
      _media(dev).flatMap(_.get("video")).flatMap(value => _map_value(value, "watch_url")) shouldBe Some("https://example.com/dev-identity")
      _media(web).flatMap(_.get("video")).flatMap(value => _map_value(value, "watch_url")) shouldBe Some("https://example.com/web-identity")
      _string(antora, "antora.d/docs/api/modules/ROOT/pages/identity.adoc") should include ("https://example.com/api-identity")
      _string(antora, "antora.d/docs/dev/modules/ROOT/pages/identity.adoc") should include ("https://example.com/dev-identity")
      _string(antora, "antora.d/docs/web/modules/ROOT/pages/identity.adoc") should include ("https://example.com/web-identity")
    }

      "project a non-locale two-letter root identity through English site and Notice outputs and default-Japanese Antora output" in {
      Given("a go root article with exact English and Japanese external variants")
      val site = _site()
      val antora = new AntoraGenerator(
        _context,
        DoxSite.Config.default,
        Some(new File("src/test/resources/article-media-projection-publication"))
      ).generate(Realm.create(DoxSite.realmConfig, new File("src/test/resources/article-media-projection-site")))

      When("the source identity is projected without a locale fallback")
      val englishnotice = _notice(site, "doxsite.d/WEB-INF/data/en", "go/identity.html")

      Then("the English site and exact Notice use the English variant")
      _string(site, "doxsite.d/en/go/identity.html") should include ("https://example.com/go-identity-en")
      _media(englishnotice).flatMap(_.get("video")).flatMap(value => _map_value(value, "watch_url")) shouldBe Some("https://example.com/go-identity-en")

      And("the default-Japanese Antora output uses only the exact Japanese variant")
      _string(antora, "antora.d/docs/go/modules/ROOT/pages/identity.adoc") should include ("https://example.com/go-identity-ja")
      _string(antora, "antora.d/docs/go/modules/ROOT/pages/identity.adoc") should not include ("https://example.com/go-identity-en")
    }

      "insert media before the first body section when the article has no effective lead" in {
      Given("a registered article whose first substantive section has no lead paragraph")
      val site = _site()

      When("the localized article is rendered")
      val article = _string(site, "doxsite.d/en/development-process/no-lead.html")

      Then("the header action precedes that first section and no infographic is synthesized")
      article.indexOf("smartdox-article-header-actions") should be >= 0
      article.indexOf("smartdox-article-header-actions") should be < article.indexOf("First body section")
      article should not include ("smartdox-article-infographic")
      }
    }

    "direct header contract" should {
      "retain one legacy player while suppressing only its duplicate direct video action" in {
        Given("a legacy video source package and an explicit direct-media variant with video, PDF, and infographic roles")
        val input = Realm.create(new File("src/test/resources/video-package-site"))
        val videopublications = PublishMetadata.load(new File("src/test/resources/video-publication-fixture")).toVector.
          flatMap(_.videoPublications)
        val projection = _legacy_article_media_projection

        When("DoxSite creates the direct localized article and resolves its media projection")
        val site = DoxSite.create(
          _context,
          input,
          None,
          DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full),
          Nil,
          videopublications,
          Nil,
          Some(projection)
        ).toRealm(_context)
        val article = _string(site, "en/concepts/tutorial.html")

        Then("the established source-page player remains exactly once while the direct header keeps its independent PDF and infographic actions")
        _occurrences(article, "class=\"smartdox-video-publication\"") shouldBe 1
        _occurrences(article, "class=\"smartdox-video-player\"") shouldBe 1
        _header_actions(article) shouldBe Vector(
          "/en/concepts/tutorial.pdf" -> "Article PDF",
          "#smartdox-article-infographic" -> "View infographic"
        )
        article should not include ("https://example.com/direct-legacy-video")
        article should include ("id=\"smartdox-article-infographic\"")
        article should include ("class=\"smartdox-article-infographic\"")
      }

      "project all sixteen availability masks for both exact locales through physical and virtual roots" in {
        Given("a resolved article-media variant for every availability mask")
        val roots = Vector(false -> "physical source root", true -> "virtual Realm root")
        val locales = Vector(Locale.ENGLISH, Locale.JAPANESE)

        When("the ordinary DoxSite builder renders every root, locale, and mask")
        roots.foreach { case (virtualroot, rootdescription) =>
          locales.foreach { locale =>
            (0 to 15).foreach { maskbits =>
              val article = _direct_article(_direct_site(maskbits, virtualroot), locale)

              Then(s"the $rootdescription projection preserves mask $maskbits for ${locale.toLanguageTag}")
              _assert_header_mask(article, locale, maskbits)
            }
          }
        }
      }

      "insert an infographic after the title when no effective lead exists" in {
        Given("a direct virtual Realm article whose first substantive section has no effective lead")
        val source =
          """Direct No Lead Article
            |=======================
            |
            |# HEAD
            |
            |status=published
            |
            |# First body section
            |
            |Only the first body section is present.
            |""".stripMargin

        When("DoxSite renders the direct article with an active infographic projection")
        val article = _direct_article(_direct_virtual_site(source, 8, Some("Direct no-lead infographic")), Locale.ENGLISH)
        val titleindex = article.indexOf("<h1>Direct No Lead Article</h1>")
        val headerindex = article.indexOf("class=\"smartdox-article-header\"")
        val figureindex = article.indexOf("class=\"smartdox-article-infographic\"")
        val sectionindex = article.indexOf("First body section")

        Then("the title, header, infographic figure, and first body section retain their exact order without an effective lead")
        titleindex should be >= 0
        headerindex should be >= 0
        figureindex should be >= 0
        sectionindex should be >= 0
        titleindex should be < headerindex
        headerindex should be < figureindex
        figureindex should be < sectionindex
        article should not include ("The effective lead paragraph.")
      }

      "propagate an origin-backed physical Markdown parent through an active direct header projection" in {
        Given("an origin-backed physical Markdown page and its nested local image")
        val source =
          """Header Image Root Article
            |=========================
            |
            |# HEAD
            |
            |status=published
            |
            |# Body
            |
            |A lead with a nested local image: ![Diagram](images/../images/diagram.png)
            |""".stripMargin
        val physicalroot = Files.createTempDirectory("smartdox-direct-article-physical-image-root")
        try {
          Files.createDirectories(physicalroot.resolve("development-process/images"))
          Files.write(physicalroot.resolve("development-process/images/diagram.png"), "image".getBytes(StandardCharsets.UTF_8))
          Files.write(physicalroot.resolve("development-process/example.md"), source.getBytes(StandardCharsets.UTF_8))

          When("DoxSite creates the physical page with the direct header projection")
          val physicalinput = Realm.create(DoxSite.realmConfig, physicalroot.toFile)
          val physicalsite = DoxSite.create(
            _context,
            physicalinput,
            None,
            DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full),
            Nil,
            Nil,
            Nil,
            Some(_article_media_projection(8, Some("Physical image")))
          ).toRealm(_context)
          val physicalarticle = _string(physicalsite, "en/development-process/example.html")

          Then("the physical page uses its canonical document parent and normalizes the nested image URI")
          physicalarticle should include ("smartdox-article-header-actions")
          physicalarticle should include ("src=\"images/diagram.png\"")
          physicalarticle should not include ("images/../images/diagram.png")
        } finally {
          org.goldenport.io.IoUtils.removeDirectory(physicalroot.toFile)
        }
      }

      "propagate an origin-less virtual Realm parent through an active direct header projection" in {
        Given("an origin-less virtual Realm Markdown page with a nested local image")
        val source =
          """Header Image Root Article
            |=========================
            |
            |# HEAD
            |
            |status=published
            |
            |# Body
            |
            |A lead with a nested local image: ![Diagram](images/../images/diagram.png)
            |""".stripMargin
        val virtualinput = Realm.create()
        virtualinput.backend.setContent("development-process/example.md", StringData(source, 1L))

        When("DoxSite creates the virtual page with the direct header projection")
        val virtualsite = DoxSite.create(
          _context,
          virtualinput,
          None,
          DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full),
          Nil,
          Nil,
          Nil,
          Some(_article_media_projection(8, Some("Virtual image")))
        ).toRealm(_context)
        val virtualarticle = _string(virtualsite, "en/development-process/example.html")

        Then("the virtual page uses its normalized Realm parent and the same active header projection")
        virtualarticle should include ("smartdox-article-header-actions")
        virtualarticle should include ("src=\"images/diagram.png\"")
        virtualarticle should not include ("images/../images/diagram.png")
      }

      "reject an origin-backed physical Markdown traversal with the stable diagnostic" in {
        Given("an origin-backed physical Markdown page whose image path escapes its source root")
        val source = "![Escape](../../outside.png)\n"
        val projection = _article_media_projection(8, Some("Traversal image"))
        val physicalroot = Files.createTempDirectory("smartdox-direct-article-physical-image-escape")
        try {
          Files.createDirectories(physicalroot.resolve("development-process"))
          Files.write(physicalroot.resolve("development-process/example.md"), source.getBytes(StandardCharsets.UTF_8))

          When("DoxSite creates the physical page")
          val physicalfailure = intercept[IllegalArgumentException] {
            val physicalinput = Realm.create(DoxSite.realmConfig, physicalroot.toFile)
            DoxSite.create(
              _context,
              physicalinput,
              None,
              DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full),
              Nil,
              Nil,
              Nil,
              Some(projection)
            ).toRealm(_context)
          }

          Then("the physical traversal reports the stable unsupported-resource diagnostic and raw path")
          physicalfailure.getMessage should include ("image.markdown.unsupported-resource")
          physicalfailure.getMessage should include ("raw-path=../../outside.png")
        } finally {
          org.goldenport.io.IoUtils.removeDirectory(physicalroot.toFile)
        }
      }

      "reject an origin-less virtual Realm Markdown traversal with the stable diagnostic" in {
        Given("an origin-less virtual Realm page whose image path escapes its Realm parent")
        val source = "![Escape](../../outside.png)\n"
        val virtualinput = Realm.create()
        virtualinput.backend.setContent("development-process/example.md", StringData(source, 1L))

        When("DoxSite creates the virtual page")
        val virtualfailure = intercept[IllegalArgumentException] {
          DoxSite.create(
            _context,
            virtualinput,
            None,
            DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full),
            Nil,
            Nil,
            Nil,
            Some(_article_media_projection(8, Some("Traversal image")))
          ).toRealm(_context)
        }

        Then("the virtual traversal reports the stable unsupported-resource diagnostic and raw path")
        virtualfailure.getMessage should include ("image.markdown.unsupported-resource")
        virtualfailure.getMessage should include ("raw-path=../../outside.png")
      }

      "read tags and publication dates from the documented HOCON sources" in {
        Given("a virtual Realm article using tag and publishedAt fallbacks")
        val source =
          """Header Fallback Example
            |========================
            |
            |# HEAD
            |
            |status=published
            |tag="  first-tag, , second-tag  "
            |publishedAt=2026-09-09
            |modifiedAt=2026-09-08
            |
            |# Body
            |
            |Fallback lead.
            |
            |# Body section
            |
            |Fallback body.
            |""".stripMargin

        When("the virtual-root DoxSite page is generated for each exact locale")
        val englisharticle = _direct_article(_direct_virtual_site(source, 8, Some("Alt text")), Locale.ENGLISH)
        val japanesearticle = _direct_article(_direct_virtual_site(source, 8, Some("Alt text")), Locale.JAPANESE)

        Then("the fallback tags are trimmed, blank values are omitted, and supplied order is retained")
        englisharticle should include ("Tags")
        englisharticle should include ("first-tag")
        englisharticle should include ("second-tag")
        englisharticle.indexOf("first-tag") should be < englisharticle.indexOf("second-tag")
        englisharticle should not include ("modifiedAt")
        englisharticle should not include ("2026-09-08")

        And("the fallback publication date has semantic and localized markup")
        englisharticle should include ("Published")
        englisharticle should include ("datetime=\"2026-09-09\"")
        englisharticle should include ("2026-09-09")
        japanesearticle should include ("タグ")
        japanesearticle should include ("公開日")
        japanesearticle should include ("datetime=\"2026-09-09\"")
        japanesearticle should not include ("Tags")
        japanesearticle should not include (">Published<")
      }

      "prefer tags and publication dates over their legacy fallback keys" in {
        Given("a direct virtual article with preferred and fallback metadata keys")
        val preferredsource =
          """Preferred Metadata Example
            |===========================
            |
            |# HEAD
            |
            |status=published
            |tags=["  preferred-first  ", "", "  preferred-second  "]
            |tag="fallback-tag"
            |published_at=2026-09-10
            |publishedAt=2026-09-09
            |modifiedAt=2026-09-08
            |
            |# Body
            |
            |Preferred metadata lead.
            |""".stripMargin

        When("DoxSite renders the preferred metadata article for both exact locales")
        val englishpreferred = _direct_article(_direct_virtual_site(preferredsource, 8, Some("Preferred infographic")), Locale.ENGLISH)
        val japanesepreferred = _direct_article(_direct_virtual_site(preferredsource, 8, Some("Preferred infographic")), Locale.JAPANESE)

        Then("preferred tags win, preserve input order, trim values, and omit blanks")
        englishpreferred should include ("Tags")
        englishpreferred should include ("preferred-first")
        englishpreferred should include ("preferred-second")
        englishpreferred should not include ("fallback-tag")
        englishpreferred.indexOf("preferred-first") should be < englishpreferred.indexOf("preferred-second")
        englishpreferred should not include ("  preferred-first  ")
        englishpreferred should not include ("2026-09-08")
        englishpreferred should include ("<span class=\"smartdox-article-header-label\">Tags</span>")
        englishpreferred should include ("<span class=\"smartdox-article-header-label\">Published</span>")
        englishpreferred should include ("datetime=\"2026-09-10\"")
        englishpreferred should include ("2026-09-10")
        englishpreferred should not include ("2026-09-09")

        And("preferred metadata uses exact localized labels and semantic time markup")
        japanesepreferred should include ("<span class=\"smartdox-article-header-label\">タグ</span>")
        japanesepreferred should include ("<span class=\"smartdox-article-header-label\">公開日</span>")
        japanesepreferred should not include ("Tags")
        japanesepreferred should not include (">Published<")
        japanesepreferred should include ("datetime=\"2026-09-10\"")
      }

      "use fallback metadata when the preferred values are empty" in {
        Given("a direct virtual article whose empty preferred metadata permits fallback values")
        val fallbacksource =
          """Empty Preferred Metadata Example
            |=================================
            |
            |# HEAD
            |
            |status=published
            |tags=[]
            |tag=["  fallback-first  ", "", "  fallback-second  "]
            |published_at=""
            |publishedAt=2026-09-11
            |
            |# Body
            |
            |Fallback metadata lead.
            |""".stripMargin

        When("DoxSite renders the empty-preferred article")
        val emptypreferred = _direct_article(_direct_virtual_site(fallbacksource, 8, Some("Fallback infographic")), Locale.ENGLISH)

        Then("the documented fallback values remain trimmed, ordered, and semantically rendered")
        emptypreferred should include ("Tags")
        emptypreferred should include ("fallback-first")
        emptypreferred should include ("fallback-second")
        emptypreferred.indexOf("fallback-first") should be < emptypreferred.indexOf("fallback-second")
        emptypreferred should include ("<span class=\"smartdox-article-header-label\">Tags</span>")
        emptypreferred should include ("<span class=\"smartdox-article-header-label\">Published</span>")
        emptypreferred should include ("datetime=\"2026-09-11\"")
        emptypreferred should not include ("2026-09-10")
      }

      "retain an action wrapper while omitting metadata with no metadata inputs" in {
        Given("a direct virtual article with no metadata and an available infographic action")
        val actiononlysource =
          """Action Only Metadata Example
            |=============================
            |
            |Action-only body.
            |""".stripMargin

        When("DoxSite renders the action-only article")
        val actiononly = _direct_article(_direct_virtual_site(actiononlysource, 8, Some("Action-only infographic")), Locale.ENGLISH)

        Then("the header wrapper and action remain while metadata is omitted")
        actiononly should include ("smartdox-article-header")
        actiononly should include ("smartdox-article-header-actions")
        actiononly should include ("View infographic")
        actiononly should not include ("smartdox-article-header-metadata")
      }

      "preserve all infographic alt states and the full-size accessible link" in {
        Given("a direct virtual-root article and an infographic with each registered alt state")
        val altstates = Vector(Some("Meaningful alt"), Some(""), None)

        When("the direct DoxSite projection is rendered for English and Japanese")
        Vector(Locale.ENGLISH, Locale.JAPANESE).foreach { locale =>
          altstates.foreach { altvalue =>
            val article = _direct_article(_direct_virtual_site(_virtual_example_source, 8, altvalue), locale)
            val expectedalt = altvalue.getOrElse("")
            val expectedlabel = if (locale == Locale.JAPANESE) "インフォグラフィックを見る" else "View infographic"

            Then(s"the ${locale.toLanguageTag} image keeps alt state $altvalue")
            _image_alt(article) shouldBe expectedalt
            article should include ("id=\"smartdox-article-infographic\"")
            article should include ("class=\"smartdox-article-infographic\"")
            article should include ("href=\"#smartdox-article-infographic\"")
            article should include ("aria-label=\"" + expectedlabel + "\"")
            article should include ("href=\"/" + locale.toLanguageTag + "/development-process/matrix-infographic.png\"")
            article should not include ("disabled")
          }
        }
      }

      "omit an empty header and keep the direct page free of the retired media callout" in {
        Given("a virtual Realm article with no metadata and no resolved media")
        val source = """No Header Example
          |==================
          |
          |No metadata body.
          |""".stripMargin

        When("the direct DoxSite page is rendered")
        val article = _direct_article(_direct_virtual_site(source, 0, None), Locale.ENGLISH)

        Then("no article header, action, figure, or old article-media callout is emitted")
        article should not include ("smartdox-article-header")
        article should not include ("smartdox-article-infographic")
        article should not include ("smartdox-article-media")
      }
    }
  }

  private lazy val _context: Context = {
    val environment = Environment.createJaJp()
    val cliconfig = CliConfig.buildJaJp()
    new Context(environment, Config(cliconfig), environment.contextFoundation)
  }

  private def _site(): Realm =
    new DoxSiteGenerator(
      _context,
      DoxSite.Config.default,
      Some(new File("src/test/resources/article-media-projection-publication"))
    ).generate(Realm.create(DoxSite.realmConfig, new File("src/test/resources/article-media-projection-site")))

  private val _virtual_example_source =
    """Article Media Example
      |=====================
      |
      |# HEAD
      |
      |status=published
      |published_at=2026-08-04
      |tags=["  modeling  ", "", "  ai-collaboration  "]
      |
      |# Body
      |
      |The effective lead paragraph.
      |
      |# First body section
      |
      |The first section body.
      |""".stripMargin

  private def _direct_site(maskbits: Int, virtualroot: Boolean): Realm =
    _direct_site(_virtual_example_source, maskbits, Some("Matrix infographic"), virtualroot)

  private def _direct_virtual_site(
    source: String,
    maskbits: Int,
    altvalue: Option[String]
  ): Realm =
    _direct_site(source, maskbits, altvalue, true)

  private def _direct_site(
    source: String,
    maskbits: Int,
    altvalue: Option[String],
    virtualroot: Boolean
  ): Realm = {
    val input = if (virtualroot) {
      val realm = Realm.create()
      realm.backend.setContent("development-process/example.dox", StringData(source, 1L))
      realm
    } else {
      Realm.create(DoxSite.realmConfig, new File("src/test/resources/article-media-projection-site"))
    }
    val projection = _article_media_projection(maskbits, altvalue)
    DoxSite.create(
      _context,
      input,
      None,
      DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full),
      Nil,
      Nil,
      Nil,
      Some(projection)
    ).toRealm(_context)
  }

  private def _article_media_projection(
    maskbits: Int,
    altvalue: Option[String]
  ): PublishMetadata.ArticleMediaProjection = {
    val variants = Vector(Locale.ENGLISH, Locale.JAPANESE).map { locale =>
      val localetag = locale.toLanguageTag
      val video = if ((maskbits & 1) != 0)
        Some(PublishMetadata.VideoReference(
          presentation = PublishMetadata.VideoPresentation.ExternalLink,
          status = PublishMetadata.VideoStatus.Published,
          provider = Some("youtube"),
          watchUrl = Some(new URI(s"https://example.com/$localetag/matrix-video"))
        ))
      else
        None
      val summaryslidespdf = if ((maskbits & 2) != 0)
        Some(PublishMetadata.PdfDocumentReference(
          publicPath = new URI(s"/$localetag/development-process/matrix-summary.pdf"),
          mediaType = "application/pdf"
        ))
      else
        None
      val articlepdf = if ((maskbits & 4) != 0)
        Some(PublishMetadata.PdfDocumentReference(
          publicPath = new URI(s"/$localetag/development-process/matrix-article.pdf"),
          mediaType = "application/pdf"
        ))
      else
        None
      val infographic = if ((maskbits & 8) != 0)
        Some(PublishMetadata.ImageReference(
          publicPath = new URI(s"/$localetag/development-process/matrix-infographic.png"),
          mediaType = Some("image/png"),
          alt = altvalue
        ))
      else
        None
      localetag -> PublishMetadata.ArticleMediaVariant(
        locale = localetag,
        infographic = infographic,
        video = video,
        articlePdf = articlepdf,
        summarySlidesPdf = summaryslidespdf
      )
    }
    val publication = PublishMetadata.ArticleMediaPublication(
      articleIdentity = "development-process/example",
      variants = variants.map(_._2)
    )
    PublishMetadata.ArticleMediaProjection(PublishMetadata.ArticleMediaRegistry(
      publications = Vector(publication),
      compatibilityVariants = Map.empty,
      diagnostics = Vector.empty
    ))
  }

  private def _legacy_article_media_projection: PublishMetadata.ArticleMediaProjection = {
    val variant = PublishMetadata.ArticleMediaVariant(
      locale = "en",
      infographic = Some(PublishMetadata.ImageReference(
        publicPath = new URI("/en/concepts/tutorial.png"),
        mediaType = Some("image/png"),
        alt = Some("Legacy direct infographic")
      )),
      video = Some(PublishMetadata.VideoReference(
        presentation = PublishMetadata.VideoPresentation.ExternalLink,
        status = PublishMetadata.VideoStatus.Published,
        provider = Some("youtube"),
        watchUrl = Some(new URI("https://example.com/direct-legacy-video"))
      )),
      articlePdf = Some(PublishMetadata.PdfDocumentReference(
        publicPath = new URI("/en/concepts/tutorial.pdf"),
        mediaType = "application/pdf"
      ))
    )
    PublishMetadata.ArticleMediaProjection(PublishMetadata.ArticleMediaRegistry(
      publications = Vector(PublishMetadata.ArticleMediaPublication(
        articleIdentity = "concepts/tutorial",
        variants = Vector(variant)
      )),
      compatibilityVariants = Map.empty,
      diagnostics = Vector.empty
    ))
  }

  private def _direct_article(realm: Realm, locale: Locale): String =
    realm.get(s"$locale/development-process/example.html").orElse(
      realm.get(s"${locale.toLanguageTag}/development-process/example.html")
    ).collect {
      case data: StringData => data.string
    }.getOrElse(fail(s"Missing direct generated content for ${locale.toLanguageTag}"))

  private def _assert_header_mask(article: String, locale: Locale, maskbits: Int): Unit = {
    val localetag = locale.toLanguageTag
    val expectedvideo = s"https://example.com/$localetag/matrix-video"
    val expectedsummary = s"/$localetag/development-process/matrix-summary.pdf"
    val expectedarticle = s"/$localetag/development-process/matrix-article.pdf"
    val videolabel = if (locale == Locale.JAPANESE) "動画を見る" else "Watch video"
    val summarylabel = if (locale == Locale.JAPANESE) "要約スライド PDF" else "Summary slides PDF"
    val articlelabel = if (locale == Locale.JAPANESE) "記事 PDF" else "Article PDF"
    val infographiclabel = if (locale == Locale.JAPANESE) "インフォグラフィックを見る" else "View infographic"
    val expectedactions = Vector(
      1 -> (expectedvideo, videolabel),
      2 -> (expectedsummary, summarylabel),
      4 -> (expectedarticle, articlelabel),
      8 -> ("#smartdox-article-infographic", infographiclabel)
    ).filter { case (bit, _) => (maskbits & bit) != 0 }
    val actions = _header_actions(article)
    actions shouldBe expectedactions.map { case (_, action) => action }
    if (expectedactions.isEmpty)
      article should not include ("smartdox-article-header-actions")
    else
      article should include ("smartdox-article-header-actions")
    if ((maskbits & 8) != 0) {
      article should include ("id=\"smartdox-article-infographic\"")
      article should include ("class=\"smartdox-article-infographic\"")
    } else {
      article should not include ("smartdox-article-infographic")
    }
    article should not include ("smartdox-article-media")
    article should not include ("disabled")
    article should not include ("href=\"\"")
    article should not include ("<style")
    article should not include ("<link")
    article should not include ("<script src=")
    article should not include ("type=\"text/javascript\"")
    article should not include ("javascript:")
    article should not include ("onclick=")
    "(?i)<h1(?:\\s|>)".r.findAllMatchIn(article).size shouldBe 1
    article should not include ("role=\"heading\"")
  }

  private def _header_actions(article: String): Vector[(String, String)] =
    """(?s)<a\b(?=[^>]*\bclass="smartdox-article-header-action")(?=[^>]*\bhref="([^"]*)")[^>]*>([^<]*)</a>""".r.
      findAllMatchIn(article).
      map(m => m.group(1) -> m.group(2)).
      toVector

  private def _image_alt(article: String): String =
    "(?s)<img[^>]*\\balt=\"([^\"]*)\"".r.findFirstMatchIn(article).
      map(_.group(1)).getOrElse(fail("Missing infographic alt attribute"))

  private def _string(realm: Realm, path: String): String =
    realm.get(path).collect {
      case data: StringData => data.string
    }.getOrElse(fail(s"Missing generated content: $path"))

  private def _notice(realm: Realm, directory: String, uri: String): Map[String, Any] =
    _yaml_map(_notice_string(realm, directory, uri))

  private def _notice_string(realm: Realm, directory: String, uri: String): String =
    (1 to 20).flatMap { index =>
      realm.get(f"$directory/notice$index%02d.yaml").collect {
        case data: StringData => data.string
      }
    }.find(value => _yaml_map(value).get("notice.uri").contains(uri)).getOrElse(fail(s"Missing Notice for URI: $uri"))

  private def _notice_entries(realm: Realm, directory: String): Vector[(Int, Map[String, Any])] =
    (1 to 99).flatMap { index =>
      realm.get(f"$directory/notice$index%02d.yaml").collect {
        case data: StringData => index -> _yaml_map(data.string)
      }
    }.toVector

  private def _notice_index(notices: Vector[(Int, Map[String, Any])], uri: String): Int =
    notices.indexWhere(_._2.get("notice.uri").contains(uri))

  private def _notice_slot(notices: Vector[(Int, Map[String, Any])], uri: String): Int =
    notices.find(_._2.get("notice.uri").contains(uri)).map(_._1).getOrElse(-1)

  private def _without_media(notice: Map[String, Any]): Map[String, Any] =
    notice - "notice.media"

  private def _media(notice: Map[String, Any]): Option[Map[String, Any]] =
    notice.get("notice.media").collect { case map: Map[_, _] => map.asInstanceOf[Map[String, Any]] }

  private def _map_value(value: Any, key: String): Option[String] =
    value match {
      case map: Map[_, _] => map.asInstanceOf[Map[String, Any]].get(key).map(_.toString)
      case _ => None
    }

  private def _yaml_map(yaml: String): Map[String, Any] =
    _yaml_value(new Yaml().load[Any](yaml)).asInstanceOf[Map[String, Any]]

  private def _yaml_value(value: Any): Any =
    value match {
      case map: java.util.Map[_, _] => map.asScala.map { case (key, item) => key.toString -> _yaml_value(item) }.toMap
      case list: java.util.List[_] => list.asScala.map(_yaml_value).toVector
      case other => other
    }

  private def _occurrences(text: String, token: String): Int =
    token.r.findAllMatchIn(text).length
}
