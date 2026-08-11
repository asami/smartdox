package org.smartdox.generators

import java.io.File
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
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator.{Config, Context}

/*
 * @since   Aug.  4, 2026
 * @version Aug.  4, 2026
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
      val japanesenotice = _notice(site, "doxsite.d/WEB-INF/data/ja", "development-process/example.html")

      Then("each exact locale receives only its own article URL and localized label")
      _string(site, "doxsite.d/en/development-process/example.html") should include ("https://example.com/watch-en")
      _string(site, "doxsite.d/en/development-process/example.html") should include ("Watch video")
      _string(site, "doxsite.d/ja/development-process/example.html") should include ("https://example.com/watch-ja")
      _string(site, "doxsite.d/ja/development-process/example.html") should include ("動画を見る")
      _string(site, "doxsite.d/ja/development-process/example.html") should not include ("watch-en")
      _string(antora, "antora.d/docs/development-process/modules/ROOT/pages/example.adoc") should include ("https://example.com/watch-ja")
      _string(antora, "antora.d/docs/development-process/modules/ROOT/pages/example.adoc") should not include ("https://example.com/watch-en")

      And("Notice media uses the exact normalized schema and locale-resolved URL")
      _media(englishnotice) shouldBe Some(Map(
        "infographic" -> Map(
          "public_path" -> "/en/development-process/images/example.png",
          "media_type" -> "image/png",
          "alt" -> "English infographic"
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
        "video" -> Map(
          "presentation" -> "external-link",
          "status" -> "published",
          "watch_url" -> "https://example.com/watch-ja"
        )
      ))
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
      englisharticle should not include ("/en/development-process/part-5/_images/summary.png")
      englisharticle should include ("https://youtu.be/Part5MediaEn1")
      englisharticle should not include ("View infographic")
      englisharticle should include ("Watch video")
      englisharticle should not include ("https://youtu.be/Part5MediaJa1")
      japanesearticle should not include ("/ja/development-process/part-5/_images/summary.png")
      japanesearticle should include ("https://youtu.be/Part5MediaJa1")
      japanesearticle should not include ("インフォグラフィックを見る")
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

      Then("the unavailable videos have no article-top block or Notice video")
      _string(site, "doxsite.d/en/development-process/draft.html") should not include ("smartdox-article-media-video")
      _string(site, "doxsite.d/ja/development-process/draft.html") should not include ("smartdox-article-media-video")
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

      Then("the hosted article has a player and the unregistered article remains unchanged")
      hosted should include ("smartdox-article-media-video")
      hosted should include ("<video")
      hosted should include ("src=\"/en/development-process/videos/hosted.mp4\"")
      _string(site, "doxsite.d/ja/development-process/hosted.html") should not include ("smartdox-article-media-video")
      plain should not include ("smartdox-article-media-video")
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
      "place the callout after the lead and before the first section, share Notice media with the category projection, and avoid a legacy duplicate" in {
      Given("the localized site, registry, and a legacy video-package source")
      val site = _site()
      val globalnotice = _notice(site, "doxsite.d/WEB-INF/data/en", "development-process/example.html")
      val categorynotice = _notice(site, "doxsite.d/WEB-INF/data/en/development-process", "development-process/example.html")
      val legacyinput = Realm.create(new File("src/test/resources/video-package-site"))
      val legacy = new AntoraGenerator(_context, DoxSite.Config.default, Some(new File("src/test/resources/video-publication-fixture"))).generate(legacyinput)

      When("article and Notice projections are rendered")
      val article = _string(site, "doxsite.d/en/development-process/example.html")
      val legacyarticle = _string(legacy, "antora.d/docs/concepts/modules/ROOT/pages/tutorial.adoc")

      Then("the lead, callout, and section retain their required order")
      val leadindex = article.indexOf("The effective lead paragraph.")
      val mediaindex = article.indexOf("smartdox-article-media-video")
      val bodysectionindex = article.lastIndexOf("First body section")
      leadindex should be >= 0
      mediaindex should be >= 0
      bodysectionindex should be >= 0
      leadindex should be < mediaindex
      mediaindex should be < bodysectionindex

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

      Then("the media callout precedes that first section instead of treating it as a lead")
      article.indexOf("smartdox-article-media-video") should be >= 0
      article.indexOf("smartdox-article-media-video") should be < article.indexOf("First body section")
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
