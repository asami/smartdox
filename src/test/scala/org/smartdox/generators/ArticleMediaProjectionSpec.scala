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
