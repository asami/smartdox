package org.smartdox.doxsite

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Locale
import io.circe.parser.parse
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.generator.Context

/*
 * @since   Oct.  1, 2026
 * @version Oct. 13, 2026
 */
class DoxSiteEffectiveContentSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Effective site content" should {
    "publication link identity" which {
      "resolve every admitted article alias to its localized HTTPS route and title" in {
        Given("a selected HTTPS publication configuration with bilingual ordinary and package articles")
        val dir = _fixture()
        try {
          val context = Context.create()

          When("publication contexts resolve logical, physical, legacy and canonical incoming article links")
          val publication = _publication(dir, "guide/ordinary.dox", context)
          val nested = _publication(dir, "guide/nested/source.dox", context)
          val aliases = Vector("article.dox", "article.dox/index.dox", "article.dox/index.html", "article.html")
          val resolved = for {
            (locale, title) <- Vector(Locale.JAPANESE -> "記事", Locale.ENGLISH -> "Article")
            alias <- aliases
          } yield (locale, title, publication.resolve(new URI(alias), locale).take)
          val nestedlinks = Vector(Locale.JAPANESE, Locale.ENGLISH).map { locale =>
            locale -> nested.resolve(new URI("../article.dox"), locale).take
          }
          val fragment = publication.resolve(new URI("article.dox/index.html#section%20one"), Locale.ENGLISH).take

          Then("every alias uses the selected configuration, exact locale title and canonical public path")
          resolved.foreach { case (locale, title, link) =>
            link.uri.toString shouldBe s"https://publication.example.test/docs/${locale.getLanguage}/guide/article.html"
            link.title shouldBe title
            link.document.targetPath shouldBe "guide/article.dox"
            link.document.publicPath shouldBe "guide/article.html"
            link.document.relativeUri.toString shouldBe "article.html"
            link.document.tooltip shouldBe publication.site.space.getNode("guide/article.dox").
              flatMap(DoxSiteEffectiveContent.effectiveContent).get.page.dox.head.metadata.getEffectiveTooltip
            link.document.tooltip should not be empty
          }
          And("a nested ordinary source resolves against its own parent and retains existing fragment handling")
          nestedlinks.foreach { case (locale, link) =>
            link.uri.toString shouldBe s"https://publication.example.test/docs/${locale.getLanguage}/guide/article.html"
            link.title shouldBe (if (locale == Locale.JAPANESE) "記事" else "Article")
            link.document.relativeUri.toString shouldBe "../article.html"
          }
          fragment.document.relativeUri.getFragment shouldBe "section%20one"
          fragment.uri.toString shouldBe "https://publication.example.test/docs/en/guide/article.html"
        } finally {
          _delete(dir)
        }
      }

      "resolve a package source's outgoing link beside its logical document" in {
        Given("a package index and a bilingual ordinary sibling")
        val dir = _fixture()
        try {
          val context = Context.create()

          When("the physical package source resolves an ordinary sibling in both locales")
          val publication = _publication(dir, "guide/article.dox/index.dox", context)
          val japanese = publication.resolve(new URI("ordinary.dox"), Locale.JAPANESE).take
          val english = publication.resolve(new URI("ordinary.html"), Locale.ENGLISH).take

          Then("the physical index's logical parent supplies the ordinary sibling route")
          japanese.uri.toString shouldBe "https://publication.example.test/docs/ja/guide/ordinary.html"
          japanese.title shouldBe "通常記事"
          english.uri.toString shouldBe "https://publication.example.test/docs/en/guide/ordinary.html"
          english.title shouldBe "Ordinary"
          japanese.document.relativeUri.toString shouldBe "ordinary.html"
          english.document.relativeUri.toString shouldBe "ordinary.html"
        } finally {
          _delete(dir)
        }
      }
    }

    "publication target admission" which {
      "keep draft, absent and unselected package targets unresolved and reject root escapes" in {
        Given("draft and missing packages plus private Dox and Markdown descendants")
        val dir = _fixture()
        try {
          val context = Context.create()

          When("a selected publication context receives targets outside the effective published content")
          val publication = _publication(dir, "guide/ordinary.dox", context)
          val targets = Vector("draft.dox", "draft.dox/index.dox", "missing.dox", "missing.dox/index.dox",
            "absent.dox", "article.dox/parts/private.dox", "article.dox/review/private.md")
          val unresolved = targets.map(target => publication.resolve(new URI(target), Locale.ENGLISH))
          val escape = publication.resolve(new URI("../../outside.dox"), Locale.ENGLISH)

          Then("unselected or unpublished sources cannot supply a publication route")
          unresolved.foreach { result =>
            result.isError shouldBe true
            result.message should include ("pdf.site-link.target.unresolved")
          }
          And("relative root escape retains the existing rejection classification")
          escape.isError shouldBe true
          escape.message should include ("pdf.site-link.target.outside-root")
        } finally {
          _delete(dir)
        }
      }

      "reject a symlink at the selected package index leaf" in {
        Given("an admitted package whose physical index becomes a symlink before link resolution")
        val dir = _fixture()
        try {
          val context = Context.create()
          val publication = _publication(dir, "guide/ordinary.dox", context)
          val index = dir.resolve("guide/article.dox/index.dox")
          Files.delete(index)
          Files.createSymbolicLink(index, dir.resolve("guide/ordinary.dox"))

          When("publication resolves the package using its selected physical source")
          val result = publication.resolve(new URI("article.dox"), Locale.ENGLISH)

          Then("the existing leaf-symlink protection rejects the publication target")
          result.isError shouldBe true
          result.message should include ("pdf.site-link.target.outside-root")
        } finally {
          _delete(dir)
        }
      }
    }

    "public route compatibility" which {
      "preserve the public facade's ordinary, package, separator and asset mapping" in {
        Given("existing public path forms and a site with ordinary, root-index and package pages")
        val dir = _fixture()
        try {
          val context = Context.create()
          val paths = Vector(
            "guide/ordinary.dox" -> "guide/ordinary.html",
            "guide/article.dox" -> "guide/article.html",
            "guide/article.dox/index.dox" -> "guide/article.html",
            "guide/article.dox/index.html" -> "guide/article.html",
            "guide/article.html" -> "guide/article.html",
            "/guide/article.dox/index.dox" -> "/guide/article.html",
            "guide\\article.dox\\index.html" -> "guide/article.html",
            "/guide/ordinary.dox" -> "/guide/ordinary.html",
            "assets/diagram.svg" -> "assets/diagram.svg",
            "index.dox" -> "index.html")

          When("the producer and compatibility facade map the paths and the site produces localized public pages")
          val mapped = paths.map { case (source, expected) =>
            (expected, DoxSiteEffectiveContent.publicPath(source), DoxSite.publicPath(source))
          }
          val site = DoxSite.create(context, dir.toFile, Some("site"),
            DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
          val realm = site.toRealm(context)
          implicit val i18ncontext = context.i18NContext

          Then("the producer and facade preserve the complete existing mapping")
          mapped.foreach { case (expected, producer, facade) =>
            producer shouldBe expected
            facade shouldBe expected
          }
          And("actual public outputs retain ordinary and root-index pages and one canonical package route per locale")
          for (locale <- Vector("ja", "en")) {
            realm.getString(s"$locale/index.html") should not be empty
            realm.getString(s"$locale/guide/ordinary.html").get should include ("article.html")
            realm.getString(s"$locale/guide/article.html").get should include ("ordinary.html")
            realm.getString(s"$locale/guide/article.dox/index.html") shouldBe None
          }
        } finally {
          _delete(dir)
        }
      }
    }

    "public metadata admission" which {
      "emit one physical-evidence package fragment per locale and one logical LinkCollection identity" in {
        Given("an admitted article package with JA/EN bilingual metadata output alongside unselected Dox and bare-_source Markdown")
        val dir = _fixture()
        try {
          val context = Context.create()
          val privatepaths = Vector("guide/article.dox/parts/private.dox", "guide/article.dox/review/private.md")
          val before = privatepaths.map(path => Files.readAllBytes(dir.resolve(path)).toVector)

          When("the site generates document fragments, links and RDF metadata")
          val site = DoxSite.create(context, dir.toFile, Some("site"),
            DoxSite.Config.default.copy(
              strategy = DoxSite.Strategy.Full,
              siteOutput = DoxSite.Config.SiteOutput.simplemodelingOrgCompatibility))
          val realm = site.toRealm(context)
          implicit val i18ncontext = context.i18NContext
          val fragmentjson = realm.getString("metadata/documents/fragments.json").get
          val fragments = parse(fragmentjson).toOption.get.hcursor.downField("fragments").as[Vector[io.circe.Json]].toOption.get
          val article = fragments.filter(_.hcursor.get[String]("source_path").toOption.contains("guide/article.dox/index.dox"))
          val links = site.metadata.linkCollection.get
          val rdf = Vector("site.ttl", "site.jsonld", "metadata/rdf/graph.json").map(path => realm.getString(path).get)
          val after = privatepaths.map(path => Files.readAllBytes(dir.resolve(path)).toVector)

          Then("each locale records the package once with physical source evidence and canonical public identity")
          article.size shouldBe 2
          article.map(_.hcursor.get[String]("locale").toOption.get).sorted shouldBe Vector("en", "ja")
          article.foreach { fragment =>
            fragment.hcursor.get[String]("public_path").toOption.get shouldBe "guide/article.html"
          }
          links.get("guide/article.dox") should not be empty
          links.get("guide/article.dox/index.dox") shouldBe None
          And("private descendants receive no fragments, links, pages or RDF routes and their bytes remain unchanged")
          privatepaths.foreach { path =>
            fragmentjson should not include (path)
            links.get(path) shouldBe None
            rdf.foreach(_ should not include (path.stripSuffix(".dox").stripSuffix(".md")))
          }
          for {
            locale <- Vector("ja", "en")
            path <- Vector("parts/private", "review/private")
          } realm.getString(s"$locale/guide/article.dox/$path.html") shouldBe None
          after shouldBe before
        } finally {
          _delete(dir)
        }
      }
    }

    "bibliography source admission" which {
      "retain ordinary, package and ordinary-draft references from the admitted tree without private DOI discovery" in {
        Given("published ordinary and package references, an admitted ordinary draft reference and a private-only DOI")
        val dir = _fixture()
        try {
          val context = Context.create()

          When("production strategy generates bibliography data after excluding ordinary draft pages")
          val site = DoxSite.create(context, dir.toFile, Some("site"),
            DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
          val realm = site.toRealm(context)
          implicit val i18ncontext = context.i18NContext
          val bibliography = realm.getString("metadata/bibliography/bibliography.json").get
          val entries = site.metadata.bibliography.entries
          val ordinary = entries.find(_.id == "doi:10.5555/ordinary-admitted").get
          val article = entries.find(_.id == "doi:10.5555/package-admitted").get
          val draft = entries.find(_.id == "doi:10.5555/draft-admitted").get
          val rdf = realm.getString("site.ttl").get

          Then("published references remain present with canonical logical source identity and public paths")
          ordinary.sourceRefs.map(_.sourcePath) shouldBe Vector("guide/ordinary.dox")
          ordinary.sourceRefs.map(_.publicPath) shouldBe Vector("guide/ordinary.html")
          article.sourceRefs.map(_.sourcePath) shouldBe Vector("guide/article.dox")
          article.sourceRefs.map(_.publicPath) shouldBe Vector("guide/article.html")
          And("the admitted ordinary draft's reference survives although its public page is absent")
          draft.sourceRefs.map(_.sourcePath) shouldBe Vector("guide/ordinary-draft.dox")
          for (locale <- Vector("ja", "en")) {
            realm.getString(s"$locale/guide/ordinary-draft.html") shouldBe None
          }
          And("unselected package sources contribute neither bibliography entries nor source references or RDF routes")
          entries.map(_.id) should not contain "doi:10.5555/private-only"
          bibliography should not include ("private-only")
          bibliography should not include ("article.dox/parts/private")
          bibliography should not include ("article.dox/review/private")
          rdf should not include ("private-only")
          rdf should not include ("article.dox/parts/private")
        } finally {
          _delete(dir)
        }
      }
    }
  }

  private def _publication(dir: Path, source: String, context: Context): SitePublicationContext =
    SitePublicationContext.create(dir.toFile, dir.resolve("publication.conf").toFile, dir.resolve(source).toFile, context).take

  private def _fixture(): Path = {
    val parent = Files.createDirectories(Paths.get("target/phase75").toAbsolutePath.normalize)
    val dir = Files.createTempDirectory(parent, "effective-content-")
    _write(dir.resolve("site.conf"), "site.metadata.url = \"https://inferred.example.test/\"\n")
    _write(dir.resolve("publication.conf"),
      """|site.metadata.url = "https://publication.example.test/docs/"
         |site.output.locale_mode = "multi_locale_subdirs"
         |site.output.default_locale = "ja"
         |""".stripMargin)
    _write(dir.resolve("index.dox"), _document("Home｜ホーム", "published", "", "Home content."))
    _write(dir.resolve("guide/ordinary.dox"), _document("Ordinary｜通常記事", "published",
      "doi:10.5555/ordinary-admitted", "Article: site:[article.dox/index.html]"))
    _write(dir.resolve("guide/article.dox/index.dox"), _document("Article｜記事", "published",
      "doi:10.5555/package-admitted", "Ordinary sibling: site:[ordinary.dox]"))
    _write(dir.resolve("guide/nested/source.dox"), _document("Nested｜入れ子", "published", "",
      "Article: site:[../article.dox]"))
    _write(dir.resolve("guide/draft.dox/index.dox"), _document("Draft｜草稿", "draft", "", "Draft content."))
    _write(dir.resolve("guide/ordinary-draft.dox"), _document("Ordinary Draft｜通常草稿", "draft",
      "doi:10.5555/draft-admitted", "Draft reference content."))
    _write(dir.resolve("guide/article.dox/parts/private.dox"), _document("Private｜非公開", "published",
      "doi:10.5555/private-only", "Private source content."))
    _write(dir.resolve("guide/article.dox/review/private.md"), "_source\n\n# Private review\n\nUnselected review bytes.\n")
    Files.createDirectories(dir.resolve("guide/missing.dox"))
    dir
  }

  private def _document(title: String, status: String, reference: String, body: String): String = {
    val refs = if (reference.isEmpty) "" else "bibliography.refs = [\"" + reference + "\"]\n"
    s"$title\n====================================\n\n# HEAD\n\nstatus=$status\npublished_at=2026-10-01\n$refs\n## BRIEF\n\nPublication tooltip.\n\n# Body\n\n$body\n"
  }

  private def _write(path: Path, content: String): Unit = {
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, content.getBytes(StandardCharsets.UTF_8))
  }

  private def _delete(path: Path): Unit = {
    val stream = Files.walk(path)
    try {
      import scala.collection.JavaConverters._
      stream.iterator.asScala.toVector.reverse.foreach(Files.deleteIfExists)
    } finally {
      stream.close()
    }
  }
}
