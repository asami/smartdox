package org.smartdox.doxsite

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Locale
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.i18n.I18NContext
import org.goldenport.realm.Realm
import org.smartdox.SmartDoxSpecVocabulary
import org.smartdox.generator.Context
import org.smartdox.generators.AntoraGenerator

/*
 * @since   Oct.  6, 2026
 * @version Oct.  6, 2026
 */
class DoxSitePublicContentSpec
    extends AnyWordSpec
    with GivenWhenThen
    with SmartDoxSpecVocabulary {
  "DoxSite effective public content" should {
    "admission and parsed article identity" which {
      "P751-01B-01 retain one ordinary and one packaged article with localized routes and direct resources" in {
        Given("a physical ordinary article and a direct package index with category metadata and an image")
        _with_fixture("identity-") { root =>
          _write_config(root)
          _write(root.resolve("development-process/ordinary.dox"), _article_source("Ordinary｜通常記事", "Ordinary sentinel."))
          _write(root.resolve("development-process/article.dox/index.dox"), _article_source("Article｜記事", "Package sentinel."))
          _write(root.resolve("development-process/article.dox/category.yaml"), "name: architecture\ntitle: Architecture\ndescription: Package category.\n")
          _write(root.resolve("development-process/article.dox/diagram.png"), "")
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext
          val config = DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full)

          When("the site renders admitted parsed articles and Antora projects the effective view")
          val site = create_site(context, root.toFile, config)
          val realm = site_realm(site, context)
          val antora = _antora(context, root, config)
          val ordinary = DoxSiteEffectiveContent.effectiveContent(site.space, "development-process/ordinary.dox").get
          val aliases = Vector("article.dox", "article.dox/index.dox", "article.dox/index.html")
          val selected = aliases.map(path => DoxSiteEffectiveContent.effectiveContent(site.space, s"development-process/$path").get)

          Then("ordinary identity is unchanged and all package aliases select the same retained physical index")
          ordinary.logicalPath.v shouldBe "development-process/ordinary.dox"
          ordinary.sourcePath.v shouldBe "development-process/ordinary.dox"
          selected.map(_.logicalPath.v).distinct shouldBe Vector("development-process/article.dox")
          selected.map(_.sourcePath.v).distinct shouldBe Vector("development-process/article.dox/index.dox")
          selected.map(_.getDox).distinct.size shouldBe 1
          site.space.getContent("development-process/article.dox/index.dox") should not be empty
          site.space.getContent("development-process/article.dox/category.yaml") should not be empty
          site.space.getContent("development-process/article.dox/diagram.png") should not be empty
          Vector("ja", "en").foreach { language =>
            html_at(realm, s"$language/development-process/ordinary.html") should include_html("Ordinary sentinel.")
            html_at(realm, s"$language/development-process/article.html") should include_html("Package sentinel.")
            realm.getString(s"$language/development-process/article.dox/index.html") shouldBe None
            realm.get(s"$language/development-process/article.dox/diagram.png") should not be empty
            antora.get(s"antora.d/$language/docs/development-process/modules/ROOT/pages/ordinary.adoc") should not be empty
            antora.get(s"antora.d/$language/docs/development-process/modules/ROOT/pages/article.adoc") should not be empty
            antora.get(s"antora.d/$language/docs/development-process/modules/article.dox/pages/index.adoc") shouldBe empty
          }
          Vector(
            "/development-process/article.dox" -> "/development-process/article.html",
            "development-process\\article.dox\\index.dox" -> "development-process/article.html",
            "development-process/article.dox/index.html" -> "development-process/article.html"
          ).foreach { case (source, expected) => DoxSite.publicPath(source) shouldBe expected }
        }
      }

      "P751-01B-02 exclude arbitrary private documents and nested valid projects before parsing or reference collection" in {
        Given("an outer package with direct and deeply nested private suffixes, a bare _source Markdown file and a published private citation")
        _with_fixture("private-closure-") { root =>
          _write_config(root)
          _write(root.resolve("development-process/article.dox/index.dox"), _article_source("Article｜記事", "Public sentinel."))
          val privatepaths = for {
            directory <- Vector("", "arbitrary/deeper/")
            suffix <- Vector("md", "markdown", "org", "dox")
          } yield s"development-process/article.dox/${directory}private.$suffix"
          privatepaths.foreach { path =>
            val source = if (path.endsWith(".md") || path.endsWith(".markdown"))
              "---\ntitle: Private sentinel\nstatus: published\nbibliography:\n  refs: [\"bib:private-leak\"]\n---\n\nPrivate sentinel.\n"
            else
              _article_source("Private sentinel", "Private sentinel.", "bibliography.refs = [\"bib:private-leak\"]")
            _write(root.resolve(path), source)
          }
          val nested = "development-process/article.dox/arbitrary/nested.dox/index.dox"
          _write(root.resolve(nested), _article_source("Private nested sentinel", "Nested private sentinel.", "bibliography.refs = [\"bib:private-leak\"]"))
          val barepath = "development-process/article.dox/review/bare.md"
          val baresource = "---\n_source\n---\n\nPrivate bare source.\n"
          _write(root.resolve(barepath), baresource)
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext
          val config = DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full)

          When("the normal site and Antora pipelines select the outer project's direct article")
          val site = create_site(context, root.toFile, config)
          val realm = site_realm(site, context)
          val antora = _antora(context, root, config)
          val links = site.metadata.linkCollection.get
          val bibliography = metadata_at(realm, "metadata/bibliography/bibliography.json")
          val fragments = metadata_at(realm, "metadata/documents/fragments.json")

          Then("no private source yields parsed pages, logical links, Antora pages or generated metadata")
          (privatepaths ++ Vector(nested, barepath)).foreach { path =>
            site.space.getContent(path) shouldBe empty
            DoxSiteEffectiveContent.effectiveContent(site.space, path) shouldBe empty
            links.get(path) shouldBe None
            Vector("ja", "en").foreach { language =>
              realm.getString(s"$language/${DoxSite.publicPath(path)}") shouldBe None
              val relative = path.stripPrefix("development-process/article.dox/")
              val page = relative.substring(0, relative.lastIndexOf('.')) + ".adoc"
              antora.get(s"antora.d/$language/docs/development-process/modules/article.dox/pages/$page") shouldBe empty
            }
          }
          links.get("development-process/article.dox/arbitrary/nested.dox") shouldBe None
          antora.get("antora.d/ja/docs/development-process/modules/article.dox/pages/arbitrary/nested.adoc") shouldBe empty
          bibliography should not (include_metadata("bib:private-leak"))
          fragments should not (include_metadata("Private sentinel"))
          fragments should not (include_metadata("Private nested sentinel"))
          fragments should not (include_metadata("review/bare"))
          html_at(realm, "ja/development-process/article.html") should include_html("Public sentinel.")
          new String(Files.readAllBytes(root.resolve(barepath)), StandardCharsets.UTF_8) shouldBe baresource
        }
      }

      "P751-01B-03 retain selected metadata and inline bibliography references exactly once under logical source paths" in {
        Given("published ordinary/package citations, curated and declared BibTeX entries, and private/draft citations")
        _with_fixture("bibliography-") { root =>
          _write_config(root)
          _write(root.resolve("development-process/ordinary.dox"), _article_source("Ordinary｜通常記事", "Inline bib:[inline-reference].", "bibliography.refs = [\"bib:metadata-reference\"]"))
          _write(root.resolve("development-process/article.dox/index.dox"), _article_source("Article｜記事", "Inline bib:[inline-reference].", "bibliography.refs = [\"bib:metadata-reference\"]"))
          _write(root.resolve("development-process/article.dox/private.dox"), _article_source("Private", "Private bib:[private-inline].", "bibliography.refs = [\"bib:private-leak\"]"))
          _write(root.resolve("development-process/draft.dox"), _article_source("Draft", "Draft bib:[draft-inline].", "bibliography.refs = [\"bib:draft-leak\"]", "draft"))
          _write(root.resolve("bibliography/concept/metadata-reference.bib.dox"), _article_source("Curated Reference", "Curated body.", "id = \"bib:metadata-reference\"\nkey = \"metadata-reference\""))
          _write(root.resolve("bibliography/concept/metadata-reference.bib"), "@book{metadata-reference, title = {Shadow Reference}, year = {1999}}\n")
          _write(root.resolve("bibliography/concept/inline-reference.bib"), "@book{inline-reference, title = {Declared Inline Reference}, year = {2026}}\n")
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("Production builds bibliography and localized articles from selected parsed documents")
          val site = create_site(context, root.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
          val realm = site_realm(site, context)
          val entries = site.metadata.bibliography.entries
          val metadataentry = entries.find(_.id == "bib:metadata-reference").get
          val inlineentry = entries.find(_.id == "bib:inline-reference").get

          Then("each metadata or inline citation has one logical source reference per admitted article")
          entries.count(_.id == "bib:metadata-reference") shouldBe 1
          metadataentry.title shouldBe "Curated Reference"
          inlineentry.title shouldBe "Declared Inline Reference"
          inlineentry.sourceKind shouldBe "bibtex-only"
          Vector(metadataentry, inlineentry).foreach { entry =>
            entry.sourceRefs.map(ref => ref.sourcePath -> ref.publicPath).sorted shouldBe Vector(
              "development-process/article.dox" -> "development-process/article.html",
              "development-process/ordinary.dox" -> "development-process/ordinary.html"
            )
            entry.sourceRefs.map(_.ordinal) shouldBe Vector.fill(2)(if (entry.id == "bib:metadata-reference") 1 else 2)
          }
          entries.map(_.id) should not contain "bib:private-leak"
          entries.map(_.id) should not contain "bib:private-inline"
          entries.map(_.id) should not contain "bib:draft-leak"
          entries.map(_.id) should not contain "bib:draft-inline"
          metadata_at(realm, "site.ttl") should include_metadata("development-process/article.html")
          metadata_at(realm, "metadata/bibliography/bibliography.json") should not (include_metadata("article.dox/index.dox"))
          html_at(realm, "en/development-process/article.html") should include_html("bibliography-citation")
          realm.getString("en/development-process/draft.html") shouldBe None
        }
      }
    }

    "localized site links and publication" which {
      "P751-01B-04 resolve real package aliases and reciprocal links with localized titles and logical identities" in {
        Given("an ordinary sibling linking through both package aliases and an index linking back")
        _with_fixture("links-") { root =>
          _write_config(root)
          _write(root.resolve("development-process/ordinary.dox"), _article_source("Ordinary｜通常記事", "First site:[article.dox]. Second site:[article.dox/index.html]."))
          _write(root.resolve("development-process/article.dox/index.dox"), _article_source("Article｜記事", "Sibling site:[ordinary.dox]."))
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("the site enables real HTML links and collects reciprocal relations")
          val site = create_site(context, root.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
          val realm = site_realm(site, context)
          val links = site.metadata.linkCollection.get

          Then("both aliases render canonical localized links and incoming relations retain only logical sources")
          Vector("ja" -> "記事", "en" -> "Article").foreach { case (language, title) =>
            val ordinaryhtml = html_at(realm, s"$language/development-process/ordinary.html")
            ordinaryhtml should include_html("href=\"article.html\"")
            ordinaryhtml should include_html(title)
            ordinaryhtml should not (include_html("article.dox/index.html\""))
            ordinaryhtml should not (include_text("site:[article.dox]"))
            ordinaryhtml should not (include_text("site:[article.dox/index.html]"))
            html_at(realm, s"$language/development-process/article.html") should include_html("href=\"ordinary.html\"")
          }
          links.get("development-process/article.dox").map(_.incomingLinks.links.map(_.source.v).toSet) shouldBe Some(Set("development-process/ordinary.dox"))
          links.get("development-process/ordinary.dox").map(_.incomingLinks.links.map(_.source.v).toSet) shouldBe Some(Set("development-process/article.dox"))
          links.get("development-process/article.dox/index.dox") shouldBe None
          links.get("development-process/article.dox/index.html") shouldBe None
        }
      }

      "P751-01B-05 verify the physical package index for ordinary-source publication aliases and selected JA/EN HTTPS routes" in {
        Given("a real ordinary physical source, a published package and distinct inferred/selected publication configurations")
        _with_fixture("publication-") { root =>
          _write_config(root)
          _write(root.resolve("source.dox"), _article_source("Source｜参照元", "Source article."))
          _write(root.resolve("article.dox/index.dox"), _article_source("Article｜記事", "Package article."))
          val context = Context.create()

          When("the explicit publication context resolves logical, physical Dox and physical HTML target references in each locale")
          val publication = SitePublicationContext.create(root.toFile, root.resolve("publication.conf").toFile, root.resolve("source.dox").toFile, context).take
          val resolved = for {
            target <- Vector("article.dox", "article.dox/index.dox", "article.dox/index.html")
            language <- Vector(Locale.JAPANESE, Locale.ENGLISH)
          } yield language -> publication.resolve(new URI(target), language).take

          Then("a package directory is resolved through its regular physical index with the selected base and localized title")
          publication.sourcePath shouldBe "source.dox"
          resolved.foreach { case (language, link) =>
            link.uri.toString shouldBe s"https://publication.example.test/${language.toLanguageTag}/article.html"
            link.title shouldBe (if (language == Locale.JAPANESE) "記事" else "Article")
            link.document.publicPath shouldBe "article.html"
            link.document.relativeUri.toString shouldBe "article.html"
          }
          Files.isDirectory(root.resolve("article.dox")) shouldBe true
          Files.isRegularFile(root.resolve("article.dox/index.dox")) shouldBe true
        }
      }

      "P751-01B-06 retain structured failures for missing unpublished private symlink and outside-root targets" in {
        Given("a contained publication site with a private article, a draft and a leaf symlink to a separate target-scoped root")
        _with_fixture("publication-failures-") { fixture =>
          val root = Files.createDirectories(fixture.resolve("site"))
          val outside = Files.createDirectories(fixture.resolve("outside"))
          _write_config(root)
          _write(root.resolve("source.dox"), _article_source("Source｜参照元", "Source article."))
          _write(root.resolve("article.dox/index.dox"), _article_source("Article｜記事", "Package article."))
          _write(root.resolve("article.dox/private.dox"), _article_source("Private｜非公開", "Private article."))
          _write(root.resolve("draft.dox"), _article_source("Draft｜下書き", "Draft article.", "", "draft"))
          _write(outside.resolve("outside.dox"), _article_source("Outside｜外部", "Outside article."))
          Files.createSymbolicLink(root.resolve("linked.dox"), outside.resolve("outside.dox"))
          val context = Context.create()
          val cases = Vector(
            "missing.dox" -> "pdf.site-link.target.unresolved",
            "draft.dox" -> "pdf.site-link.target.unresolved",
            "article.dox/private.dox" -> "pdf.site-link.target.unresolved",
            "linked.dox" -> "pdf.site-link.target.outside-root",
            "../outside/outside.dox" -> "pdf.site-link.target.outside-root"
          )

          When("the explicit context resolves each inadmissible target and attempts an outside-root source")
          val publication = SitePublicationContext.create(root.toFile, root.resolve("publication.conf").toFile, root.resolve("source.dox").toFile, context).take
          val failures = cases.map { case (target, diagnostic) => diagnostic -> publication.resolve(new URI(target), Locale.JAPANESE) }
          val external = SitePublicationContext.create(root.toFile, root.resolve("publication.conf").toFile, outside.resolve("outside.dox").toFile, context)

          Then("none emits a public URI and all retain the established failure diagnostics")
          failures.foreach { case (diagnostic, result) =>
            result.isError shouldBe true
            result.message should include_text(diagnostic)
          }
          external.isError shouldBe true
          external.message should include_text("pdf.site-context.source.outside-root")
        }
      }
    }

    "ordinary directories and Antora module paths" which {
      "P751-01B-07 keep descendants and following siblings of a .dox directory without a direct index public" in {
        Given("an ordinary .dox directory containing a nested project and a following sibling but no direct index")
        _with_fixture("ordinary-directory-") { root =>
          _write_config(root)
          _write(root.resolve("development-process/guides/nested.dox/article.dox/index.dox"), _article_source("Article｜記事", "Nested project sentinel."))
          _write(root.resolve("development-process/guides/nested.dox/following.dox"), _article_source("Following｜後続記事", "Following sibling sentinel."))
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext
          val config = DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full)

          When("the site and Antora project ordinary descendants after the nested effective article")
          val site = create_site(context, root.toFile, config)
          val realm = site_realm(site, context)
          val antora = _antora(context, root, config)

          Then("the directory remains ordinary and its following sibling retains the nested module path")
          DoxSiteEffectiveContent.effectiveContent(site.space, "development-process/guides/nested.dox") shouldBe empty
          DoxSiteEffectiveContent.effectiveContent(site.space, "development-process/guides/nested.dox/following.dox").map(_.sourcePath.v) shouldBe Some("development-process/guides/nested.dox/following.dox")
          site.metadata.linkCollection.get.get("development-process/guides/nested.dox/following.dox") should not be empty
          Vector("ja", "en").foreach { language =>
            html_at(realm, s"$language/development-process/guides/nested.dox/following.html") should include_html("Following sibling sentinel.")
            html_at(realm, s"$language/development-process/guides/nested.dox/article.html") should include_html("Nested project sentinel.")
            antora.get(s"antora.d/$language/docs/development-process/modules/guides/pages/nested.dox/article.adoc") should not be empty
            antora.get(s"antora.d/$language/docs/development-process/modules/guides/pages/nested.dox/following.adoc") should not be empty
            antora.get(s"antora.d/$language/docs/development-process/modules/guides/pages/following.adoc") shouldBe empty
          }
        }
      }
    }
  }

  private def _with_fixture(prefix: String)(body: Path => Unit): Unit = {
    val target = Files.createDirectories(Paths.get("target", "doxsite-public-content").toAbsolutePath)
    val root = Files.createTempDirectory(target, prefix)
    try body(root) finally _delete_fixture(root)
  }

  private def _write(path: Path, content: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, content.getBytes(StandardCharsets.UTF_8))
  }

  private def _write_config(root: Path): Unit = {
    _write(root.resolve("site.conf"), _configuration("https://inferred.example.test/"))
    _write(root.resolve("publication.conf"), _configuration("https://publication.example.test/"))
  }

  private def _configuration(url: String): String =
    s"""|site.metadata.url = "$url"
        |site.output.locale_mode = "multi_locale_subdirs"
        |site.output.default_locale = "ja"
        |""".stripMargin

  private def _article_source(title: String, body: String, metadata: String = "", status: String = "published"): String =
    s"""|$title
        |=====================
        |
        |# HEAD
        |
        |status = "$status"
        |published_at = "2026-10-06"
        |$metadata
        |
        |# Body
        |
        |$body
        |""".stripMargin

  private def _antora(context: Context, root: Path, config: DoxSite.Config): Realm =
    new AntoraGenerator(context, config).generate(Realm.create(DoxSite.realmConfig, root.toFile))

  private def _delete_fixture(root: Path): Unit = {
    val stream = Files.walk(root)
    try {
      import scala.collection.JavaConverters._
      stream.iterator.asScala.toVector.reverse.foreach(Files.deleteIfExists)
    } finally stream.close()
  }
}
