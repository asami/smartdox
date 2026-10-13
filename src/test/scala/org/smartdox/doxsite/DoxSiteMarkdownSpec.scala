package org.smartdox.doxsite

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Locale
import scala.collection.JavaConverters._
import org.jsoup.Jsoup
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.i18n.I18NContext
import org.goldenport.realm.Realm
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.generators.AntoraGenerator
import org.smartdox.metadata.DocumentMetaData

/*
 * @since   Oct.  6, 2026
 * @version Oct. 13, 2026
 */
class DoxSiteMarkdownSpec
    extends AnyWordSpec
    with GivenWhenThen
    with SmartDoxSpecVocabulary {
  "Admitted Markdown site documents" should {
    "interpret the body and YAML metadata" which {
      "P751-01C2-01 retain literal Dox directives and genuine Markdown constructs for both suffixes" in {
        Given("admitted md and markdown articles with YAML metadata and body HEAD, status, include, site and bibliography literals")
        _with_fixture("grammar-") { root =>
          _write_config(root)
          val body = "# HEAD\n\nstatus = draft\npublished_at = 1999-01-01\n\ninclude::missing.dox[] site:[missing.dox] bib:[body-only]\n\n## Body *heading*\n\n- **strong** and *emphasis*\n- `include::code.dox[]`\n\n```scala\nsite:[code] bib:[code]\n```\n"
          Vector("md", "markdown").foreach { suffix =>
            _write(root.resolve(s"development-process/literal-$suffix.$suffix"),
              _markdown_source(s"YAML $suffix", body, metadata = "bibliography:\n  refs: [\"bib:yaml-only\"]"))
          }
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("the site parses the admitted Markdown articles and renders their shared documents")
          val site = create_site(context, root.toFile, _full_config)
          val realm = site_realm(site, context)
          val documents = Vector("md", "markdown").map { suffix =>
            suffix -> DoxSiteEffectiveContent.effectiveContent(site.space, s"development-process/literal-$suffix.$suffix").get.getDox
          }

          Then("only YAML supplies metadata while body directives remain text and Markdown supplies typed structure")
          documents.foreach { case (suffix, document) =>
            document.head.metadata.titleStringDefault shouldBe s"YAML $suffix"
            document.head.metadata.status shouldBe DocumentMetaData.Status.Published
            document.head.metadata.properties.get.getStringList("bibliography.refs").asScala.toVector shouldBe Vector("bib:yaml-only")
            val nodes = _nodes(document.body)
            nodes.collect { case section: Section =>
              section.title.map {
                case fragment: I18NFragment => fragment.distillString(Locale.ENGLISH)
                case inline => Dox.toText(List(inline))
              }.mkString
            } shouldBe List("HEAD", "Body heading", "Bibliography")
            nodes.collect { case list: Ul => list } should have size 1
            nodes.exists(_.isInstanceOf[Strong]) shouldBe true
            nodes.exists(_.isInstanceOf[Em]) shouldBe true
            nodes.exists(_.isInstanceOf[Code]) shouldBe true
            nodes.exists(_.isInstanceOf[InlineMacro]) shouldBe false
            nodes.collect { case link: Hyperlink if link.isSite => link } shouldBe empty
            val html = html_at(realm, s"ja/development-process/literal-$suffix.html")
            Vector("status = draft", "published_at = 1999-01-01", "include::missing.dox[]", "site:[missing.dox]", "bib:[body-only]", "site:[code] bib:[code]").foreach { literal =>
              html should include_html(literal)
            }
            html should include_html("<strong>strong</strong>")
            html should include_html("<em>emphasis</em>")
            html should include_html("language-scala")
          }
          site.metadata.bibliography.entries.map(_.id) should not contain "bib:body-only"
        }
      }

      "P751-01C2-02 filter both suffixes by YAML status despite contradictory body declarations" in {
        Given("published and draft YAML articles for both suffixes with opposite body status declarations")
        _with_fixture("strategy-") { root =>
          _write_config(root)
          Vector("md", "markdown").foreach { suffix =>
            _write(root.resolve(s"development-process/published-$suffix.$suffix"),
              _markdown_source(s"Published $suffix", "# HEAD\n\nstatus = draft\n\nPublished sentinel.", "published"))
            _write(root.resolve(s"development-process/draft-$suffix.$suffix"),
              _markdown_source(s"Draft $suffix", "# HEAD\n\nstatus = published\n\nDraft sentinel.", "draft"))
          }
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("Production selects and renders the parsed articles")
          val site = create_site(context, root.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Production))
          val realm = site_realm(site, context)

          Then("published YAML articles remain visible and draft YAML articles have no selected page or generated metadata")
          Vector("md", "markdown").foreach { suffix =>
            val published = s"development-process/published-$suffix.$suffix"
            val draft = s"development-process/draft-$suffix.$suffix"
            DoxSiteEffectiveContent.effectiveContent(site.space, published).get.getDox.head.metadata.status shouldBe DocumentMetaData.Status.Published
            DoxSiteEffectiveContent.effectiveContent(site.space, draft) shouldBe empty
            site.metadata.linkCollection.get.get(draft) shouldBe None
            Vector("ja", "en").foreach { language =>
              html_at(realm, s"$language/development-process/published-$suffix.html") should include_html("Published sentinel.")
              html_at(realm, s"$language/development-process/published-$suffix.html") should include_html("status = draft")
              realm.getString(s"$language/development-process/draft-$suffix.html") shouldBe None
            }
          }
          metadata_at(realm, "metadata/documents/fragments.json") should not (include_metadata("Draft sentinel."))
        }
      }
    }

    "retain public identity and package admission" which {
      "P751-01C2-03 share actual nested Markdown source identities across HTML aliases and projections" in {
        Given("nested admitted md and markdown articles with YAML bibliography and a Dox control")
        _with_fixture("routes-") { root =>
          _write_config(root)
          Vector("md", "markdown").foreach { suffix =>
            _write(root.resolve(s"development-process/guides/page-$suffix.$suffix"),
              _markdown_source(s"Page $suffix", s"# Page $suffix\n\nRoute sentinel $suffix.", metadata = "bibliography:\n  refs: [\"bib:route-reference\"]"))
          }
          _write(root.resolve("development-process/guides/control.dox"), _dox_source("Control｜対照", "Dox control sentinel."))
          _write(root.resolve("bibliography/concept/route-reference.bib"), "@book{route-reference, title = {Route Reference}, year = {2026}}\n")
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("source and normalized HTML aliases select parsed pages and the site and Antora project them")
          val site = create_site(context, root.toFile, _full_config)
          val realm = site_realm(site, context)
          val antora = _antora(context, root, _full_config)
          val selected = Vector("md", "markdown").map { suffix =>
            val source = s"development-process/guides/page-$suffix.$suffix"
            val aliases = Vector(source, s"development-process/guides/page-$suffix.html", s"/development-process/guides/page-$suffix.html", s"development-process\\guides\\page-$suffix.html")
            source -> aliases.map(path => DoxSiteEffectiveContent.effectiveContent(site.space, path).get)
          }

          Then("every alias retains the actual source and shared parsed page with canonical html and adoc output")
          selected.foreach { case (source, aliases) =>
            aliases.map(_.sourcePath.v).distinct shouldBe Vector(source)
            aliases.map(_.logicalPath.v).distinct shouldBe Vector(source)
            aliases.tail.foreach(value => (value.page eq aliases.head.page) shouldBe true)
            val publicpath = DoxSite.publicPath(source)
            publicpath shouldBe source.substring(0, source.lastIndexOf('.')) + ".html"
            DoxSite.publicPath("/" + source) shouldBe "/" + publicpath
            DoxSite.publicPath(source.replace('/', '\\')) shouldBe publicpath
            site.space.getContent(source.substring(0, source.lastIndexOf('.')) + ".dox") shouldBe empty
            site.metadata.linkCollection.get.get(source) should not be empty
            Vector("ja", "en").foreach { language =>
              html_at(realm, s"$language/$publicpath") should include_html("Route sentinel")
              val leaf = source.split('/').last
              antora.get(s"antora.d/$language/docs/development-process/modules/guides/pages/${leaf.substring(0, leaf.lastIndexOf('.'))}.adoc") should not be empty
            }
            metadata_at(realm, "metadata/documents/fragments.json") should include_metadata(publicpath)
            metadata_at(realm, "site.ttl") should include_metadata(publicpath)
          }
          site.metadata.bibliography.entries.find(_.id == "bib:route-reference").get.sourceRefs.map(ref => ref.sourcePath -> ref.publicPath).toSet shouldBe
            selected.map { case (source, _) => source -> DoxSite.publicPath(source) }.toSet
          DoxSiteEffectiveContent.effectiveContent(site.space, "development-process/guides/control.html").get.sourcePath.v shouldBe "development-process/guides/control.dox"
          html_at(realm, "ja/development-process/guides/control.html") should include_html("Dox control sentinel.")
          DoxSite.publicPath("guides/picture.png") shouldBe "guides/picture.png"
        }
      }

      "P751-01C2-04 exclude private Markdown before parsing while retaining ordinary directory descendants and siblings" in {
        Given("a package index with invalid private Markdown and a nested project, plus an ordinary .dox directory and following Markdown sibling")
        _with_fixture("admission-") { root =>
          _write_config(root)
          _write(root.resolve("development-process/article.dox/index.dox"), _dox_source("Article｜記事", "Public index sentinel."))
          val privatepaths = Vector("development-process/article.dox/private.md", "development-process/article.dox/arbitrary/deep/private.markdown")
          privatepaths.foreach { path =>
            _write(root.resolve(path), "---\n_source\n---\n\n![Private](../../../../outside.png)\nPrivate sentinel.\n")
          }
          val nested = "development-process/article.dox/arbitrary/nested.dox/index.dox"
          _write(root.resolve(nested), _dox_source("Private nested｜非公開", "Nested private sentinel."))
          _write(root.resolve("development-process/ordinary.dox/descendant.md"), _markdown_source("Descendant", "# Descendant\n\nOrdinary descendant sentinel."))
          _write(root.resolve("development-process/following.markdown"), _markdown_source("Following", "# Following\n\nFollowing sibling sentinel."))
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("normal site and Antora construction admit public sources before choosing their grammar")
          val site = create_site(context, root.toFile, _full_config)
          val realm = site_realm(site, context)
          val antora = _antora(context, root, _full_config)

          Then("private malformed and traversal sources never yield pages, links or metadata and public siblings remain")
          (privatepaths :+ nested).foreach { path =>
            site.space.getContent(path) shouldBe empty
            DoxSiteEffectiveContent.effectiveContent(site.space, path) shouldBe empty
            DoxSiteEffectiveContent.effectiveContent(site.space, DoxSite.publicPath(path)) shouldBe empty
            site.metadata.linkCollection.get.get(path) shouldBe None
            Vector("ja", "en").foreach { language => realm.getString(s"$language/${DoxSite.publicPath(path)}") shouldBe None }
          }
          DoxSiteEffectiveContent.effectiveContent(site.space, "development-process/article.dox/arbitrary/nested.dox") shouldBe empty
          DoxSiteEffectiveContent.effectiveContent(site.space, "development-process/ordinary.dox") shouldBe empty
          val fragments = metadata_at(realm, "metadata/documents/fragments.json")
          fragments should not (include_metadata("Private sentinel"))
          fragments should not (include_metadata("Nested private sentinel"))
          Vector("ja", "en").foreach { language =>
            html_at(realm, s"$language/development-process/article.html") should include_html("Public index sentinel.")
            html_at(realm, s"$language/development-process/ordinary.dox/descendant.html") should include_html("Ordinary descendant sentinel.")
            html_at(realm, s"$language/development-process/following.html") should include_html("Following sibling sentinel.")
            antora.get(s"antora.d/$language/docs/development-process/modules/ROOT/pages/article.adoc") should not be empty
            antora.get(s"antora.d/$language/docs/development-process/modules/ordinary.dox/pages/descendant.adoc") should not be empty
            antora.get(s"antora.d/$language/docs/development-process/modules/ROOT/pages/following.adoc") should not be empty
            antora.get(s"antora.d/$language/docs/development-process/modules/article.dox/pages/private.adoc") shouldBe empty
            antora.get(s"antora.d/$language/docs/development-process/modules/article.dox/pages/arbitrary/deep/private.adoc") shouldBe empty
            antora.get(s"antora.d/$language/docs/development-process/modules/article.dox/pages/arbitrary/nested.adoc") shouldBe empty
          }
          site.metadata.linkCollection.get.get("development-process/ordinary.dox/descendant.md") should not be empty
          site.metadata.linkCollection.get.get("development-process/following.markdown") should not be empty
        }
      }
    }

    "retain physical and virtual origins" which {
      "P751-01C2-05 resolve both suffixes' images from their source parents and reject traversal" in {
        Given("physical nested md and markdown pages with local images and separate traversal fixtures")
        _with_fixture("images-") { fixture =>
          val root = Files.createDirectories(fixture.resolve("physical"))
          _write_config(root)
          Vector("md", "markdown").foreach { suffix =>
            _write(root.resolve(s"guides/$suffix/images/diagram.png"), "")
            _write(root.resolve(s"guides/$suffix/page.$suffix"), "# Images\n\n![Diagram](images/../images/diagram.png \"Image title\")\n")
          }
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("physical site pages render with the existing Builder source context")
          val site = create_site(context, root.toFile, _full_config)
          val output = site_realm(site, context)

          Then("each image retains the normalized source-parent URI and title")
          Vector("md", "markdown").foreach { suffix =>
            val document = DoxSiteEffectiveContent.effectiveContent(site.space, s"guides/$suffix/page.$suffix").get.getDox
            _nodes(document).collect { case image: ReferenceImg => image } should have size 1
            html_at(output, s"ja/guides/$suffix/page.html") should include_html("src=\"images/diagram.png\"")
            html_at(output, s"ja/guides/$suffix/page.html") should include_html("title=\"Image title\"")
          }

          Given("an origin-less Realm with both Markdown suffixes and virtual relative images")
          val virtual = Realm.create()
          Vector("md", "markdown").foreach { suffix =>
            virtual.backend.setContent(s"guides/$suffix/page.$suffix", Realm.StringData("# Virtual\n\n![Diagram](images/diagram.png)\n", 1L))
          }
          val virtualcontext = Context.create()
          When("the virtual site renders those admitted sources")
          val virtualsite = DoxSite.create(virtualcontext, virtual, None, _full_config)
          val virtualoutput = site_realm(virtualsite, virtualcontext)
          Then("virtual images retain their source-relative paths")
          Vector("md", "markdown").foreach { suffix =>
            html_at(virtualoutput, s"ja/guides/$suffix/page.html") should include_html("src=\"images/diagram.png\"")
          }

          Vector("md", "markdown").foreach { suffix =>
            Given(s"physical and virtual $suffix image paths escaping their admitted source parent")
            val escape = Files.createDirectories(fixture.resolve(s"escape-$suffix"))
            _write(escape.resolve(s"guides/page.$suffix"), "![Escape](../outside.png)\n")
            val escapingvirtual = Realm.create()
            escapingvirtual.backend.setContent(s"guides/page.$suffix", Realm.StringData("![Escape](../../outside.png)\n", 1L))
            When("the site interprets the physical and virtual traversal destinations")
            val physicalfailure = intercept[IllegalArgumentException] { create_site(Context.create(), escape.toFile, _full_config) }
            val virtualfailure = intercept[IllegalArgumentException] { DoxSite.create(Context.create(), escapingvirtual, None, _full_config) }
            Then("the existing unsupported-resource diagnostic rejects both traversal forms")
            physicalfailure.getMessage should include_text("image.markdown.unsupported-resource")
            physicalfailure.getMessage should include_text("raw-path=../outside.png")
            virtualfailure.getMessage should include_text("image.markdown.unsupported-resource")
            virtualfailure.getMessage should include_text("raw-path=../../outside.png")
          }
        }
      }
    }

    "resolve localized links and publication" which {
      "P751-02A-SDX-FRAGMENT-01 preserve raw fragments across ordinary Markdown package aliases" in {
        Given("a published bilingual physical package with a real fragment target and two published bilingual Markdown sources with distinct stems")
        _with_fixture("fragments-") { root =>
          _write_config(root)
          _write(root.resolve("development-process/targets/package.dox/index.dox"),
            _dox_source("Package target｜対象パッケージ", "<dfn id=\"package-part\">Actual target.</dfn>"))
          val cases = (for {
            alias <- Vector("../targets/package.dox", "../targets/package.dox/index.dox", "../targets/package.dox/index.html", "../targets/package.html")
            fragment <- Vector("", "#package-part", "#package%2Dpart")
          } yield alias -> fragment).zipWithIndex
          val body = cases.map { case ((alias, fragment), index) =>
            s"Alias-$index [Authored caption]($alias$fragment)"
          }.mkString("\n\n")
          Vector("md", "markdown").foreach { suffix =>
            _write(root.resolve(s"development-process/sources/one-$suffix.$suffix"),
              _localized_markdown_source(s"Source $suffix", s"参照元 $suffix", body))
          }
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("one real site renders both distinct Markdown pages and their package target in Japanese and English")
          val site = create_site(context, root.toFile, _full_config)
          val realm = site_realm(site, context)

          Then("each marked paragraph has exactly one canonical article link with its authored raw fragment and the actual localized package caption")
          Vector("ja" -> "対象パッケージ", "en" -> "Package target").foreach { case (language, title) =>
            val target = Jsoup.parse(html_at(realm, s"$language/development-process/targets/package.html"))
            val definitions = target.select("dfn#package-part").asScala.toVector
            definitions should have size 1
            definitions.head.attr("id") shouldBe "package-part"
            definitions.head.text() shouldBe "Actual target."
            target.select("h1").asScala.map(_.text()).toVector should contain (title)
            Vector("md", "markdown").foreach { suffix =>
              val source = Jsoup.parse(html_at(realm, s"$language/development-process/sources/one-$suffix.html"))
              val paragraphs = source.select("p").asScala.filter(_.text().startsWith("Alias-")).toVector
              paragraphs should have size 12
              paragraphs.map(_.text().takeWhile(_ != ' ')) shouldBe cases.map { case (_, index) => s"Alias-$index" }
              paragraphs.zip(cases).foreach { case (paragraph, ((_, fragment), _)) =>
                val links = paragraph.select("a.article").asScala.toVector
                links should have size 1
                val link = links.head
                link.attr("href") shouldBe s"../targets/package.html$fragment"
                link.text().stripPrefix(DoxSite.Config.WorkAround.textMark.article) shouldBe title
                val uri = new URI(link.attr("href"))
                if (fragment.isEmpty) {
                  Option(uri.getRawFragment) shouldBe None
                } else {
                  uri.getRawFragment shouldBe fragment.stripPrefix("#")
                  uri.getFragment shouldBe definitions.head.attr("id")
                }
              }
            }
          }
        }
      }

      "P751-01C2-06 resolve real Dox site links and publication aliases through actual Markdown files" in {
        Given("a real Dox source linking to md and markdown source/HTML aliases with fragments and exact YAML JA/EN titles")
        _with_fixture("publication-") { root =>
          _write_config(root)
          _write(root.resolve("development-process/source.dox"), _dox_source("Source｜参照元",
            "First site:[guides/first.md#part]. Alias site:[guides/first.html#part]. Second site:[guides/second.markdown#part]. Alias site:[guides/second.html#part]."))
          _write(root.resolve("development-process/guides/first.md"), _localized_markdown_source("First", "第一", "# Part\n\nFirst body."))
          _write(root.resolve("development-process/guides/second.markdown"), _localized_markdown_source("Second", "第二", "# Part\n\nSecond body."))
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("site links and relations render and the explicit publication context resolves source and HTML aliases")
          val site = create_site(context, root.toFile, _full_config)
          val realm = site_realm(site, context)
          val publication = SitePublicationContext.create(root.toFile, root.resolve("publication.conf").toFile, root.resolve("development-process/source.dox").toFile, Context.create()).take
          val resolved = for {
            target <- Vector("guides/first.md#part", "guides/first.html#part", "guides/second.markdown#part", "guides/second.html#part")
            locale <- Vector(Locale.JAPANESE, Locale.ENGLISH)
          } yield (target, locale, publication.resolve(new URI(target), locale).take)

          Then("localized titles and fragments use canonical relative HTML and HTTPS routes with real Markdown source identities")
          Vector("ja" -> Vector("第一", "第二"), "en" -> Vector("First", "Second")).foreach { case (language, titles) =>
            val html = html_at(realm, s"$language/development-process/source.html")
            html should include_html("href=\"guides/first.html#part\"")
            html should include_html("href=\"guides/second.html#part\"")
            titles.foreach(title => html should include_html(title))
            html should not (include_html("site:[guides/"))
          }
          resolved.foreach { case (target, locale, link) =>
            val first = target.contains("first")
            val stem = if (first) "first" else "second"
            val title = if (locale == Locale.JAPANESE) { if (first) "第一" else "第二" } else { if (first) "First" else "Second" }
            link.uri.toString shouldBe s"https://publication.example.test/${locale.toLanguageTag}/development-process/guides/$stem.html"
            link.title shouldBe title
            link.document.relativeUri.toString shouldBe s"guides/$stem.html#part"
            link.document.publicPath shouldBe s"development-process/guides/$stem.html"
            val suffix = if (first) "md" else "markdown"
            link.document.targetPath shouldBe s"development-process/guides/$stem.$suffix"
          }
          Vector("first.md", "second.markdown").foreach { leaf =>
            val source = s"development-process/guides/$leaf"
            val effective = DoxSiteEffectiveContent.effectiveContent(publication.site.space, DoxSite.publicPath(source)).get
            effective.sourcePath.v shouldBe source
            Files.isRegularFile(root.resolve(effective.sourcePath.v)) shouldBe true
            site.metadata.linkCollection.get.get(source) should not be empty
          }
          Then("each Markdown target records the actual incoming source and renders its canonical localized Related Articles link")
          Vector("first.md", "second.markdown").foreach { leaf =>
            val source = s"development-process/guides/$leaf"
            val incoming = site.metadata.linkCollection.get.get(source).get.incomingLinks.links
            incoming.map(_.source.v).toSet shouldBe Set("development-process/source.dox")
            incoming should have size 1
            Vector("ja" -> "参照元", "en" -> "Source").foreach { case (language, title) =>
              val related = _related_links(html_at(realm, s"$language/${DoxSite.publicPath(source)}"), language)
              related should have size 2
              related.toSet shouldBe Set("../source.html" -> title)
            }
          }
          And("publication aliases still verify actual files without fabricated Dox targets")
          Files.exists(root.resolve("development-process/guides/first.dox")) shouldBe false
          Files.exists(root.resolve("development-process/guides/second.dox")) shouldBe false
          site.metadata.linkCollection.get.get("development-process/source.dox").get.internalLinks.links should not be empty
        }
      }

      "P751-01C2-07 retain publication failures for missing private draft symlink and outside-root Markdown" in {
        Given("a contained publication site with missing HTML, private/draft Markdown and a symlink into a separately owned fixture directory")
        _with_fixture("publication-failures-") { fixture =>
          val root = Files.createDirectories(fixture.resolve("site"))
          val outside = Files.createDirectories(fixture.resolve("outside"))
          _write_config(root)
          _write(root.resolve("source.dox"), _dox_source("Source｜参照元", "Source body."))
          _write(root.resolve("article.dox/index.dox"), _dox_source("Article｜記事", "Package body."))
          Vector("md", "markdown").foreach { suffix =>
            _write(root.resolve(s"article.dox/private.$suffix"), _localized_markdown_source("Private", "非公開", "Private body."))
            _write(root.resolve(s"draft-$suffix.$suffix"), _markdown_source("Draft", "Draft body.", "draft"))
            _write(outside.resolve(s"outside.$suffix"), _localized_markdown_source("Outside", "外部", "Outside body."))
            Files.createSymbolicLink(root.resolve(s"linked-$suffix.$suffix"), outside.resolve(s"outside.$suffix"))
          }
          _write(root.resolve("english-only.md"), _markdown_source("English only", "English body."))
          _write(root.resolve("insecure.conf"), _configuration("http://publication.example.test/"))
          val cases = Vector("missing.html" -> "pdf.site-link.target.unresolved") ++ Vector("md", "markdown").flatMap { suffix =>
            Vector(s"article.dox/private.$suffix" -> "pdf.site-link.target.unresolved", s"draft-$suffix.$suffix" -> "pdf.site-link.target.unresolved", s"draft-$suffix.html" -> "pdf.site-link.target.unresolved", s"linked-$suffix.$suffix" -> "pdf.site-link.target.outside-root", s"../outside/outside.$suffix" -> "pdf.site-link.target.outside-root")
          }

          When("the explicit publication context resolves the inadmissible targets and inherited locale/base controls")
          val publication = SitePublicationContext.create(root.toFile, root.resolve("publication.conf").toFile, root.resolve("source.dox").toFile, Context.create()).take
          val failures = cases.map { case (target, diagnostic) => diagnostic -> publication.resolve(new URI(target), Locale.JAPANESE) }
          val missingtitle = publication.resolve(new URI("english-only.html"), Locale.JAPANESE)
          val badlocale = publication.resolve(new URI("english-only.html"), Locale.FRENCH)
          val insecure = SitePublicationContext.create(root.toFile, root.resolve("insecure.conf").toFile, root.resolve("source.dox").toFile, Context.create())

          Then("no rejected target produces a URI and existing diagnostics, exact locale titles and HTTPS rules remain")
          failures.foreach { case (diagnostic, result) =>
            result.isError shouldBe true
            result.toOption shouldBe None
            result.message should include_text(diagnostic)
          }
          missingtitle.isError shouldBe true
          missingtitle.message should include_text("pdf.site-link.target.localized-title-missing")
          badlocale.isError shouldBe true
          insecure.isError shouldBe true
          insecure.message should include_text("pdf.site-context.base-url.invalid")
        }
      }
    }

    "retain parsed incoming relations" which {
      "P751-01C2-09 collect source and HTML fragment aliases under actual Markdown Dox and package owners" in {
        Given("md, markdown, ordinary Dox and package targets with a unique localized Dox source for each source or HTML alias")
        _with_fixture("incoming-") { root =>
          _write_config(root)
          val targets = Vector("md" -> "page-md.md", "markdown" -> "page-markdown.markdown", "dox" -> "page-dox.dox", "package" -> "page-package.dox")
          val cases = targets.flatMap { case (kind, leaf) =>
            val aliases = Vector(leaf, DoxSite.publicPath(leaf)) ++
              (if (kind == "package") Vector(s"$leaf/index.dox", s"$leaf/index.html") else Vector.empty)
            val targetpath = s"development-process/targets/$leaf"
            val physicalpath = if (kind == "package") s"$targetpath/index.dox" else targetpath
            val targetsource = if (kind == "md" || kind == "markdown")
              _localized_markdown_source(s"Target $kind", s"対象 $kind", "# Part\n\nTarget body.")
            else _dox_source(s"Target $kind｜対象 $kind", "Target body.")
            _write(root.resolve(physicalpath), targetsource)
            for {
              (alias, index) <- aliases.zipWithIndex
              fragment <- Vector(false, true)
            } yield {
              val name = s"source-$kind-$index-${if (fragment) "fragment" else "plain"}"
              val href = s"../targets/$alias${if (fragment) "#part" else ""}"
              val sourcepath = s"development-process/sources/$name.dox"
              _write(root.resolve(sourcepath), _dox_source(s"Source $name｜参照元 $name", s"Target site:[$href]."))
              (targetpath, sourcepath, name, href)
            }
          }
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("the parsed site collects incoming identities and renders both localized target and source pages")
          val site = create_site(context, root.toFile, _full_config)
          val realm = site_realm(site, context)

          Then("every target has exactly its real source identities and localized Related Articles href and title sets")
          targets.foreach { case (_, leaf) =>
            val targetpath = s"development-process/targets/$leaf"
            val expected = cases.filter(_._1 == targetpath)
            val incoming = site.metadata.linkCollection.get.get(targetpath).get.incomingLinks.links
            incoming should have size expected.size
            incoming.map(_.source.v).toSet shouldBe expected.map(_._2).toSet
            Vector("ja", "en").foreach { language =>
              val related = _related_links(html_at(realm, s"$language/${DoxSite.publicPath(targetpath)}"), language)
              related should have size expected.size
              related.toSet shouldBe expected.map { case (_, _, name, _) =>
                val title = if (language == "ja") s"参照元 $name" else s"Source $name"
                s"../sources/$name.html" -> title
              }.toSet
            }
          }
          And("final source relations and rendered site links retain the established canonical HTML hrefs and fragments")
          cases.foreach { case (targetpath, sourcepath, _, href) =>
            val canonical = s"../targets/${DoxSite.publicPath(targetpath).split('/').last}${if (href.endsWith("#part")) "#part" else ""}"
            site.metadata.linkCollection.get.get(sourcepath).get.internalLinks.links.map(_.href.toString).toSet should contain (canonical)
            Vector("ja", "en").foreach { language =>
              val html = html_at(realm, s"$language/${DoxSite.publicPath(sourcepath)}")
              Jsoup.parse(html).select("a[href]").asScala.map(_.attr("href")).toSet should contain (canonical)
            }
          }
        }
      }

      "P751-01C2-10 exclude reciprocal source and HTML fragment aliases while retaining nonreciprocal observers" in {
        Given("md, markdown, ordinary Dox and package targets linking back to two incoming sources through source and HTML fragment aliases")
        _with_fixture("reciprocal-") { root =>
          _write_config(root)
          val targets = Vector("md" -> "page-md.md", "markdown" -> "page-markdown.markdown", "dox" -> "page-dox.dox", "package" -> "page-package.dox")
          targets.foreach { case (kind, leaf) =>
            val targetpath = s"development-process/targets/$leaf"
            val backlinks = Vector("source", "html").map { alias =>
              val name = s"reciprocal-$kind-$alias"
              val href = s"../sources/$name.${if (alias == "source") "dox" else "html"}#part"
              if (kind == "md" || kind == "markdown") s"[Back $alias]($href)" else s"Back site:[$href]."
            }.mkString("\n\n")
            val physicalpath = if (kind == "package") s"$targetpath/index.dox" else targetpath
            val targetsource = if (kind == "md" || kind == "markdown")
              _localized_markdown_source(s"Target $kind", s"対象 $kind", s"# Part\n\n$backlinks")
            else _dox_source(s"Target $kind｜対象 $kind", backlinks)
            _write(root.resolve(physicalpath), targetsource)
            Vector("source", "html", "observer").foreach { role =>
              val name = if (role == "observer") s"observer-$kind" else s"reciprocal-$kind-$role"
              val alias = if (role == "source") leaf else DoxSite.publicPath(leaf)
              _write(root.resolve(s"development-process/sources/$name.dox"),
                _dox_source(s"Source $name｜参照元 $name", s"Target site:[../targets/$alias#part]."))
            }
          }
          val context = Context.create()
          implicit val i18ncontext: I18NContext = context.i18NContext

          When("the parsed site records all incoming relations and renders localized target references")
          val site = create_site(context, root.toFile, _full_config)
          val realm = site_realm(site, context)

          Then("incoming identity sets retain reciprocal sources and observers while Related Articles contains exactly the observer")
          targets.foreach { case (kind, leaf) =>
            val targetpath = s"development-process/targets/$leaf"
            val incoming = site.metadata.linkCollection.get.get(targetpath).get.incomingLinks.links
            incoming should have size 3
            incoming.map(_.source.v).toSet shouldBe Set(
              s"development-process/sources/reciprocal-$kind-source.dox",
              s"development-process/sources/reciprocal-$kind-html.dox",
              s"development-process/sources/observer-$kind.dox")
            Vector("ja", "en").foreach { language =>
              val title = if (language == "ja") s"参照元 observer-$kind" else s"Source observer-$kind"
              _related_links(html_at(realm, s"$language/${DoxSite.publicPath(targetpath)}"), language) shouldBe
                Vector(s"../sources/observer-$kind.html" -> title)
            }
          }
        }
      }
    }

    "retain finite parsed-tree alias precedence" which {
      "P751-01C2-08 prefer historical Dox and package owners then md then markdown without reparsing" in {
        Given("a virtual parsed tree with Dox/Markdown collisions, a package collision, both Markdown suffixes and a markdown-only article")
        val realm = Realm.create()
        Vector(
          "same.dox" -> _dox_source("Dox winner", "Dox body."),
          "same.md" -> _markdown_source("MD loser", "MD body."),
          "package.dox/index.dox" -> _dox_source("Package winner", "Package body."),
          "package.md" -> _markdown_source("Package MD loser", "MD body."),
          "both.md" -> _markdown_source("MD winner", "MD body."),
          "both.markdown" -> _markdown_source("Markdown loser", "Markdown body."),
          "only.markdown" -> _markdown_source("Markdown winner", "Markdown body.")
        ).foreach { case (path, source) => realm.backend.setContent(path, Realm.StringData(source, 1L)) }
        val site = DoxSite.create(Context.create(), realm, None, _full_config)
        val expected = Vector("same.html" -> "same.dox", "package.html" -> "package.dox/index.dox", "both.html" -> "both.md", "only.html" -> "only.markdown")
        realm.backend.setContent("both.md", Realm.StringData("![Invalid](../../outside.png)", 2L))
        realm.backend.setContent("absent.md", Realm.StringData(_markdown_source("Not parsed", "Not parsed body."), 2L))

        When("HTML aliases resolve only against the already parsed tree after origin bytes change")
        val selected = expected.map { case (alias, _) => alias -> DoxSiteEffectiveContent.effectiveContent(site.space, alias).get }
        val absent = DoxSiteEffectiveContent.effectiveContent(site.space, "absent.html")
        val direct = DoxSiteEffectiveContent.effectiveContent(site.space, "both.markdown").get

        Then("legacy candidates win and the ordered Markdown fallback uses retained pages without new source parsing")
        selected.map { case (alias, content) => alias -> content.sourcePath.v } shouldBe expected
        selected.map(_._2.getDox.head.metadata.titleStringDefault) shouldBe Vector("Dox winner", "Package winner", "MD winner", "Markdown winner")
        direct.sourcePath.v shouldBe "both.markdown"
        direct.getDox.head.metadata.titleStringDefault shouldBe "Markdown loser"
        absent shouldBe None
        DoxSiteEffectiveContent.effectivePath("", "both.html") shouldBe "both.dox"
        DoxSiteEffectiveContent.effectiveContent(site.space, "absent.txt") shouldBe None
      }
    }
  }

  private def _full_config: DoxSite.Config = DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full)

  private def _related_links(html: String, language: String): Vector[(String, String)] = {
    val heading = if (language == "ja") "関連記事" else "Related Articles"
    val sections = Jsoup.parse(html).select("section").asScala.filter { section =>
      section.children().asScala.exists(child => child.tagName().matches("h[1-6]") && child.text() == heading)
    }.toVector
    sections should have size 1
    val section = sections.head
    section.children().asScala.filter(_.tagName().matches("h[1-6]")).map(_.text()).toVector shouldBe
      Vector(heading)
    section.select("a[href]").asScala.map { link =>
      link.attr("href") -> link.text().stripPrefix(DoxSite.Config.WorkAround.textMark.article)
    }.toVector
  }

  private def _with_fixture(prefix: String)(body: Path => Unit): Unit = {
    val target = Files.createDirectories(Paths.get("target", "doxsite-markdown").toAbsolutePath)
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

  private def _markdown_source(title: String, body: String, status: String = "published", metadata: String = ""): String =
    s"---\ntitle: $title\nstatus: $status\npublished_at: 2026-10-06\n$metadata\n---\n\n$body\n"

  private def _localized_markdown_source(en: String, ja: String, body: String): String =
    _markdown_source(s"$en｜$ja", body)

  private def _dox_source(title: String, body: String): String =
    s"$title\n=====================\n\n# HEAD\n\nstatus = published\npublished_at = 2026-10-06\n\n# Body\n\n$body\n"

  private def _antora(context: Context, root: Path, config: DoxSite.Config): Realm =
    new AntoraGenerator(context, config).generate(Realm.create(DoxSite.realmConfig, root.toFile))

  private def _nodes(dox: Dox): List[Dox] =
    dox :: (dox match {
      case section: Section => section.title.flatMap(_nodes) ::: section.contents.flatMap(_nodes)
      case other => other.elements.flatMap(_nodes)
    })

  private def _delete_fixture(root: Path): Unit = {
    val stream = Files.walk(root)
    try stream.iterator.asScala.toVector.reverse.foreach(Files.deleteIfExists) finally stream.close()
  }
}
