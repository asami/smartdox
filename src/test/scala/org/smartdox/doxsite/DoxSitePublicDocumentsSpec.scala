package org.smartdox.doxsite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._
import org.goldenport.realm.Realm
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.generator.Context

/*
 * @since   Oct.  7, 2026
 * @version Oct.  7, 2026
 */
class DoxSitePublicDocumentsSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Structural public document sources" should {
    "retain physical identities and producer-selected public routes" which {
      "retain root leaves and index alongside nested and package controls (P751-02F-CATALOGUE-SDX-11)" in {
        Given("unordered root suffixes and index, nested ordinary and package sources, private controls and a Japanese leaf")
        val paths = Vector("z.markdown", "b.org", "index.dox", "a.dox", "c.md",
          "docs/ordinary.org", "package.dox/index.dox", "package.dox/private.md",
          "_partial.dox", "assets/hidden.md", "日本語 note.md")
        val realm = _realm(paths)
        When("the actual in-memory Realm is structurally selected")
        val selected = DoxSitePublicDocuments.sources(realm)
        Then("the entire ordered vector retains exact root and nested physical identities and canonical routes")
        _pairs(selected) shouldBe Vector(
          "a.dox" -> "a.html",
          "b.org" -> "b.html",
          "c.md" -> "c.html",
          "docs/ordinary.org" -> "docs/ordinary.html",
          "index.dox" -> "index.html",
          "package.dox/index.dox" -> "package.html",
          "z.markdown" -> "z.html",
          "日本語 note.md" -> "日本語 note.html")
      }

      "map ordinary suffixes without parsing their contents (P751-02F-CATALOGUE-SDX-01)" in {
        Given("ordinary Dox, Org and both Markdown inputs in deliberately unordered insertion order")
        val realm = _realm(Vector("docs/z.markdown", "docs/b.org", "docs/a.dox", "docs/c.md"))
        When("the supplied Realm is structurally selected")
        val selected = DoxSitePublicDocuments.sources(realm)
        Then("actual physical suffixes survive and each canonical public suffix is HTML in deterministic order")
        _pairs(selected) shouldBe Vector(
          "docs/a.dox" -> "docs/a.html", "docs/b.org" -> "docs/b.html",
          "docs/c.md" -> "docs/c.html", "docs/z.markdown" -> "docs/z.html")
      }

      "close every private suffix under the outermost package (P751-02F-CATALOGUE-SDX-02)" in {
        Given("a package index, direct and deep private sources, nested valid project and bare _source")
        val privatepaths = for {
          directory <- Vector("", "arbitrary/deeper/")
          suffix <- Vector("dox", "org", "md", "markdown")
        } yield s"docs/package.dox/${directory}private.$suffix"
        val realm = _realm(Vector("docs/ordinary.dox", "docs/package.dox/index.dox",
          "docs/package.dox/arbitrary/nested.dox/index.dox", "docs/package.dox/review/bare.md") ++ privatepaths)
        realm.setContent("docs/package.dox/review/bare.md", "---\n_source\n---\n")
        When("existing Rule admission selects public source descriptors")
        val selected = DoxSitePublicDocuments.sources(realm)
        Then("only the ordinary source and direct package index are public, with no private parse")
        _pairs(selected) shouldBe Vector("docs/ordinary.dox" -> "docs/ordinary.html",
          "docs/package.dox/index.dox" -> "docs/package.html")
      }

      "retain ordinary descendants of a dox directory without a direct index (P751-02F-CATALOGUE-SDX-03)" in {
        Given("an unrecognized dox directory and an independent recognized package")
        val realm = _realm(Vector("docs/folder.dox/child.org", "docs/folder.dox/deep/child.md",
          "docs/package.dox/index.dox"))
        When("public sources are selected without inventing package recognition")
        val selected = DoxSitePublicDocuments.sources(realm)
        Then("ordinary descendants keep their directories while the real package flattens")
        _pairs(selected) shouldBe Vector("docs/folder.dox/child.org" -> "docs/folder.dox/child.html",
          "docs/folder.dox/deep/child.md" -> "docs/folder.dox/deep/child.html",
          "docs/package.dox/index.dox" -> "docs/package.html")
      }
    }

    "delegate availability and ignored ancestry" which {
      "exclude partials and resource paths with positive ordinary and package controls (P751-02F-CATALOGUE-SDX-04)" in {
        Given("include-only partials, excluded resource paths, ignored ancestry and public controls")
        val realm = _realm(Vector("docs/_partial.dox", "docs/_hidden/child.md", "docs/includes/child.org",
          "docs/assets/child.md", "docs/styles/child.markdown", "docs/work.d/child.dox",
          "docs/ordinary.md", "docs/package.dox/index.dox"))
        When("selection applies the existing default Rule")
        val selected = DoxSitePublicDocuments.sources(realm)
        Then("unavailable sources do not become descriptors")
        _pairs(selected) shouldBe Vector("docs/ordinary.md" -> "docs/ordinary.html",
          "docs/package.dox/index.dox" -> "docs/package.html")
      }

      "honor configured filename and path patterns and isIgnore independently (P751-02F-CATALOGUE-SDX-05)" in {
        Given("a permissive path configuration, filename/path exclusions and a .d ancestor")
        val realm = _realm(Vector("docs/keep.md", "docs/reject.md", "docs/blocked/keep.md",
          "docs/work.d/keep.md", "docs/package.dox/index.dox"))
        val config = DoxSite.Config.default.copy(
          includeFilePatterns = Vector("(keep\\.md|index\\.dox)".r),
          excludeFilePatterns = Vector("reject\\.md".r),
          includePathPatterns = Vector(".*".r), excludePathPatterns = Vector(".*/blocked/.*".r))
        When("the same structural facade receives the selected configuration")
        val selected = DoxSitePublicDocuments.sources(realm, config)
        Then(".d ancestry remains ignored even when the configured path patterns allow it")
        _pairs(selected) shouldBe Vector("docs/keep.md" -> "docs/keep.html",
          "docs/package.dox/index.dox" -> "docs/package.html")
      }

      "exclude reserved directories at every depth while retaining lookalikes (P751-02F-CATALOGUE-SDX-08)" in {
        Given("root and deep reserved directories alongside ordinary, package and reserved-name lookalike sources")
        val excluded = Vector("_hidden/child.md", "assets/child.dox", "styles/child.org",
          "includes/child.markdown", "work.d/child.dox")
        val positives = Vector("myassets/child.md", "includes-guide/child.dox", "stylesheets/child.org",
          "work.data/child.markdown", "with_under/child.md", "folder.dox/child.org",
          "docs/ordinary.md", "docs/package.dox/index.dox")
        val realm = _realm(excluded ++ excluded.map(path => s"docs/deep/$path") ++ positives)
        When("the actual structural facade applies its default configuration")
        val selected = DoxSitePublicDocuments.sources(realm)
        Then("the entire ordered catalogue contains exactly the eligible physical and public pairs")
        _pairs(selected) shouldBe Vector(
          "docs/ordinary.md" -> "docs/ordinary.html",
          "docs/package.dox/index.dox" -> "docs/package.html",
          "folder.dox/child.org" -> "folder.dox/child.html",
          "includes-guide/child.dox" -> "includes-guide/child.html",
          "myassets/child.md" -> "myassets/child.html",
          "stylesheets/child.org" -> "stylesheets/child.html",
          "with_under/child.md" -> "with_under/child.html",
          "work.data/child.markdown" -> "work.data/child.html")
      }

      "match complete reserved paths across directory and separator boundaries (P751-02F-CATALOGUE-SDX-09)" in {
        Given("explicit slash and backslash paths with root, deep, exact underscore and terminal .d segments")
        val excluded = Vector(
          "_hidden/child.md", "_/child.md", "assets/child.dox", "styles/child.org",
          "includes/child.markdown", "work.d/child.dox", "work.d", ".d",
          "docs/deep/_hidden/child.md", "docs/deep/_/child.md", "docs/deep/assets/child.dox",
          "docs/deep/styles/child.org", "docs/deep/includes/child.markdown",
          "docs/deep/work.d/child.dox", "docs/deep/work.d", "docs/deep/.d",
          "_hidden\\child.md", "_\\child.md", "assets\\child.dox", "styles\\child.org",
          "includes\\child.markdown", "work.d\\child.dox",
          "docs\\deep\\_hidden\\child.md", "docs\\deep\\_\\child.md", "docs\\deep\\assets\\child.dox",
          "docs\\deep\\styles\\child.org", "docs\\deep\\includes\\child.markdown",
          "docs\\deep\\work.d\\child.dox", "docs\\deep\\work.d", "docs\\deep\\.d")
        val positives = Vector(
          "myassets/child.md", "includes-guide/child.dox", "stylesheets/child.org",
          "work.data/child.markdown", "with_under/child.md", "folder.dox/child.org",
          "docs/deep/myassets/child.md", "docs/deep/includes-guide/child.dox",
          "docs/deep/stylesheets/child.org", "docs/deep/work.data/child.markdown",
          "docs/deep/with_under/child.md", "docs/deep/folder.dox/child.org",
          "myassets\\child.md", "includes-guide\\child.dox", "stylesheets\\child.org",
          "work.data\\child.markdown", "with_under\\child.md", "folder.dox\\child.org",
          "docs\\deep\\myassets\\child.md", "docs\\deep\\includes-guide\\child.dox",
          "docs\\deep\\stylesheets\\child.org", "docs\\deep\\work.data\\child.markdown",
          "docs\\deep\\with_under\\child.md", "docs\\deep\\folder.dox\\child.org")
        val patterns = DoxSite.Config.default.excludePathPatterns
        When("the actual default regex matchers consume each complete path")
        val excludedmatches = excluded.map(path => patterns.exists(_.pattern.matcher(path).matches()))
        val positivematches = positives.map(path => patterns.exists(_.pattern.matcher(path).matches()))
        Then("every reserved path matches and every segment-boundary lookalike does not")
        excludedmatches shouldBe Vector.fill(excluded.size)(true)
        positivematches shouldBe Vector.fill(positives.size)(false)
      }

      "share default exclusion with actual parsed-site admission (P751-02F-CATALOGUE-SDX-10)" in {
        Given("fresh native Dox sources in reserved directories and ordinary, package and lookalike controls")
        val excluded = Vector("_hidden/child.dox", "assets/child.dox", "styles/child.dox",
          "includes/child.dox", "docs/deep/_hidden/child.dox", "docs/deep/assets/child.dox",
          "docs/deep/styles/child.dox", "docs/deep/includes/child.dox")
        val positives = Vector("docs/ordinary.dox", "docs/package.dox/index.dox", "docs/myassets/child.dox")
        val builder = Realm.Builder()
        val source = """|Availability control
                         |====================
                         |
                         |# HEAD
                         |
                         |status = "published"
                         |published_at = "2026-10-07"
                         |
                         |# Body
                         |
                         |Retained parsed availability body.
                         |""".stripMargin
        (excluded ++ positives).foreach(path => builder.set(path, source))
        val realm = builder.build()
        val context = Context.create()
        val config = DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full)
        When("DoxSite creates its actual parsed tree from the supplied Realm with Full strategy")
        val site = DoxSite.create(context, realm, None, config)
        val retained = positives.map(path => DoxSiteEffectiveContent.effectiveContent(site.space, path))
        Then("excluded documents are absent and all positive content and effective identities are retained")
        excluded.foreach(path => site.space.getContent(path) shouldBe empty)
        positives.foreach(path => site.space.getContent(path) shouldBe defined)
        retained.foreach(content => content shouldBe defined)
        retained.map(_.map(content => (content.sourcePath.v, content.logicalPath.v,
          DoxSite.publicPath(content.logicalPath.v)))) shouldBe Vector(
          Some(("docs/ordinary.dox", "docs/ordinary.dox", "docs/ordinary.html")),
          Some(("docs/package.dox/index.dox", "docs/package.dox", "docs/package.html")),
          Some(("docs/myassets/child.dox", "docs/myassets/child.dox", "docs/myassets/child.html")))
      }
    }

    "select supplied native trees" which {
      "retain the complete root matrix independently in physical and memory trees (P751-02F-CATALOGUE-SDX-12)" in {
        Given("identical deliberately unparsed root and nested sources in unordered physical and in-memory trees")
        _with_fixture { root =>
          val paths = Vector("z.markdown", "b.org", "index.dox", "a.dox", "c.md",
            "docs/ordinary.org", "package.dox/index.dox", "package.dox/private.md",
            "_partial.dox", "assets/hidden.md", "日本語 note.md")
          val expected = Vector(
            "a.dox" -> "a.html",
            "b.org" -> "b.html",
            "c.md" -> "c.html",
            "docs/ordinary.org" -> "docs/ordinary.html",
            "index.dox" -> "index.html",
            "package.dox/index.dox" -> "package.html",
            "z.markdown" -> "z.html",
            "日本語 note.md" -> "日本語 note.html")
          paths.foreach(path => _write(root.resolve(path), "Unparsed structural input."))
          val physical = Realm.create(DoxSite.realmConfig, root.toFile)
          val memory = _realm(paths)
          When("the actual structural facade selects both supplied native trees")
          val physicalpairs = _pairs(DoxSitePublicDocuments.sources(physical))
          val memorypairs = _pairs(DoxSitePublicDocuments.sources(memory))
          Then("each entire vector independently matches the exact physical and canonical public pairs")
          physicalpairs shouldBe expected
          memorypairs shouldBe expected
        }
      }

      "match physical roots containing spaces and Japanese with in-memory input (P751-02F-CATALOGUE-SDX-06)" in {
        Given("identical physical and in-memory sources, including unreadable-as-Dox private content")
        _with_fixture { root =>
          val paths = Vector("docs/ordinary.org", "docs/package.dox/index.dox", "docs/package.dox/private.md")
          paths.foreach(path => _write(root.resolve(path), "---\n_source\n---\n"))
          val physical = Realm.create(DoxSite.realmConfig, root.toFile)
          val memory = _realm(paths)
          When("both native content trees are selected")
          val physicalpairs = _pairs(DoxSitePublicDocuments.sources(physical))
          val memorypairs = _pairs(DoxSitePublicDocuments.sources(memory))
          Then("origin spelling has no effect on the exact structural contract")
          physicalpairs shouldBe Vector("docs/ordinary.org" -> "docs/ordinary.html",
            "docs/package.dox/index.dox" -> "docs/package.html")
          memorypairs shouldBe physicalpairs
        }
      }

      "return an empty deterministic vector for an empty Realm (P751-02F-CATALOGUE-SDX-07)" in {
        Given("an empty native Realm")
        val realm = Realm.create()
        When("public sources are requested")
        val selected = DoxSitePublicDocuments.sources(realm)
        Then("no descriptors are invented")
        selected shouldBe Vector.empty
      }
    }
  }

  private def _pairs(selected: Vector[DoxSitePublicDocuments.Source]): Vector[(String, String)] =
    selected.map(source => source.sourcePath -> source.publicPath)

  private def _realm(paths: Vector[String]): Realm = {
    val builder = Realm.Builder()
    paths.foreach(path => builder.set(path, "Unparsed structural input."))
    builder.build()
  }

  private def _write(path: Path, content: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, content.getBytes(StandardCharsets.UTF_8))
    ()
  }

  private def _with_fixture[A](f: Path => A): A = {
    val workroot = Paths.get(sys.props("user.dir")).resolve("target/doxsite-public-documents-spec")
    Files.createDirectories(workroot)
    val root = Files.createTempDirectory(workroot, "physical 日本語 -")
    try f(root)
    finally {
      val stream = Files.walk(root)
      try stream.iterator.asScala.toVector.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists(_))
      finally stream.close()
    }
  }
}
