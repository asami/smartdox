package org.smartdox.doxsite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.i18n.I18NContext
import org.goldenport.realm.Realm
import org.smartdox.SmartDoxSpecVocabulary
import org.smartdox.generator.Context

/*
 * @since   Oct.  6, 2026
 * @version Oct.  6, 2026
 */
class DoxSitePhysicalIncludeSpec
    extends AnyWordSpec
    with GivenWhenThen
    with SmartDoxSpecVocabulary {
  "DoxSite include origins" should {
    "retain an absolute Document Project index as the sibling include base and expose only the canonical article route" in {
      Given("an absolute physical site with category/article.dox/index.dox including its sibling _fragment.dox")
      _with_fixture("document-project-") { root =>
        _write(root.resolve("category/article.dox/index.dox"), _article_source("include::_fragment.dox[]"))
        _write(root.resolve("category/article.dox/_fragment.dox"), "Document Project included sentinel.\n")
        val context = Context.create()
        implicit val i18ncontext: I18NContext = context.i18NContext

        When("a fresh Full-strategy DoxSite parses the physical site and renders its effective article")
        val site = create_site(context, root.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
        val realm = site_realm(site, context)
        val effective = site.space.getNode("category/article.dox").flatMap(DoxSiteEffectiveContent.effectiveContent)

        Then("the parsed effective article and both public locales contain the included sentinel at the canonical route")
        site.config.origin.map(_.getCanonicalFile) shouldBe Some(root.toFile.getCanonicalFile)
        effective.map(_.logicalPath.v) shouldBe Some("category/article.dox")
        effective.map(_.sourcePath.v) shouldBe Some("category/article.dox/index.dox")
        effective.get.getDox.toString should include_text("Document Project included sentinel.")
        html_at(realm, "ja/category/article.html") should include_html("Document Project included sentinel.")
        html_at(realm, "en/category/article.html") should include_html("Document Project included sentinel.")
        realm.getString("ja/category/article.dox/index.html") shouldBe None
        realm.getString("en/category/article.dox/index.html") shouldBe None
      }
    }

    "retain ordinary physical document sibling include compatibility" in {
      Given("an ordinary category/ordinary.dox using the same relative sibling include contract")
      _with_fixture("ordinary-document-") { root =>
        _write(root.resolve("category/ordinary.dox"), _article_source("include::_fragment.dox[]"))
        _write(root.resolve("category/_fragment.dox"), "Ordinary physical included sentinel.\n")
        val context = Context.create()
        implicit val i18ncontext: I18NContext = context.i18NContext

        When("a fresh Full-strategy DoxSite parses and renders the ordinary physical document")
        val site = create_site(context, root.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
        val realm = site_realm(site, context)
        val effective = site.space.getNode("category/ordinary.dox").flatMap(DoxSiteEffectiveContent.effectiveContent)

        Then("the ordinary source keeps its identity and includes its sibling in parsed content and HTML")
        effective.map(_.logicalPath.v) shouldBe Some("category/ordinary.dox")
        effective.map(_.sourcePath.v) shouldBe Some("category/ordinary.dox")
        effective.get.getDox.toString should include_text("Ordinary physical included sentinel.")
        html_at(realm, "ja/category/ordinary.html") should include_html("Ordinary physical included sentinel.")
        html_at(realm, "en/category/ordinary.html") should include_html("Ordinary physical included sentinel.")
      }
    }

    "resolve a Document Project include under a physical root containing spaces and Japanese characters" in {
      Given("an absolute fixture root containing spaces and Japanese characters, with a sibling fragment outside the process cwd")
      _with_fixture("physical 日本語 root -") { root =>
        _write(root.resolve("category/article.dox/index.dox"), _article_source("include::_fragment.dox[]"))
        _write(root.resolve("category/article.dox/_fragment.dox"), "Encoded physical root included sentinel.\n")
        val context = Context.create()
        implicit val i18ncontext: I18NContext = context.i18NContext

        When("a fresh Full-strategy DoxSite renders the physical source without changing the process working directory")
        val site = create_site(context, root.toFile, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
        val realm = site_realm(site, context)
        val effective = site.space.getNode("category/article.dox").flatMap(DoxSiteEffectiveContent.effectiveContent)

        Then("the absolute encoded physical base resolves the fragment and keeps the canonical article identity and output")
        site.config.origin.map(_.getCanonicalFile) shouldBe Some(root.toFile.getCanonicalFile)
        effective.map(_.logicalPath.v) shouldBe Some("category/article.dox")
        effective.map(_.sourcePath.v) shouldBe Some("category/article.dox/index.dox")
        effective.get.getDox.toString should include_text("Encoded physical root included sentinel.")
        html_at(realm, "ja/category/article.html") should include_html("Encoded physical root included sentinel.")
        html_at(realm, "en/category/article.html") should include_html("Encoded physical root included sentinel.")
        realm.getString("ja/category/article.dox/index.html") shouldBe None
        realm.getString("en/category/article.dox/index.html") shouldBe None
      }
    }

    "preserve origin-less virtual document identity and ordinary rendering" in {
      Given("an origin-less Realm containing an ordinary virtual document without includes")
      val input = Realm.create()
      input.backend.setContent("category/ordinary.dox", Realm.StringData(_article_source("Virtual ordinary sentinel.")))
      val context = Context.create()
      implicit val i18ncontext: I18NContext = context.i18NContext

      When("a fresh Full-strategy DoxSite parses and renders the virtual Realm")
      val site = DoxSite.create(context, input, None, DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
      val realm = site_realm(site, context)
      val effective = site.space.getNode("category/ordinary.dox").flatMap(DoxSiteEffectiveContent.effectiveContent)

      Then("the virtual origin stays absent and its relative source and public HTML retain their ordinary meaning")
      site.config.origin shouldBe None
      effective.map(_.logicalPath.v) shouldBe Some("category/ordinary.dox")
      effective.map(_.sourcePath.v) shouldBe Some("category/ordinary.dox")
      effective.get.getDox.toString should include_text("Virtual ordinary sentinel.")
      html_at(realm, "ja/category/ordinary.html") should include_html("Virtual ordinary sentinel.")
      html_at(realm, "en/category/ordinary.html") should include_html("Virtual ordinary sentinel.")
    }
  }

  private def _with_fixture(prefix: String)(body: Path => Unit): Unit = {
    val target = Files.createDirectories(Paths.get("target", "doxsite-physical-include").toAbsolutePath)
    val root = Files.createTempDirectory(target, prefix)
    try body(root) finally _delete_fixture(root)
  }

  private def _write(path: Path, content: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, content.getBytes(StandardCharsets.UTF_8))
  }

  private def _article_source(body: String): String =
    s"""Include Compatibility
       |=====================
       |
       |# HEAD
       |
       |title = "Include Compatibility"
       |status = "published"
       |published_at = "2026-10-06"
       |
       |# Body
       |
       |$body
       |""".stripMargin

  private def _delete_fixture(root: Path): Unit = {
    val stream = Files.walk(root)
    try {
      import scala.collection.JavaConverters._
      stream.iterator.asScala.toVector.reverse.foreach(Files.deleteIfExists)
    } finally stream.close()
  }
}
