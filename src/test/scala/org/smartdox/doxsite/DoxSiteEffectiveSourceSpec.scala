package org.smartdox.doxsite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.generator.Context
import org.smartdox.parser.Dox2Parser

/*
 * @since   Oct.  1, 2026
 * @version Oct.  1, 2026
 */
class DoxSiteEffectiveSourceSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "DoxSite effective source" should {
    "physical relative includes" which {
      "resolve an ordinary page from its physical directory like the direct parser" in {
        Given("an ordinary page and a Document Project with distinct relative include content")
        val dir = _fixture()
        try {
          val context = Context.create()
          val sourcefile = dir.resolve("guide/ordinary.dox").toFile.getCanonicalFile
          val source = new String(Files.readAllBytes(sourcefile.toPath), StandardCharsets.UTF_8)
          val parserconfig = Dox2Parser.Config.default.copy(file = Some(sourcefile.toURI))

          When("the direct parser and DoxSite interpret the same ordinary source")
          val direct = Dox2Parser.parse(parserconfig, source)
          val site = DoxSite.create(context, dir.toFile, None,
            DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
          val effective = site.space.getNode("guide/ordinary.dox").
            flatMap(DoxSiteEffectiveContent.effectiveContent).get
          val realm = site.toRealm(context)
          implicit val i18ncontext = context.i18NContext
          val html = realm.getString("en/guide/ordinary.html").get

          Then("both interpretations retain the ordinary directory's included content")
          direct.toPlainText should include ("Ordinary physical include sentinel.")
          effective.getDox.toPlainText shouldBe direct.toPlainText
          effective.getDox.toPlainText should not include ("Project physical include sentinel.")
          And("the ordinary page retains its public route and renders the included text")
          effective.logicalPath.v shouldBe "guide/ordinary.dox"
          effective.sourcePath.v shouldBe "guide/ordinary.dox"
          html should include ("Ordinary physical include sentinel.")
        } finally {
          _delete(dir)
        }
      }

      "resolve a Document Project index from its physical package directory" in {
        Given("a Document Project index whose relative include differs from the guide directory's include")
        val dir = _fixture()
        try {
          val context = Context.create()

          When("DoxSite interprets the canonical package index and produces public pages")
          val site = DoxSite.create(context, dir.toFile, None,
            DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
          val effective = site.space.getNode("guide/project.dox").
            flatMap(DoxSiteEffectiveContent.effectiveContent).get
          val realm = site.toRealm(context)
          implicit val i18ncontext = context.i18NContext
          val html = realm.getString("en/guide/project.html").get

          Then("effective content retains the include resolved beside the physical index")
          effective.getDox.toPlainText should include ("Project physical include sentinel.")
          effective.getDox.toPlainText should not include ("Ordinary physical include sentinel.")
          And("the package exposes its logical document and public HTML route")
          effective.logicalPath.v shouldBe "guide/project.dox"
          effective.sourcePath.v shouldBe "guide/project.dox/index.dox"
          html should include ("Project physical include sentinel.")
          realm.getString("en/guide/project.dox/index.html") shouldBe None
          realm.getString("ja/guide/project.dox/index.html") shouldBe None
        } finally {
          _delete(dir)
        }
      }
    }

    "Document Project public input selection" which {
      "keep subordinate and review sources outside the public page candidates" in {
        Given("a canonical package index alongside subordinate Dox and private Dox and Markdown review sources")
        val dir = _fixture()
        try {
          val context = Context.create()

          When("DoxSite selects package pages and produces the public realm")
          val site = DoxSite.create(context, dir.toFile, None,
            DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full))
          val realm = site.toRealm(context)
          implicit val i18ncontext = context.i18NContext

          Then("only the canonical index contributes a public package document")
          site.space.getContent("guide/project.dox/index.dox") should not be empty
          site.space.getContent("guide/project.dox/parts/content.dox") shouldBe empty
          site.space.getContent("guide/project.dox/review/private.dox") shouldBe empty
          site.space.getContent("guide/project.dox/review/private.md") shouldBe empty
          And("subordinate and review sources receive no public HTML route in either locale")
          for {
            locale <- List("en", "ja")
            source <- List("parts/content", "review/private")
          } realm.getString(s"$locale/guide/project.dox/$source.html") shouldBe None
        } finally {
          _delete(dir)
        }
      }
    }
  }

  private def _fixture(): Path = {
    val parent = Files.createDirectories(Paths.get("target/phase75"))
    val dir = Files.createTempDirectory(parent, "effective-source-")
    _write(dir.resolve("guide/ordinary.dox"), "# Ordinary\n\ninclude::parts/content.dox[]\n")
    _write(dir.resolve("guide/parts/content.dox"), "Ordinary physical include sentinel.\n")
    _write(dir.resolve("guide/project.dox/index.dox"), "# Project\n\ninclude::parts/content.dox[]\n")
    _write(dir.resolve("guide/project.dox/parts/content.dox"), "Project physical include sentinel.\n")
    _write(dir.resolve("guide/project.dox/review/private.dox"), "Private Dox review sentinel.\n")
    _write(dir.resolve("guide/project.dox/review/private.md"), "# Private Markdown review\n\nPrivate Markdown review sentinel.\n")
    dir
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
