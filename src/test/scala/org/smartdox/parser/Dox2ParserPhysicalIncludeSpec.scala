package org.smartdox.parser

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.goldenport.io.{FileTextResolver, ResourceLocator}
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.SmartDoxSpecVocabulary

/*
 * @since   Oct.  6, 2026
 * @version Oct.  6, 2026
 */
class Dox2ParserPhysicalIncludeSpec
    extends AnyWordSpec
    with GivenWhenThen
    with SmartDoxSpecVocabulary {
  "Dox2Parser include origins" should {
    "resolve a native physical sibling under spaces and Japanese characters" in {
      Given("an absolute source and sibling under a fixture root containing spaces and Japanese characters")
      _with_fixture("physical 日本語 root -") { root =>
        val source = root.resolve("source.dox")
        val cwd = Paths.get("").toAbsolutePath
        _write(source, _article_source("include::_fragment.dox[]"))
        _write(root.resolve("_fragment.dox"), "Native Japanese sibling sentinel.\n")
        val config = Dox2Parser.Config.default.copy(file = Some(source.toFile.toURI))

        When("the parser interprets the authored include with the native physical source URI")
        val dox = Dox2Parser.parse(config, _read(source))

        Then("the sibling is included while the cwd and default resolver fallbacks and contexts are retained")
        dox.toString should include_text("Native Japanese sibling sentinel.")
        Paths.get("").toAbsolutePath shouldBe cwd
        val context = config.fileTextResolverContext
        val defaultcontext = FileTextResolver.Context.default
        context.fileResolverContext.resources.tail shouldBe defaultcontext.fileResolverContext.resources
        context.fileResolverContext.parameters shouldBe defaultcontext.fileResolverContext.parameters
        context.textResolverContext shouldBe defaultcontext.textResolverContext
      }
    }

    "resolve a native physical sibling under literal percent plus and hash characters" in {
      Given("a real absolute fixture root named with physical %25+hash# 日本語 and spaces")
      _with_fixture("physical %25+hash# 日本語 root -") { root =>
        val source = root.resolve("source.dox")
        val cwd = Paths.get("").toAbsolutePath
        _write(source, _article_source("include::_fragment.dox[]"))
        _write(root.resolve("_fragment.dox"), "Literal percent plus hash sibling sentinel.\n")
        val config = Dox2Parser.Config.default.copy(file = Some(source.toFile.toURI))

        When("the parser interprets the authored include with that native physical source URI")
        val dox = Dox2Parser.parse(config, _read(source))

        Then("the exact sibling sentinel is included without double decoding or plus conversion and the cwd is retained")
        dox.toString should include_text("Literal percent plus hash sibling sentinel.")
        Paths.get("").toAbsolutePath shouldBe cwd
      }
    }

    "retain sibling inclusion for a cwd-relative ASCII source" in {
      Given("an ASCII source and sibling with source identity relative to the unchanged process cwd")
      _with_fixture("relative-ascii-") { root =>
        val source = root.resolve("source.dox")
        val cwd = Paths.get("").toAbsolutePath
        val relativepath = Paths.get("").toAbsolutePath.relativize(source).toString
        _write(source, _article_source("include::_fragment.dox[]"))
        _write(root.resolve("_fragment.dox"), "Relative ASCII sibling sentinel.\n")
        val config = Dox2Parser.Config.default.withPathname(relativepath)

        When("the parser interprets the authored include with its existing relative source configuration")
        val dox = Dox2Parser.parse(config, _read(source))

        Then("the existing sibling sentinel is included and the process cwd is retained")
        dox.toString should include_text("Relative ASCII sibling sentinel.")
        Paths.get("").toAbsolutePath shouldBe cwd
      }
    }

    "retain the existing HTTPS candidate projection without fetching" in {
      Given("a source URI at https://example.invalid/docs/source.dox")
      val config = Dox2Parser.Config.default.copy(file = Some(new URI("https://example.invalid/docs/source.dox")))

      When("the file resolver projects candidates for the relative _fragment.dox include")
      val candidates = config.fileTextResolverContext.fileResolverContext.makeCandidates("_fragment.dox")

      Then("the first candidate retains the existing host-root HTTPS URL without fetching its content")
      ResourceLocator.get(candidates.head).flatMap(_.getUrl).map(_.toExternalForm) shouldBe
        Some("https://example.invalid/_fragment.dox")
    }
  }

  private def _with_fixture(prefix: String)(body: Path => Unit): Unit = {
    val target = Files.createDirectories(Paths.get("target", "dox2-physical-include").toAbsolutePath)
    val root = Files.createTempDirectory(target, prefix)
    try body(root) finally _delete_fixture(root)
  }

  private def _write(path: Path, content: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, content.getBytes(StandardCharsets.UTF_8))
  }

  private def _read(path: Path): String =
    new String(Files.readAllBytes(path), StandardCharsets.UTF_8)

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
