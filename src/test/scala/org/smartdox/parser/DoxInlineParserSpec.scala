package org.smartdox.parser

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.file.Files
import org.goldenport.parser.ParseLocation
import scalaz._, Scalaz._
import org.scalatest.GivenWhenThen
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._

/*
 * @since   Nov. 29, 2020
 *  version Aug. 16, 2025
 *  version Apr. 20, 2026
 * @version Sep.  7, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxInlineParserSpec extends AnyWordSpec with Matchers with ScalazMatchers with GivenWhenThen with UseDoxParser {
  "plain" should {
    "retain six-field parser configuration products while carrying a resource root through location and style selection" in {
      Given("the original inline and document parser configuration contracts and an article root")
      val root = Files.createTempDirectory("smartdox-markdown-image-config")
      try {
        When("a resource-rooted inline configuration is relocated and a Dox2 configuration selects Markdown by filename")
        val inline = DoxInlineParser.Config.smartdox.
          withResourceRoot(root).
          withLocation(ParseLocation.create(12))
        val inlineimage = DoxInlineParser.parse(inline, "![inline](images/diagram.png)").asInstanceOf[ReferenceImg]
        val document = Dox2Parser.parseWithFilename(
          Dox2Parser.Config.default.withResourceRoot(root),
          "article.md",
          "![document](images/diagram.png)"
        )
        val documentimage = document.find { case _: ReferenceImg => true; case _ => false }.
          collect { case m: ReferenceImg => m }.get
        val virtualconfig = DoxInlineParser.Config.smartdox.
          _with_virtual_resource_parent("articles")
        val virtualcopy = virtualconfig.copy(isDebug = true)
        val virtualimage = DoxInlineParser.parse(
          virtualcopy,
          "![virtual](images/diagram.png)"
        ).asInstanceOf[ReferenceImg]
        val virtualdocument = Dox2Parser.parseWithFilename(
          Dox2Parser.Config.default._with_virtual_resource_parent("articles"),
          "article.md",
          "![virtual-document](images/diagram.png)"
        )
        val virtualdocumentimage = virtualdocument.find { case _: ReferenceImg => true; case _ => false }.
          collect { case m: ReferenceImg => m }.get

        Then("both public case classes retain their six-argument constructor and product arity")
        classOf[DoxInlineParser.Config].getConstructors.map(_.getParameterTypes.length) should contain (6)
        DoxInlineParser.Config.default.productArity shouldBe 6
        classOf[Dox2Parser.Config].getConstructors.map(_.getParameterTypes.length) should contain (6)
        Dox2Parser.Config.default.productArity shouldBe 6
        And("the root survives inline relocation and Dox2 filename-driven Markdown selection")
        inlineimage.src.toString shouldBe "images/diagram.png"
        inlineimage.location should not be empty
        documentimage.src.toString shouldBe "images/diagram.png"
        And("the internal virtual origin survives copy and Dox2 filename-driven style selection")
        virtualimage.src.toString shouldBe "images/diagram.png"
        virtualdocumentimage.src.toString shouldBe "images/diagram.png"
      } finally {
        org.goldenport.io.IoUtils.removeDirectory(root.toFile)
      }
    }

    "retain the nested SkipOneState compatibility identity" in {
      Given("a generic inline parent and the public SkipOneState companion")
      val parent = DoxInlineParser.NormalState.init(DoxInlineParser.Config.default)

      When("the companion is called with its compatibility named argument")
      val state = DoxInlineParser.SkipOneState(parent = parent, skipChar = ']')

      Then("the nested binary name and skip-character accessor remain callable")
      classOf[DoxInlineParser.SkipOneState].getName shouldBe "org.smartdox.parser.DoxInlineParser$SkipOneState"
      state.skipChar shouldBe ']'
    }

    "retain a resource root through ordinary public Config copy" in {
      Given("a SmartDox configuration rooted at an article directory")
      val root = Files.createTempDirectory("smartdox-markdown-image-copy")
      val config = DoxInlineParser.Config.smartdox.withResourceRoot(root)
      try {
        When("a normal public copy changes an existing six-field configuration value")
        val copied = config.copy(isDebug = true)
        val image = DoxInlineParser.parse(copied, "![diagram](images/diagram.png)").asInstanceOf[ReferenceImg]

        Then("the retained root admits the relative image as a ReferenceImg rather than an unsupported resource")
        image.src.toString shouldBe "images/diagram.png"
        image.alt shouldBe Some("diagram")
      } finally {
        org.goldenport.io.IoUtils.removeDirectory(root.toFile)
      }
    }

    "admit virtual Markdown images within the lexical source parent" in {
      Given("a Markdown-enabled parser with an internal virtual source parent")
      val config = DoxInlineParser.Config.smartdox.
        _with_virtual_resource_parent("articles/guides")

      When("the parser reads an image path with lexical dot segments inside that parent")
      val image = DoxInlineParser.parse(
        config,
        "![図](images/../images/diagram.png)"
      ).asInstanceOf[ReferenceImg]

      Then("the image is retained as a normalized virtual-root-relative reference")
      image.src.toString shouldBe "images/diagram.png"
      image.alt shouldBe Some("図")
    }

    "reject virtual Markdown image traversal outside the source parent" in {
      Given("a Markdown-enabled parser with an internal virtual source parent")
      val config = DoxInlineParser.Config.smartdox.
        _with_virtual_resource_parent("articles/guides")

      When("the parser reads an image path that traverses above that parent")
      val failure = intercept[IllegalArgumentException] {
        DoxInlineParser.parse(config, "![外](../../outside.png)")
      }

      Then("the stable unsupported-resource diagnostic identifies the authored path")
      failure.getMessage should include("image.markdown.unsupported-resource")
      failure.getMessage should include("source=![外](../../outside.png)")
      failure.getMessage should include("raw-path=../../outside.png")
    }

    "reject rootless Markdown images without consulting the current directory" in {
      Given("a Markdown-enabled parser with no source origin")
      val config = DoxInlineParser.Config.smartdox

      When("the parser reads a local Markdown image")
      val failure = intercept[IllegalArgumentException] {
        DoxInlineParser.parse(config, "![diagram](diagram.png)")
      }

      Then("the stable unsupported-resource diagnostic is returned without a derived root")
      failure.getMessage shouldBe
        "image.markdown.unsupported-resource: location=<absent> source=![diagram](diagram.png) raw-path=diagram.png"
    }

    "admit exact Markdown images with source-exact alternative text" in {
      Given("a Markdown-enabled parser rooted at an article directory")
      val root = Files.createTempDirectory("smartdox-markdown-image-inline")
      val config = DoxInlineParser.Config.smartdox.withResourceRoot(root)
      try {
        val source = "before ![日本語の代替テキスト](images/../images/team's-diagram.png) after"

        When("the parser reads a local Markdown image whose path normalizes under that root")
        val parsed = DoxInlineParser.parse(config, source)
        val image = _nodes(parsed).collectFirst { case m: ReferenceImg => m }.get
        val text = _nodes(parsed).collect { case m: Text => m.contents }.mkString
        val decoded = DoxInlineParser.parse(config, "![decoded](images%2Fdiagram.png)").asInstanceOf[ReferenceImg]

        Then("one shared image model preserves the authored Japanese alternative text and normalized URI")
        image.src.toString shouldBe "images/team's-diagram.png"
        image.alt shouldBe Some("日本語の代替テキスト")
        image.attributes shouldBe empty
        And("URI decoding occurs before the normalized root-relative model URI is retained")
        decoded.src.toString shouldBe "images/diagram.png"
        And("the Markdown image opener is syntax rather than emitted text")
        text should not include "!"
      } finally {
        org.goldenport.io.IoUtils.removeDirectory(root.toFile)
      }
    }

    "retain empty Markdown image alternative text" in {
      Given("a Markdown-enabled parser rooted at an article directory")
      val root = Files.createTempDirectory("smartdox-markdown-image-empty-alt")
      val config = DoxInlineParser.Config.smartdox.withResourceRoot(root)
      try {
        When("the parser reads an exact image candidate with an empty alternative text")
        val image = DoxInlineParser.parse(config, "![](empty.png)").asInstanceOf[ReferenceImg]

        Then("the shared image model retains the observable empty alternative text")
        image.alt shouldBe Some("")
      } finally {
        org.goldenport.io.IoUtils.removeDirectory(root.toFile)
      }
    }

    "reject malformed and unsupported Markdown image candidates with stable context" in {
      Given("a Markdown-enabled parser rooted at an article directory")
      val root = Files.createTempDirectory("smartdox-markdown-image-diagnostics")
      val config = DoxInlineParser.Config.smartdox.
        withResourceRoot(root).
        withLocation(ParseLocation.create(13))
      try {
        When("malformed grammar and every rejected local-resource category are parsed")
        val closing = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, "![diagram](diagram.png")
        }
        val title = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, """![diagram](diagram.png "title")""")
        }
        val reference = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, "![diagram][reference]")
        }
        val unclosedreference = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, "![diagram][reference")
        }
        val remote = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, "![diagram](https://example.invalid/diagram.png)")
        }
        val absolute = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, "![diagram](file:///tmp/diagram.png)")
        }
        val empty = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, "![diagram]()")
        }
        val nonimage = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, "![diagram](diagram.txt)")
        }
        val escaped = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, "![diagram](../outside.png)")
        }
        val decodedinvalid = intercept[IllegalArgumentException] {
          DoxInlineParser.parse(config, "![diagram](diagram%00.png)")
        }

        Then("the parser reports the stable malformed and unsupported-resource diagnostics")
        closing.getMessage should include ("image.markdown.malformed")
        closing.getMessage should include ("source=![diagram](diagram.png")
        title.getMessage should include ("image.markdown.malformed")
        title.getMessage should include ("source=![diagram](diagram.png \"title\")")
        reference.getMessage should include ("source=![diagram][reference]")
        reference.getMessage should include ("raw-path=<absent>")
        unclosedreference.getMessage should include ("source=![diagram][reference")
        List(remote, absolute, empty, nonimage, escaped, decodedinvalid).foreach { failure =>
          failure.getMessage should include ("image.markdown.unsupported-resource")
          failure.getMessage should include ("location=")
          failure.getMessage should not include ("location=<absent>")
        }
        escaped.getMessage should include ("raw-path=../outside.png")
        decodedinvalid.getMessage should include ("raw-path=diagram%00.png")
      } finally {
        org.goldenport.io.IoUtils.removeDirectory(root.toFile)
      }
    }

    "stop malformed Markdown image reference diagnostics at the closing bracket" in {
      Given("a Markdown-enabled parser reading a reference-style image followed by trailing text")
      val source = "![diagram][reference] trailing"

      When("the parser reads the unsupported reference-style image candidate")
      val failure = intercept[IllegalArgumentException] {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, source)
      }

      Then("the malformed diagnostic captures exactly the image candidate and excludes trailing text")
      failure.getMessage shouldBe
        "image.markdown.malformed: location=<absent> source=![diagram][reference] raw-path=<absent>"
    }

    "report a malformed Markdown image when the path delimiter reaches EOF" in {
      Given("a Markdown-enabled parser reading an image whose opening path delimiter is terminal")
      val source = "![diagram]("

      When("the parser reads the terminal path delimiter")
      val failure = intercept[IllegalArgumentException] {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, source)
      }

      Then("the malformed diagnostic captures the exact available source without an identifiable raw path")
      failure.getMessage shouldBe
        "image.markdown.malformed: location=<absent> source=![diagram]( raw-path=<absent>"
    }

    "report a malformed Markdown image when the reference delimiter reaches EOF" in {
      Given("a Markdown-enabled parser reading an image whose reference delimiter is terminal")
      val source = "![diagram]["

      When("the parser reads the terminal reference delimiter")
      val failure = intercept[IllegalArgumentException] {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, source)
      }

      Then("the malformed diagnostic captures the exact available source without an identifiable raw path")
      failure.getMessage shouldBe
        "image.markdown.malformed: location=<absent> source=![diagram][ raw-path=<absent>"
    }

    "retain bracket-link and default parser compatibility around Markdown image syntax" in {
      Given("the established SmartDox and default parser configurations")
      val root = Files.createTempDirectory("smartdox-markdown-image-compatibility")
      val smartdox = DoxInlineParser.Config.smartdox.withResourceRoot(root)
      try {
        When("ordinary links and established bracket images are parsed beside the new image form")
        val link = DoxInlineParser.parse(smartdox, "[overview](overview.dox)")
        val bracket = DoxInlineParser.parse(smartdox, "[[diagram.png]]")
        val default = DoxInlineParser.parse(DoxInlineParser.Config.default, "![diagram](diagram.png)")

        Then("ordinary Markdown links and SmartDox bracket images retain their existing projections")
        link shouldBe a [Hyperlink]
        bracket shouldBe a [ReferenceImg]
        bracket.asInstanceOf[ReferenceImg].alt shouldBe None
        And("the non-Markdown default configuration retains literal exclamation text")
        _nodes(default).collect { case m: Text => m.contents }.mkString should include ("!")
      } finally {
        org.goldenport.io.IoUtils.removeDirectory(root.toFile)
      }
    }

    "retain ordinary Japanese text as text" in {
      Given("ordinary Japanese text without SmartDox inline syntax")

      When("the inline parser reads the text")
      val result = DoxInlineParser.parse("特性一覧")

      Then("the result retains the authored text")
      result shouldBe Text("特性一覧")
    }

    "parse markdown link with bilingual label" in {
      Given("a Markdown link with an English and Japanese label")
      val source = "[Literate Model｜文芸モデル](/Users/asami/src/dev2025/simplemodeling-org/src/main/doxsite/literate-modeling/what-is-literate-model.dox)"

      When("the inline parser reads the link")
      val result = DoxInlineParser.parse(source)

      Then("the link destination and bilingual label are retained")
      result shouldBe a [Hyperlink]
      val link = result.asInstanceOf[Hyperlink]
      link.href.toString shouldBe "/Users/asami/src/dev2025/simplemodeling-org/src/main/doxsite/literate-modeling/what-is-literate-model.dox"
      link.contents should have size 1
      link.contents.head shouldBe a [Text]
      link.contents.head.asInstanceOf[Text].contents shouldBe "Literate Model｜文芸モデル"
    }

    "parse pass inline macro as raw contents" in {
      Given("a pass inline macro containing a regular-expression payload")
      val source = "pass:[^[A-Z]{2}$]"

      When("the SmartDox inline parser reads the macro")
      val result = DoxInlineParser.parse(DoxInlineParser.Config.smartdox, source)

      Then("the macro retains its raw payload and serialized representation")
      result shouldBe a [InlineMacro]
      val macroNode = result.asInstanceOf[InlineMacro]
      macroNode.name shouldBe "pass"
      macroNode.contents shouldBe "^[A-Z]{2}$"
      macroNode.toText shouldBe "^[A-Z]{2}$"
      macroNode.toData shouldBe "pass:[^[A-Z]{2}$]"
    }

    "preserve site inline macro provenance as a Site link" in {
      Given("a SmartDox site inline macro")

      When("the SmartDox inline parser reads the macro")
      val link = DoxInlineParser.parse(
        DoxInlineParser.Config.smartdox,
        "site:[overview.dox]"
      ).asInstanceOf[Hyperlink]

      Then("the authored target remains an internal URI with distinct Site provenance")
      link.href.toString shouldBe "overview.dox"
      link.contents shouldBe Vector(Text("overview.dox"))
      link.linkKind shouldBe Hyperlink.LinkKind.Site
      link.isSite shouldBe true
    }

    "warn legacy single bracket site link" in {
      Given("a legacy single-bracket SmartDox site link")
      val buffer = new ByteArrayOutputStream()

      When("the SmartDox inline parser reads the legacy form")
      val result = scala.Console.withErr(new PrintStream(buffer)) {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, "[overview.dox]")
      }

      Then("the parser preserves the legacy link and emits its migration warning")
      buffer.toString("UTF-8") should include (
        "warning: Deprecated SmartDox site link '[overview.dox]'. Use 'site:[overview.dox]' instead."
      )
      result shouldBe Hyperlink(Vector(Text("overview.dox")), "overview.dox")
    }

    "keep markdown link label without legacy site link warning" in {
      Given("a standard Markdown link")
      val buffer = new ByteArrayOutputStream()

      When("the SmartDox inline parser reads the Markdown form")
      val result = scala.Console.withErr(new PrintStream(buffer)) {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, "[overview](overview.dox)")
      }

      Then("the parser retains the Markdown link without emitting a legacy warning")
      buffer.toString("UTF-8") should not include "Deprecated SmartDox site link"
      result shouldBe a [Hyperlink]
      val link = result.asInstanceOf[Hyperlink]
      link.href.toString shouldBe "overview.dox"
      link.contents shouldBe Vector(Text("overview"))
    }

    "keep asciidoc link label without legacy site link warning" in {
      Given("an AsciiDoc link")
      val buffer = new ByteArrayOutputStream()

      When("the SmartDox inline parser reads the AsciiDoc form")
      val result = scala.Console.withErr(new PrintStream(buffer)) {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, "link:overview.dox[Overview]")
      }

      Then("the parser retains the link without emitting a legacy warning")
      buffer.toString("UTF-8") should not include "Deprecated SmartDox site link"
      result shouldBe a [Fragment]
    }

    "keep quoted bracket text without legacy site link warning" in {
      Given("quoted bracket text that is not a site link")
      val buffer = new ByteArrayOutputStream()

      When("the SmartDox inline parser reads the text")
      val result = scala.Console.withErr(new PrintStream(buffer)) {
        DoxInlineParser.parse(DoxInlineParser.Config.smartdox, """["SimpleEntity"]""")
      }

      Then("the bracket text remains text without emitting a legacy warning")
      buffer.toString("UTF-8") should not include "Deprecated SmartDox site link"
      result shouldBe Text("""[&quot;SimpleEntity&quot;]""")
    }

    "parse back quoted angle brackets as code inside span" in {
      Given("a span that contains two back-quoted angle-bracket expressions")
      val source =
        """<span lang="ja">`minimal.main.hello` は `<component>.<service>.<operation>` の形です。</span>"""

      When("the SmartDox inline parser reads the span")
      val result = DoxInlineParser.parse(
        DoxInlineParser.Config.smartdox,
        source
      )

      Then("both quoted expressions are retained as code")
      val codes = _collect_code_text(result)
      codes should contain ("minimal.main.hello")
      codes should contain ("<component>.<service>.<operation>")
    }

    "keep inline tag outside back quote inside span" in {
      Given("a span with an inline tag outside any back-quoted text")
      val source = """<span lang="ja">This is <i>important</i>.</span>"""

      When("the SmartDox inline parser reads the span")
      val result = DoxInlineParser.parse(
        DoxInlineParser.Config.smartdox,
        source
      )

      Then("the nested tag remains italic text rather than code")
      _contains_italic(result) shouldBe true
      _collect_code_text(result) shouldBe Nil
    }
  }

  "generic inline tags" should {
    "preserve a terminal boolean attribute as an empty-string value" in {
      Given("a generic span with a terminal boolean attribute and text content")
      val source = "<span enabled>text</span>"

      When("the inline parser reads the source")
      val (_, result, _) = DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)

      Then("the supported span retains the boolean attribute and its contents")
      val nodes = result.toOption.get
      nodes should have size 1
      val span = nodes.head.asInstanceOf[Span]
      span.attributes.list shouldBe List("enabled" -> "")
      span.contents shouldBe List(Text("text"))
    }

    "preserve quoted and terminal boolean attributes in source order" in {
      Given("a generic span with a quoted attribute followed by a boolean attribute")
      val source = "<span lang=\"ja\" enabled>text</span>"

      When("the inline parser reads the source")
      val (_, result, _) = DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)

      Then("both attributes are retained in their authored order")
      val nodes = result.toOption.get
      nodes should have size 1
      val span = nodes.head.asInstanceOf[Span]
      span.attributes.list shouldBe List("lang" -> "ja", "enabled" -> "")
      span.contents shouldBe List(Text("text"))
    }

    "create empty supported nodes for generic self-closing forms" in {
      Given("generic spans in empty and boolean self-closing forms")
      val sources = Vector("<span/>", "<span enabled/>")

      When("the inline parser reads both sources")
      val results = sources.map { source =>
        val (_, result, _) = DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)
        result.toOption.get
      }
      val recovered = results.map { nodes =>
        nodes.flatMap(_nodes).collect { case m: Span => m }
      }

      Then("each form produces one empty span without requiring a close tag")
      recovered should have size 2
      recovered.foreach { spans =>
        spans should have size 1
        spans.head shouldBe a [Span]
        spans.head.contents shouldBe Nil
      }
      recovered.head.head.attributes.list shouldBe Nil
      recovered(1).head.attributes.list shouldBe List("enabled" -> "")
    }

    "distinguish a standalone closing tag from self-closing syntax" in {
      Given("a standalone generic closing tag and an immediate self-closing span")
      val closing = "</span>"
      val selfclosingsource = "<span/>"

      When("the inline parser reads both tag forms")
      val error = intercept[org.goldenport.exception.SyntaxErrorFaultException] {
        DoxInlineParser.apply(DoxInlineParser.Config.smartdox, closing)
      }
      val selfclosing = DoxInlineParser.parse(DoxInlineParser.Config.smartdox, selfclosingsource)

      Then("the closing tag is rejected as unmatched and the self-closing form remains an empty span")
      error.getMessage should include("no matching open tag")
      selfclosing shouldBe a [Span]
      selfclosing.asInstanceOf[Span].contents shouldBe Nil
    }

    "preserve standalone self-closing nodes through the public parse method" in {
      Given("standalone generic spans in empty and boolean self-closing forms")
      val sources = Vector("<span/>", "<span enabled/>")

      When("the public inline parser parses each source")
      val parsed = sources.map(source => DoxInlineParser.parse(DoxInlineParser.Config.smartdox, source))

      Then("both supported spans remain present with empty contents and authored attributes")
      parsed should have size 2
      parsed.foreach { dox =>
        dox shouldBe a [Span]
        dox.asInstanceOf[Span].contents shouldBe Nil
      }
      parsed.head.asInstanceOf[Span].attributes.list shouldBe Nil
      parsed(1).asInstanceOf[Span].attributes.list shouldBe List("enabled" -> "")
    }

    "retain nested boolean and self-closing generic nodes in authored order" in {
      Given("a generic parent containing a terminal boolean child and self-closing children")
      val source = "<span>before<span enabled>flag</span><span/><span enabled/>after</span>"

      When("the public inline parser parses the nested generic source")
      val parsed = DoxInlineParser.parse(DoxInlineParser.Config.smartdox, source)

      Then("the parent retains each nested node, empty contents, and source order")
      parsed shouldBe a [Span]
      val parent = parsed.asInstanceOf[Span]
      parent.contents should have size 5
      parent.contents(0) shouldBe Text("before")
      parent.contents(1) shouldBe a [Span]
      val booleanchild = parent.contents(1).asInstanceOf[Span]
      booleanchild.attributes.list shouldBe List("enabled" -> "")
      booleanchild.contents shouldBe List(Text("flag"))
      parent.contents(2) shouldBe a [Span]
      parent.contents(2).asInstanceOf[Span].contents shouldBe Nil
      parent.contents(3) shouldBe a [Span]
      val selfclosingbooleanchild = parent.contents(3).asInstanceOf[Span]
      selfclosingbooleanchild.attributes.list shouldBe List("enabled" -> "")
      selfclosingbooleanchild.contents shouldBe Nil
      parent.contents(4) shouldBe Text("after")
    }

    "retain self-closing nodes and surrounding text in authored order" in {
      Given("ordinary text surrounding empty and boolean self-closing spans")
      val source = "before<span/>middle<span enabled/>after"

      When("the inline parser reads the mixed result-flow source")
      val (_, result, _) = DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)
      val nodes = result.toOption.get.flatMap {
        case m: Fragment => m.contents
        case m => Vector(m)
      }

      Then("the parser retains both spans and all surrounding text in source order")
      nodes should have size 5
      nodes(0) shouldBe Text("before")
      nodes(1) shouldBe a [Span]
      nodes(1).asInstanceOf[Span].attributes.list shouldBe Nil
      nodes(1).asInstanceOf[Span].contents shouldBe Nil
      nodes(2) shouldBe Text("middle")
      nodes(3) shouldBe a [Span]
      nodes(3).asInstanceOf[Span].attributes.list shouldBe List("enabled" -> "")
      nodes(3).asInstanceOf[Span].contents shouldBe Nil
      nodes(4) shouldBe Text("after")
    }

    "reject a slash that is not immediately followed by a closing angle bracket" in {
      Given("a self-closing span whose slash is followed by whitespace")
      val source = "<span enabled/ >"

      When("the inline parser reads the malformed source")
      val error = intercept[org.goldenport.exception.NoReachDefectException] {
        DoxInlineParser.apply(DoxInlineParser.Config.smartdox, source)
      }

      Then("the existing deterministic parser failure boundary is retained")
      error shouldBe a [org.goldenport.exception.NoReachDefectException]
    }
  }

  private def _collect_code_text(dox: Dox): List[String] = {
    val own = dox match {
      case m: Code => List(m.contents.map(_.toText).mkString)
      case _ => Nil
    }
    own ++ dox.elements.toList.flatMap(_collect_code_text)
  }

  private def _contains_italic(dox: Dox): Boolean =
    dox.isInstanceOf[Italic] || dox.elements.exists(_contains_italic)

  private def _nodes(dox: Dox): Vector[Dox] =
    dox +: dox.elements.toVector.flatMap(_nodes)
}
