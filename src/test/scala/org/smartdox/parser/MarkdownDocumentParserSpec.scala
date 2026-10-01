package org.smartdox.parser

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.goldenport.parser.{ParseLocation, ParseSuccess}
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData

/*
 * @since   Oct.  1, 2026
 * @version Oct.  1, 2026
 * @author  ASAMI, Tomoharu
 */
class MarkdownDocumentParserSpec extends AnyWordSpec with Matchers with GivenWhenThen
    with ScalaCheckPropertyChecks {
  "Pure Markdown source interpretation" should {
    "retain native document structure" which {
      "close one-line headings and retain preamble, nested sections and siblings" in {
        Given("Markdown with a preamble and consecutive heading/body lines without blank separators")
        val source = "Preamble.\n\n# First\nFirst body.\n## Child\nChild body.\n# Second\nSecond body."

        When("the source is parsed with the default Markdown filename profile")
        val document = _document(source)

        Then("the preamble precedes two native level-one sections and the child belongs to the first")
        document.body.elements.head shouldBe Paragraph(List(Text("Preamble.")))
        val sections = document.body.elements.collect { case section: Section => section }
        sections.map(_.titleName) shouldBe List("First", "Second")
        sections.map(_.level) shouldBe List(1, 1)
        sections.head.contents.head shouldBe Paragraph(List(Text("First body.")))
        val child = sections.head.contents(1).asInstanceOf[Section]
        child.titleName shouldBe "Child"
        child.level shouldBe 2
        child.contents shouldBe List(Paragraph(List(Text("Child body."))))
        document.toString should include ("<h2>First</h2>")
        document.toString should include ("<h3>Child</h3>")
      }

      "retain authored heading levels across the finite CommonMark range" in {
        forAll(Gen.choose(1, 6)) { level =>
          Given("an authored ATX heading level from the CommonMark range")
          val source = "#" * level + " Heading\nBody."

          When("the heading is projected into the native document")
          val section = _document(source).body.elements.head.asInstanceOf[Section]

          Then("the authored level and body are retained without metadata promotion")
          section.level shouldBe level
          section.titleName shouldBe "Heading"
          section.contents shouldBe List(Paragraph(List(Text("Body."))))
        }
      }

      "retain Setext headings as body sections" in {
        Given("a first Setext heading and a following paragraph")
        val source = "Title\n=====\n\nOrdinary body."

        When("the source is parsed as Pure Markdown")
        val document = _document(source)

        Then("the title is a body section and the head stays empty")
        document.head shouldBe Head()
        val section = document.body.elements.head.asInstanceOf[Section]
        section.titleName shouldBe "Title"
        section.level shouldBe 1
        section.contents shouldBe List(Paragraph(List(Text("Ordinary body."))))
      }
    }

    "project Markdown inlines without Dox grammar" which {
      "retain nested emphasis, escapes, word underscores and opaque code" in {
        Given("CommonMark emphasis nesting, escaped punctuation, an internal underscore and Dox-only inline code")
        val source = "*outer **inner** end* and \\*literal\\* word_part `site:[target.dox] [[image.png]] _source`"

        When("the paragraph becomes native SmartDox inlines")
        val paragraph = _document(source).body.elements.head.asInstanceOf[Paragraph]

        Then("Em contains Strong and the remaining punctuation and code remain literal")
        val em = paragraph.contents.head.asInstanceOf[Em]
        em.contents shouldBe List(Text("outer "), Strong(List(Text("inner"))), Text(" end"))
        val code = paragraph.contents.collectFirst { case node: Code => node }.get
        code.contents shouldBe List(Text("site:[target.dox] [[image.png]] _source"))
        paragraph.toText should include ("*literal* word_part")
        _all(paragraph).exists(_.isInstanceOf[InlineMacro]) shouldBe false
      }

      "resolve reference links and titles while keeping unresolved references literal" in {
        Given("a Markdown reference definition, nested label emphasis and an unresolved reference")
        val source = "[*label*][target] and [missing][unknown]\n\n[target]: https://example.com/path \"Link title\""

        When("CommonMark resolves references before native projection")
        val document = _document(source)
        val link = _all(document).collectFirst { case node: Hyperlink => node }.get

        Then("the resolved URI, label and title belong to Hyperlink, unknown references stay text and definitions are invisible")
        link.href shouldBe new URI("https://example.com/path")
        link.contents shouldBe List(Em(List(Text("label"))))
        link.title.map(_.en) shouldBe Some("Link title")
        document.toText should include ("[missing][unknown]")
        document.body.toText should not include ("[target]:")
      }

      "retain valid Markdown destinations that require URI escaping" in {
        Given("a Markdown link whose angle-delimited destination contains a space and Japanese text")
        val source = "[label](<guides/日本語 page.html>)"

        When("the parsed destination becomes a native URI")
        val link = _all(_document(source)).collectFirst { case node: Hyperlink => node }.get

        Then("URI encoding preserves the resolved path without grammar reinterpretation")
        link.href.getPath shouldBe "guides/日本語 page.html"
        link.href.toASCIIString should include ("%20")
        link.contents shouldBe List(Text("label"))
      }

      "normalize soft breaks and retain hard breaks and entities once" in {
        Given("soft and hard Markdown line breaks with escaped punctuation and an entity")
        val source = "soft\nline  \nhard &amp; \\!"

        When("the paragraph is projected")
        val paragraph = _document(source).body.elements.head.asInstanceOf[Paragraph]

        Then("soft breaks are spaces, hard breaks are native br and entity text is decoded once")
        paragraph.contents should contain (Text(" "))
        paragraph.contents.collect { case node: Html5Inline => node.name } shouldBe List("br")
        paragraph.toText should include ("hard & !")
        paragraph.toString should include ("&amp;")
        paragraph.toString should not include ("&amp;amp;")
      }
    }

    "retain typed blocks" which {
      "retain tight nested lists and authored ordered starts without definition lists" in {
        Given("a tight ordered list starting at three, nested bullets and definition-looking text")
        val source = "3. first\n   - nested\n4. term :: definition"

        When("the list is parsed as Pure Markdown")
        val ordered = _document(source).body.elements.head.asInstanceOf[Ol]

        Then("the ordered start and nested Ul are retained, direct paragraphs are flattened and no Dl appears")
        ordered.attributes.get("start") shouldBe Some("3")
        ordered.contents.head.contents.head shouldBe Text("first")
        ordered.contents.head.contents(1) shouldBe a[Ul]
        ordered.contents(1).contents shouldBe List(Text("term :: definition"))
        _all(ordered).exists(_.isInstanceOf[Dl]) shouldBe false
        ordered.toString should include ("<li>first<ul>")
      }

      "retain loose list paragraphs and omit the default ordered start attribute" in {
        Given("a loose bullet list with a nested ordered list starting at one")
        val source = "- first\n\n  second paragraph\n\n  1. nested\n\n- last"

        When("the source becomes native list content")
        val list = _document(source).body.elements.head.asInstanceOf[Ul]

        Then("loose paragraphs remain native Paragraphs and the default ordered start is absent")
        list.contents.head.contents.head shouldBe a[Paragraph]
        list.contents.head.contents(1) shouldBe Paragraph(List(Text("second paragraph")))
        val ordered = _all(list).collectFirst { case node: Ol => node }.get
        ordered.attributes.get("start") shouldBe None
        list.toString should include ("<li><p>first</p>")
      }

      "retain quotes with nested headings and thematic breaks" in {
        Given("a quote containing a heading and paragraph followed by a thematic break")
        val source = "> # Quote\n> Body.\n\n---"

        When("the block container is projected")
        val document = _document(source)

        Then("the quote contains a native Section and the sibling is a HorizontalRule")
        val quote = document.body.elements.head.asInstanceOf[Quotation.BlockQuote]
        quote.contents.head shouldBe a[Section]
        quote.contents.head.asInstanceOf[Section].contents shouldBe List(Paragraph(List(Text("Body."))))
        document.body.elements(1) shouldBe a[HorizontalRule]
        document.toString should include ("<blockquote>")
        document.toString should include ("<hr/>")
      }

      "retain fenced and indented code literally with kind and empty callouts" in {
        Given("code containing a Dox callout, HEAD, site macro and unavailable include")
        val literal = "// <1> note\n# HEAD\nsite:[target.dox]\ninclude::missing.dox[]\n"
        val source = "```scala extra info\n" + literal + "```\n\n    indented\n    // <1> note\n"

        When("both code block forms become native Programs")
        val programs = _all(_document(source)).collect { case node: Program => node }

        Then("literal text and the first info token survive without callout extraction or file reads")
        programs.map(_.contents) shouldBe List(literal, "indented\n// <1> note\n")
        programs.map(_.kind) shouldBe List(Some("scala"), None)
        programs.foreach(_.callouts shouldBe Dox.Callouts.empty)
      }

      "retain empty and unknown fenced info as ordinary literal code" in {
        Given("one untyped fence and one fence with an unknown first info token")
        val source = "```\nplain\n```\n\n```unknown extra\nopaque\n```"

        When("the code blocks are projected into Programs")
        val programs = _all(_document(source)).collect { case node: Program => node }

        Then("both remain literal code and only the authored first token becomes kind")
        programs.map(_.contents) shouldBe List("plain\n", "opaque\n")
        programs.map(_.kind) shouldBe List(None, Some("unknown"))
        programs.foreach(_.callouts shouldBe Dox.Callouts.empty)
      }

      "retain aligned GFM tables with typed cells, emphasis, code and escaped delimiters" in {
        Given("a GFM table with all alignment directions and Markdown cell syntax")
        val source = "| *Left* | Center | Right |\n| :--- | :---: | ---: |\n| a\\|b | `co\\|de` | **strong** |"

        When("the Markdown table becomes a native Table")
        val table = _document(source).body.elements.head.asInstanceOf[Table]

        Then("native columns, header and body cells retain alignments and inline semantics")
        table.colgroup.get.cols.map(_.align) shouldBe List(Some(org.smartdox.Table.Align.Left), Some(org.smartdox.Table.Align.Center), Some(org.smartdox.Table.Align.Right))
        table.head.get.records.head.fields.head shouldBe TH(List(Em(List(Text("Left")))))
        val fields = table.body.records.head.fields
        fields.head shouldBe TD(List(Text("a|b")))
        fields(1) shouldBe TD(List(Code(List(Text("co|de")))))
        fields(2) shouldBe TD(List(Strong(List(Text("strong")))))
        table.toString should include ("<col align=\"center\"/>")
        table.toString should include ("<thead><tr><th><em>Left</em></th>")
      }

      "omit a colgroup when no Markdown table alignment is authored" in {
        Given("a plain GFM table without column alignment")
        val source = "| Header |\n| --- |\n| Body |"

        When("the table is projected")
        val table = _document(source).body.elements.head.asInstanceOf[Table]

        Then("typed table compartments are retained without synthetic columns")
        table.colgroup shouldBe None
        table.head.get.records.head.fields.head shouldBe TH(List(Text("Header")))
        table.body.records.head.fields.head shouldBe TD(List(Text("Body")))
      }
    }

    "retain ordinary HTML" which {
      "bridge inline HTML nesting while retaining Markdown formatting and literal macros" in {
        Given("an inline HTML span containing Markdown emphasis, ordinary HTML strong and Dox-looking text")
        val source = "before <span class=\"note\">*em* <strong>bold</strong> &amp; site:[target.dox]</span> after <!--hidden-->"

        When("the HTML-bearing inline sequence is projected")
        val document = _document(source)
        val span = _all(document).collectFirst { case node: Html5Inline if node.name == "span" => node }.get

        Then("attributes and native inline formatting survive without Dox macros or comment content")
        span.attributes.get("class") shouldBe Some("note")
        _all(span).collect { case node: Em => node.toText } shouldBe List("em")
        _all(span).collect { case node: Strong => node.toText } shouldBe List("bold")
        span.toText should include ("& site:[target.dox]")
        _all(document).exists(_.isInstanceOf[InlineMacro]) shouldBe false
        document.toString should not include ("hidden")
      }

      "retain raw HTML blocks without HEAD, include or special-tag interpretation" in {
        Given("a raw HTML block with Dox-like tags, literal includes, an ordinary hyperlink and image")
        val source = "<div class=\"raw\"><HEAD>status=published</HEAD><include>include::missing.dox[]</include><p>*literal*</p><a href=\"local.html\" title=\"Local\">link</a><img src=\"images/local.png\" alt=\"図\" title=\"Picture\"></div>"
        val config = Dox2Parser.Config.markdown._with_virtual_resource_parent("articles")

        When("the raw HTML fragment is projected with its explicit resource origin")
        val document = _document(source, config)
        val block = document.body.elements.head.asInstanceOf[Html5]

        Then("generic HTML preserves attributes and text while a and img use native resource nodes")
        block.name shouldBe "div"
        block.attributes.get("class") shouldBe Some("raw")
        document.head shouldBe Head()
        document.toText should include ("status=published")
        document.toText should include ("include::missing.dox[]")
        document.toText should include ("*literal*")
        _all(document).exists(_.isInstanceOf[Include]) shouldBe false
        _all(document).collectFirst { case node: Hyperlink => node.href } shouldBe Some(new URI("local.html"))
        val image = _all(document).collectFirst { case node: ReferenceImg => node }.get
        image.src shouldBe new URI("images/local.png")
        image.alt shouldBe Some("図")
        image.attributes.get("title") shouldBe Some("Picture")
      }
    }

    "admit images with Markdown syntax and explicit origins" which {
      "retain inline and reference images with Japanese, emphasized and empty alt and titles" in {
        Given("CommonMark image forms with formatted Japanese alt, a reference and an empty alt")
        val source = "![*日本語* `図`](images/../images/diagram.png \"説明\") ![Reference][picture] ![](images/empty.png)\n\n[picture]: images/ref.png 'Reference title'"
        val config = Dox2Parser.Config.markdown._with_virtual_resource_parent("guides")

        When("the parsed image AST nodes enter existing local resource admission")
        val document = _document(source, config)
        val images = _all(document).collect { case node: ReferenceImg => node }

        Then("normalized resource URIs, plain alt and optional title attributes are retained and definitions are invisible")
        images.map(_.src.toString) shouldBe List("images/diagram.png", "images/ref.png", "images/empty.png")
        images.map(_.alt) shouldBe List(Some("日本語 図"), Some("Reference"), Some(""))
        images.map(_.attributes.get("title")) shouldBe List(Some("説明"), Some("Reference title"), None)
        document.body.toText should not include ("[picture]:")
      }

      "admit missing physical resources without probing existence" in {
        Given("a physical document resource root and a path to an absent local image")
        val config = Dox2Parser.Config.markdown.withResourceRoot(Paths.get("target/phase75/absent-image-root"))
        val source = "![Absent](images/./absent.png)"

        When("the image is admitted from its Markdown AST")
        val image = _all(_document(source, config)).collectFirst { case node: ReferenceImg => node }.get

        Then("the model retains the normalized URI for the renderer-owned missing-resource outcome")
        image.src shouldBe new URI("images/absent.png")
        image.alt shouldBe Some("Absent")
      }

      "reject absent origins, escaping paths and unsupported remote or absolute resources" in {
        val cases = Vector(
          (Dox2Parser.Config.markdown, "images/image.png"),
          (Dox2Parser.Config.markdown._with_virtual_resource_parent("guides"), "../image.png"),
          (Dox2Parser.Config.markdown.withResourceRoot(Paths.get("target/phase75/images")), "../image.png"),
          (Dox2Parser.Config.markdown._with_virtual_resource_parent("guides"), "https://example.com/image.png"),
          (Dox2Parser.Config.markdown._with_virtual_resource_parent("guides"), "/image.png")
        )
        cases.foreach { case (config, destination) =>
          Given("a Markdown image with an absent or unsupported explicit resource origin/path")
          val source = s"![図]($destination)"

          When("the parsed image reaches resource admission")
          val failure = intercept[IllegalArgumentException] { _document(source, config) }

          Then("the existing unsupported-resource diagnostic retains the image source context")
          failure.getMessage should include ("image.markdown.unsupported-resource")
          failure.getMessage should include (source)
          failure.getMessage should include (destination)
        }
      }

      "attribute image diagnostics and nodes to body-relative source spans" in {
        Given("YAML metadata preceding a Markdown body with an image on its third line")
        val source = "---\nstatus: published\n---\n# Body\n\n![図](images/image.png)"
        val config = Dox2Parser.Config.markdown._with_virtual_resource_parent("guides")

        When("the static filename API parses front matter and the Markdown body")
        val document = Dox2Parser.parseWithFilename(config, "guides/page.md", source).asInstanceOf[Document]
        val image = _all(document).collectFirst { case node: ReferenceImg => node }.get

        Then("locations remain relative to the split body and retain the configured file")
        image.location shouldBe Some(ParseLocation(Some(new URI("guides/page.md")), Some(3), Some(1)))

        When("the same body is parsed with location reporting disabled")
        val disabled = _document("![図](images/image.png)", config.copy(isLocation = false))

        Then("image locations remain absent")
        _all(disabled).collectFirst { case node: ReferenceImg => node.location }.get shouldBe None
      }
    }

    "honor explicit parser mode and API scope" which {
      "keep HEAD, metadata-looking text, macros and Org brackets as Pure body" in {
        Given("a Markdown body containing HEAD, status, _source and SmartDox-only source forms")
        val source = "# HEAD\nstatus=published\n_source\n\nsite:[target.dox] [[target.dox][label]]\n\ninclude::missing.dox[]\n\n#+INCLUDE: missing.dox"

        When("the default Pure filename profile parses the body")
        val document = _document(source)

        Then("metadata is absent, all operational-looking text is visible and no macro/include nodes exist")
        document.head shouldBe Head()
        document.head.metadata.status should not be DocumentMetaData.Status.Published
        document.toText should include ("status=published")
        document.toText should include ("_source")
        document.toText should include ("site:[target.dox]")
        document.toText should include ("[[target.dox][label]]")
        document.toText should include ("include::missing.dox[]")
        _all(document).exists(x => x.isInstanceOf[Include] || x.isInstanceOf[InlineMacro]) shouldBe false
      }

      "default both suffixes and direct body APIs to Pure" in {
        Given("a Markdown heading followed immediately by body content with locations disabled for semantic comparisons")
        val source = "# Body\n*emphasis*"
        val config = Dox2Parser.Config.markdown.copy(isLocation = false)

        When("both suffixes, configured class parsing, parseOne, parseFragment and parse2 interpret the source")
        val documents = List("body.md", "body.markdown").map(name =>
          Dox2Parser.parseWithFilename(config, name, source).asInstanceOf[Document])
        val direct = Dox2Parser.create(config).apply(source).asInstanceOf[ParseSuccess[Document]].ast
        val one = Dox2Parser.parseOne(config, source)
        val fragment = Dox2Parser.parseFragment(config, source)
        val second = Dox2Parser.parse2(config, source)

        Then("each public entry yields native section/emphasis behavior")
        (documents :+ direct).foreach { document =>
          document.body.elements.head shouldBe a[Section]
          _all(document).collect { case node: Em => node.toText } shouldBe List("emphasis")
        }
        one shouldBe documents.head.body.elements.head
        fragment.contents shouldBe documents.head.body.elements
        second shouldBe documents.head
      }

      "merge YAML once in both static modes while direct parsing stays body-oriented" in {
        Given("a YAML-published Markdown source and a body containing only a paragraph")
        val source = "---\ntitle: YAML title\nstatus: published\n---\nBody."
        val configs = List(Dox2Parser.Config.pureMarkdown, Dox2Parser.Config.enhancedMarkdown)

        When("the static API and Pure parse2 process front matter, and the class API processes body bytes")
        val documents = configs.map(config => Dox2Parser.parse(config, source).asInstanceOf[Document])
        val second = Dox2Parser.parse2(Dox2Parser.Config.markdown, source).asInstanceOf[Document]
        val direct = Dox2Parser.create(Dox2Parser.Config.markdown).apply(source).
          asInstanceOf[ParseSuccess[Document]].ast

        Then("both static modes share YAML metadata and direct body parsing leaves metadata absent")
        documents.foreach { document =>
          document.head.metadata.status shouldBe DocumentMetaData.Status.Published
          document.body.toText shouldBe "Body."
        }
        second shouldBe documents.head
        direct.head shouldBe Head()
      }

      "preserve explicit Enhanced across filename, style, origin and file copies" in {
        Given("the explicit Enhanced preset with physical and virtual origins")
        val enhanced = Dox2Parser.Config.enhancedMarkdown
        val root = Paths.get("target/phase75/mode-origin")
        val configs = List(enhanced.withFilename("page.md"), enhanced.withFilename("page.markdown"),
          enhanced.withDoxStyle(Dox2Parser.Config.DoxStyle.Markdown),
          enhanced.withResourceRoot(root).withFilename("page.md"),
          enhanced._with_virtual_resource_parent("guides").withFilename("page.md"),
          enhanced.withPathname("page.md"),
          enhanced.copy(isDebug = true, isLocation = false),
          enhanced.copy(file = Some(new URI("page.md"))),
          enhanced.withInlineConfig(enhanced.linesConfig.inlineConfig),
          enhanced.withDoxStyle(Dox2Parser.Config.DoxStyle.SmartDox).withFilename("page.md"))
        val source = "# HEAD\n\nstatus=published\n\n# Body\n\nsite:[target.dox]\n\n- term :: definition"

        When("each copied config parses the Enhanced body")
        val documents = configs.map(config => Dox2Parser.parse(config, source).asInstanceOf[Document])

        Then("selection survives and the legacy HEAD, macro and definition-list contracts remain")
        configs.foreach(_.markdownMode shouldBe Dox2Parser.Config.MarkdownMode.Enhanced)
        documents.foreach { document =>
          document.head.metadata.status shouldBe DocumentMetaData.Status.Published
          _all(document).collect { case node: Hyperlink if node.attributes.get("class").contains("site") => node.href } shouldBe List(new URI("target.dox"))
          _all(document).exists(_.isInstanceOf[Dl]) shouldBe true
        }

        When("identical source is parsed with Pure")
        val pure = _document(source)

        Then("HEAD properties and macro syntax stay ordinary Markdown body content")
        pure.head shouldBe Head()
        pure.toText should include ("status=published")
        pure.toText should include ("site:[target.dox]")
        _all(pure).exists(_.isInstanceOf[Dl]) shouldBe false
      }

      "switch Markdown mode on fresh copies while preserving originals and shared presets" in {
        Given("shared Pure and Enhanced presets and a HEAD body with publication status")
        val purepreset = Dox2Parser.Config.pureMarkdown
        val enhancedpreset = Dox2Parser.Config.enhancedMarkdown
        val source = "# HEAD\n\nstatus=published\n\n# Body\n\nBody."

        When("the Pure preset is copied with locations disabled")
        val original = purepreset.copy(isLocation = false)

        Then("the copy is Pure and both shared presets retain their selected modes")
        purepreset.markdownMode shouldBe Dox2Parser.Config.MarkdownMode.Pure
        enhancedpreset.markdownMode shouldBe Dox2Parser.Config.MarkdownMode.Enhanced
        original.markdownMode shouldBe Dox2Parser.Config.MarkdownMode.Pure

        When("Enhanced is selected on the copy and Pure is selected again on its result")
        val enhanced = original.withMarkdownMode(Dox2Parser.Config.MarkdownMode.Enhanced)
        val restored = enhanced.withMarkdownMode(Dox2Parser.Config.MarkdownMode.Pure)
        val copied = enhanced.copy(isDebug = true)

        Then("each result has its selected mode and the original and shared presets retain theirs")
        List(enhanced, copied, enhancedpreset).foreach(_.markdownMode shouldBe Dox2Parser.Config.MarkdownMode.Enhanced)
        List(original, restored, purepreset).foreach(_.markdownMode shouldBe Dox2Parser.Config.MarkdownMode.Pure)

        When("the same HEAD and status body is parsed with each original, switched and copied config")
        val enhanceddocuments = List(enhanced, copied).map(config => _document(source, config))
        val puredocuments = List(original, restored).map(config => _document(source, config))

        Then("Enhanced interprets publication metadata while original and restored Pure retain visible body text")
        enhanceddocuments.foreach { document =>
          document.head.metadata.status shouldBe DocumentMetaData.Status.Published
          document.body.toText should include ("Body.")
        }
        puredocuments.foreach { document =>
          document.head shouldBe Head()
          document.toText should include ("status=published")
          document.body.toText should include ("Body.")
        }
        List(enhanced, copied, enhancedpreset).foreach(_.markdownMode shouldBe Dox2Parser.Config.MarkdownMode.Enhanced)
        List(original, restored, purepreset).foreach(_.markdownMode shouldBe Dox2Parser.Config.MarkdownMode.Pure)
      }

      "leave SmartDox and Org source selection independent of Markdown mode" in {
        Given("a SmartDox HEAD body and an Org heading body under each Markdown mode")
        val doxsource = "# HEAD\n\nstatus=published\n\n# Body\n\nDox body."
        val orgsource = "* Body\n\nOrg body."
        val configs = List(Dox2Parser.Config.pureMarkdown, Dox2Parser.Config.enhancedMarkdown)

        When("filename selection applies the existing .dox and .org styles")
        val doxdocuments = configs.map(config => Dox2Parser.parseWithFilename(config, "body.dox", doxsource).asInstanceOf[Document])
        val orgdocuments = configs.map(config => Dox2Parser.parseWithFilename(config, "body.org", orgsource))

        Then("both Markdown modes produce the same existing SmartDox and Org behavior")
        doxdocuments.head shouldBe doxdocuments(1)
        doxdocuments.head.head.metadata.status shouldBe DocumentMetaData.Status.Published
        orgdocuments.head shouldBe orgdocuments(1)
        orgdocuments.head.toText should include ("Org body.")
      }

      "retain explicit Enhanced include resolution from an absolute physical base" in {
        Given("a target beneath a Phase75 fixture directory and identical source for both modes")
        val parent = Paths.get("target/phase75/enhanced-include")
        Files.createDirectories(parent)
        val root = Files.createTempDirectory(parent, "source-").toAbsolutePath
        try {
          Files.write(root.resolve("included.dox"), "Included body.".getBytes(StandardCharsets.UTF_8))
          val source = "# Body\n\ninclude::included.dox[]"
          val config = Dox2Parser.Config.enhancedMarkdown.copy(file = Some(root.resolve("page.md").toUri))

          When("Enhanced resolves the include with its existing physical-base resolver")
          val enhanced = Dox2Parser.parse(config, source).asInstanceOf[Document]

          Then("the included body is present")
          enhanced.toText should include ("Included body.")

          When("Pure receives identical source and physical base")
          val pure = Dox2Parser.parse(config.withMarkdownMode(Dox2Parser.Config.MarkdownMode.Pure), source)

          Then("the include directive remains literal body text")
          pure.toText should include ("include::included.dox[]")
          pure.toText should not include ("Included body.")
        } finally {
          _delete(root)
        }
      }
    }
  }

  private def _document(source: String, config: Dox2Parser.Config = Dox2Parser.Config.markdown.copy(isLocation = false)): Document =
    Dox2Parser.parseWithFilename(config, "spec.md", source).asInstanceOf[Document]

  private def _all(dox: Dox): List[Dox] = dox :: dox.elements.flatMap(_all)

  private def _delete(path: Path): Unit = {
    val stream = Files.walk(path)
    try stream.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists(_))
    finally stream.close()
  }
}
