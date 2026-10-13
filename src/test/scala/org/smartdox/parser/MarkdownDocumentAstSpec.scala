package org.smartdox.parser

import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._
import org.goldenport.parser.ParseLocation
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData

/*
 * @since   Oct.  6, 2026
 * @version Oct. 13, 2026
 */
class MarkdownDocumentAstSpec extends AnyWordSpec with Matchers with GivenWhenThen with ScalaCheckPropertyChecks {
  "Markdown document structure" should {
    "interpret headings and metadata" which {
      "P751-01C1-01 retain preamble, real heading levels and ordered nesting" in {
        Given("a preamble followed by ATX and Setext headings with a skipped level")
        val source = "Preamble.\n\n# *Root*\n\n### Deep\n\nDeep body.\n\n## Peer\n\nPeer body.\n\nNext\n====\n\nNext body."
        When("the Markdown document is interpreted")
        val document = _parse(source)
        Then("the preamble precedes sections and no heading is fabricated")
        document.body.contents.head shouldBe a[Paragraph]
        val roots = document.body.contents.collect { case x: Section => x }
        roots.map(_.titleName) shouldBe List("Root", "Next")
        roots.map(_.level) shouldBe List(1, 1)
        roots.head.title.head shouldBe an[Em]
        roots.head.contents.collect { case x: Section => (x.titleName, x.level) } shouldBe
          List("Deep" -> 3, "Peer" -> 2)
        document.head.metadata.title shouldBe None
      }
      "P751-01C1-02 preserve a first Setext level-two title and all ATX levels" in {
        Given("a level-two Setext heading followed by increasing ATX levels")
        val source = "First **title**\n---\n\n### Three\n\n#### Four\n\n##### Five\n\n###### Six"
        When("the Markdown document is interpreted")
        val document = _parse(source)
        Then("the title keeps converted markup and section levels remain two through six")
        _nodes(document).collect { case x: Section => x.level } shouldBe List(2, 3, 4, 5, 6)
        document.head.metadata.title shouldBe None
        document.body.contents.head.asInstanceOf[Section].title.exists(_.isInstanceOf[Strong]) shouldBe true
      }
      "P751-01C1-03 give YAML title authority while retaining status and bibliography" in {
        Given("Markdown YAML metadata and body HEAD/status/include/bibliography text")
        val source = "---\ntitle: YAML title\nstatus: published\nbibliography:\n  refs: [\"bib:metadata\"]\n---\n\n# Body heading\n\nHEAD status = draft\n\ninclude::missing.dox[] site:[guide] bib:[literal]"
        When("the configured front matter and Markdown body are interpreted")
        val document = _parse(source)
        Then("YAML remains metadata and body syntax remains literal text")
        document.head.metadata.titleStringDefault shouldBe "YAML title"
        document.head.metadata.status shouldBe DocumentMetaData.Status.Published
        document.head.metadata.properties.get.getStringList("bibliography.refs").asScala.toList shouldBe List("bib:metadata")
        document.body.contents.head.asInstanceOf[Section].titleName shouldBe "Body heading"
        _text(document.body) should include("HEAD status = draft")
        _text(document.body) should include("include::missing.dox[] site:[guide] bib:[literal]")
        _nodes(document.body).exists(_.isInstanceOf[InlineMacro]) shouldBe false
      }
      "P751-01C1-04 treat HEAD as an ordinary heading and keep raw heading labels" in {
        Given("a HEAD heading and a labelled raw root heading")
        val source = "# HEAD\n\nstatus = draft\n\n<h2 id=\"label\" data-key=\"value\">Label <em>title</em></h2>\n"
        When("the body is interpreted as Markdown")
        val document = _parse(source)
        Then("HEAD is retained and raw root heading labels survive in Section attributes")
        document.head.metadata.title shouldBe None
        val roots = document.body.contents.collect { case x: Section => x }
        roots.head.contents.head shouldBe a[Paragraph]
        val labelled = _nodes(document).collectFirst { case x: Section if x.titleName == "Label title" => x }.get
        labelled.level shouldBe 2
        labelled.attributes.get("id") shouldBe Some("label")
        labelled.attributes.get("data-key") shouldBe Some("value")
      }
    }
    "retain inline meaning" which {
      "P751-01C1-05 preserve Unicode escapes unmatched delimiters and soft breaks" in {
        forAll(Gen.oneOf("日本語 🌱", "Ελληνικά", "العربية", "français", "한국어")) { unicode =>
          Given("a Unicode paragraph with escaped emphasis, unmatched markers and a soft break")
          val source = unicode + " \\*literal\\* unmatched *marker\nsoft line\n\n_source include::missing[] bib:[entry] English｜日本語"
          When("the source is interpreted as Markdown")
          val document = _parse(source)
          Then("decoded literal text and soft newlines survive without Dox or i18n reinterpretation")
          _text(document.body) shouldBe unicode + " *literal* unmatched *marker\nsoft line_source include::missing[] bib:[entry] English｜日本語"
          document.body.contents should have size 2
          _nodes(document.body).collect { case x: Em => x } shouldBe empty
        }
      }
      "P751-01C1-06 convert emphasis code and inline/reference links with titles" in {
        Given("nested emphasis, literal inline code and resolved link references")
        val source = "**strong *inside*** and `include::x[] // <1>` [site:[literal]](guide.md \"Inline title\") [reference][ref]\n\n[ref]: https://example.invalid/path \"Reference title\""
        When("the Markdown inlines are interpreted")
        val document = _parse(source)
        Then("typed markup and literal hyperlink children, destinations and titles survive")
        _nodes(document).collect { case x: Strong => x }.head.contents.last shouldBe an[Em]
        _nodes(document).collect { case x: Code => _text(x) } shouldBe List("include::x[] // <1>")
        val links = _nodes(document).collect { case x: Hyperlink => x }
        links.map(_.href.toString) shouldBe List("guide.md", "https://example.invalid/path")
        links.map(x => _text(x)) shouldBe List("site:[literal]", "reference")
        links.map(_.title.map(_.as(java.util.Locale.ENGLISH))) shouldBe
          List(Some("Inline title"), Some("Reference title"))
        document.toString should include("<strong>strong <em>inside</em></strong>")
      }
    }
  }

  "Markdown blocks" should {
    "retain structured blocks" which {
      "P751-01C1-07 preserve nested lists and nondefault ordered start" in {
        Given("an ordered list starting at three with a nested unordered list")
        val source = "3. first\n   - nested\n   - second nested\n4. second\n\n- outer\n  1. inner\n  2. next"
        When("the Markdown lists are interpreted")
        val document = _parse(source)
        Then("ordered start, list items and both nesting directions survive")
        val ordered = document.body.contents.head.asInstanceOf[Ol]
        ordered.attributes.get("start") shouldBe Some("3")
        ordered.contents should have size 2
        ordered.contents.head.contents.collect { case x: Ul => x }.head.contents should have size 2
        val unordered = document.body.contents.last.asInstanceOf[Ul]
        unordered.contents.head.contents.collect { case x: Ol => x }.head.contents should have size 2
        document.toString should include("<ol start=\"3\">")
      }
      "P751-01C1-08 preserve quotes thematic breaks hard breaks and soft breaks" in {
        Given("a blockquote with a nested heading and hard and soft line breaks")
        val source = "> # Inside quote\n>\n> quoted  \n> hard\n> soft\n\n---\n\nOutside."
        When("the Markdown blocks are interpreted")
        val document = _parse(source)
        Then("the quote stays a typed quote and its heading stays inside it")
        document.body.contents.head shouldBe a[Quotation.SimpleQuote]
        document.body.contents(1) shouldBe a[HorizontalRule]
        _nodes(document.body).collect { case x: Section => x } shouldBe empty
        _nodes(document.body).collect { case x: Html5 if x.name == "h1" => x } should have size 1
        _nodes(document.body).collect { case x: Html5Inline if x.name == "br" => x } should have size 1
        _text(document.body) should include("hard\nsoft")
        document.head.metadata.title shouldBe empty
      }
      "P751-01C1-09 preserve fenced and indented code literal whitespace directives and callouts" in {
        Given("fenced Scala and indented code containing Dox directives and a callout marker")
        val literal = "  include::missing.dox[]\nsite:[guide] bib:[entry]\nval x = 1 // <1>\n"
        val source = "```scala\n" + literal + "```\n\n    indented\n      two spaces\n    include::missing[]\n"
        When("the code blocks are interpreted as Markdown")
        val document = _parse(source)
        Then("pre/code nodes retain exact literal contents and the language class without Program extraction")
        val blocks = document.body.contents.collect { case x: Html5 if x.name == "pre" => x }
        blocks should have size 2
        val code = blocks.head.contents.head.asInstanceOf[Code]
        code.attributes.get("class") shouldBe Some("language-scala")
        _text(code) shouldBe literal
        _text(blocks.last) shouldBe "indented\n  two spaces\ninclude::missing[]\n"
        _nodes(document).collect { case x: Program => x } shouldBe empty
        document.toString should include("// &lt;1&gt;")
        document.toString should include("class=\"language-scala\"")
      }
      "P751-01C1-10 preserve GFM table compartments alignment markup and links" in {
        Given("a three-column GFM table aligned left, center and right")
        val source = "| *Left* | Center | Right |\n| :--- | :---: | ---: |\n| [link](guide.md) | **bold** | `bib:[x]` |"
        When("the GFM table is interpreted")
        val document = _parse(source)
        Then("typed table rows retain alignment and converted inline contents")
        val table = document.body.contents.head.asInstanceOf[Table]
        val fields = table.head.get.records.head.fields
        fields.map(_.attributes.get("align")) shouldBe List(Some("left"), Some("center"), Some("right"))
        table.body.records.head.fields.map(_.attributes.get("align")) shouldBe fields.map(_.attributes.get("align"))
        fields.head.asInstanceOf[TH].contents.head shouldBe an[Em]
        _nodes(table).collect { case x: Hyperlink => x.href.toString } shouldBe List("guide.md")
        _nodes(table).collect { case x: Code => _text(x) } shouldBe List("bib:[x]")
        document.toString should include("<thead>")
        document.toString should include("<tbody>")
      }
    }
    "retain raw HTML meaning" which {
      "P751-01C1-11 preserve raw block and inline elements attributes and literal data" in {
        Given("raw HTML containers, comments, script/style data and inline markup")
        val source = "<div data-key=\"kept\"><span title=\"literal\">include::x[]</span><!-- omitted --><script>if (a < b) { x(); }</script><style>.x { color: red; }</style></div>\n\nText <mark data-note=\"yes\">bib:[literal]</mark>."
        When("the raw HTML emitted by the Markdown AST is converted")
        val document = _parse(source)
        Then("raw DOM meaning survives, comments are omitted and data is retained without execution")
        val block = document.body.contents.head.asInstanceOf[Html5]
        block.name shouldBe "div"
        block.attributes.get("data-key") shouldBe Some("kept")
        _nodes(block).collect { case x: Html5Inline if x.name == "span" => x.attributes.get("title") } shouldBe List(Some("literal"))
        _text(block) should include("if (a < b) { x(); }")
        _text(block) should include(".x { color: red; }")
        _text(document) should not include "omitted"
        _nodes(document).collect { case x: Html5Inline if x.name == "mark" => x.attributes.get("data-note") } shouldBe List(Some("yes"))
      }
      "P751-01C1-12 distinguish raw images from Markdown images by their rendered origin" in {
        Given("raw images surrounding Markdown images with repeated destinations and marker-looking user attributes")
        val config = Dox2Parser.Config.markdown._with_virtual_resource_parent("articles")
        val source = "<img src=\"../raw.png\" alt=\"raw\" data-smartdox-markdown-image=\"true\"> ![**Markdown**](images/diagram.png \"MD title\") <img src=\"images/diagram.png\" alt=\"same path\" data-origin=\"markdown\"> ![second](images/second.png)"
        When("the document is interpreted with a virtual Markdown image origin")
        val document = MarkdownDocumentParser.parse(config, source)
        Then("only actual Markdown Image nodes use resource admission and raw image attributes stay intact")
        val raw = _nodes(document).collect { case x: Html5Inline if x.name == "img" => x }
        raw should have size 2
        raw.head.attributes.get("src") shouldBe Some("../raw.png")
        raw.head.attributes.get("data-smartdox-markdown-image") shouldBe Some("true")
        raw.last.attributes.get("data-origin") shouldBe Some("markdown")
        val images = _nodes(document).collect { case x: ReferenceImg => x }
        images.map(_.src.toString) shouldBe List("images/diagram.png", "images/second.png")
        images.head.alt shouldBe Some("Markdown")
        images.head.attributes.get("title") shouldBe Some("MD title")
      }
      "P751-01C1-18 preserve raw quote attributes and Unicode paragraph contents alongside typed Markdown quotes" in {
        forAll(Gen.oneOf("日本語 🌱", "Ελληνικά", "العربية"), Gen.oneOf("note", "note important", "日本語-note")) { (unicode, cssclass) =>
          Given("a raw blockquote with an authored class and literal Unicode paragraph, followed by a Markdown quote")
          val literal = unicode + " include::missing[] bib:[literal]"
          val source = "<blockquote class=\"" + cssclass + "\"><p>" + literal + "</p></blockquote>\n\n> Markdown quote."
          When("both entry points interpret the raw and Markdown quotes")
          val document = _parse(source)
          val consequence = MarkdownDocumentParser.parseC(Dox2Parser.Config.markdown, source)
          Then("the raw quote retains its class, paragraph and literal text while the Markdown quote remains typed")
          consequence.isSuccess shouldBe true
          consequence.toOption shouldBe Some(document)
          val quote = document.body.contents.head.asInstanceOf[Html5]
          quote.name shouldBe "blockquote"
          quote.attributes.get("class") shouldBe Some(cssclass)
          val paragraph = quote.contents.head.asInstanceOf[Html5]
          paragraph.name shouldBe "p"
          _text(paragraph) shouldBe literal
          document.body.contents.last shouldBe a[Quotation.SimpleQuote]
          document.toString should include("class=\"" + cssclass + "\"")
          document.toString should include(s"<p>$literal</p>")
        }
      }
      "P751-01C1-19 preserve raw rule attributes alongside a typed Markdown thematic break" in {
        Given("a raw hr with an id, class and data attribute followed by an ordinary Markdown thematic break")
        val source = "<hr id=\"break\" class=\"wide\" data-note=\"kept\">\n\n---\n\nOutside."
        When("both entry points interpret the raw and Markdown breaks")
        val document = _parse(source)
        val consequence = MarkdownDocumentParser.parseC(Dox2Parser.Config.markdown, source)
        Then("the raw rule keeps every authored attribute and the Markdown break remains HorizontalRule")
        consequence.isSuccess shouldBe true
        _nodes(consequence.toOption.get).map(x => (x.getClass.getName, x.attributes.toVector, x.location, x.toString)) shouldBe
          _nodes(document).map(x => (x.getClass.getName, x.attributes.toVector, x.location, x.toString))
        val rule = document.body.contents.head.asInstanceOf[Html5]
        rule.name shouldBe "hr"
        rule.attributes.toVector.toMap shouldBe Map("id" -> "break", "class" -> "wide", "data-note" -> "kept")
        rule.contents shouldBe empty
        document.body.contents(1) shouldBe a[HorizontalRule]
        document.toString should include("id=\"break\"")
        document.toString should include("class=\"wide\"")
        document.toString should include("data-note=\"kept\"")
      }
      "P751-01C1-20 preserve raw table compartments attributes and child order alongside a typed GFM table" in {
        Given("a raw table with caption, body and footer attributes and an ordinary GFM table")
        val source = "<table id=\"table\" class=\"ledger\"><caption data-caption=\"kept\">Kept caption</caption><tbody data-body=\"kept\"><tr data-row=\"body\"><td class=\"body-cell\">Body</td></tr></tbody><tfoot data-footer=\"kept\"><tr data-row=\"footer\"><td class=\"footer-cell\">Kept footer</td></tr></tfoot></table>\n\n| Heading |\n| --- |\n| Markdown body |"
        When("both entry points interpret the raw and GFM tables")
        val document = _parse(source)
        val consequence = MarkdownDocumentParser.parseC(Dox2Parser.Config.markdown, source)
        Then("the raw table keeps all compartments, cells and attributes in DOM order while GFM stays typed")
        consequence.isSuccess shouldBe true
        consequence.toOption shouldBe Some(document)
        val table = document.body.contents.head.asInstanceOf[Html5]
        table.name shouldBe "table"
        table.attributes.toVector.toMap shouldBe Map("id" -> "table", "class" -> "ledger")
        val compartments = table.contents.collect { case x: Html5 => x }
        compartments.map(_.name) shouldBe List("caption", "tbody", "tfoot")
        compartments.map(_text) shouldBe List("Kept caption", "Body", "Kept footer")
        compartments.map(_.attributes.toVector.toMap) shouldBe List(
          Map("data-caption" -> "kept"), Map("data-body" -> "kept"), Map("data-footer" -> "kept")
        )
        val rows = _nodes(table).collect { case x: Html5 if x.name == "tr" => x }
        rows.map(_.attributes.get("data-row")) shouldBe List(Some("body"), Some("footer"))
        val cells = _nodes(table).collect { case x: Html5 if x.name == "td" => x }
        cells.map(_.attributes.get("class")) shouldBe List(Some("body-cell"), Some("footer-cell"))
        cells.map(_text) shouldBe List("Body", "Kept footer")
        document.body.contents.last shouldBe a[Table]
        val rendered = table.toString
        rendered should include("data-caption=\"kept\"")
        rendered should include("class=\"footer-cell\"")
        rendered.indexOf("<caption") should be < rendered.indexOf("<tbody")
        rendered.indexOf("<tbody") should be < rendered.indexOf("<tfoot")
      }
      "P751-01C1-21 preserve a raw anchor's literal spaced href alongside a typed Markdown hyperlink" in {
        Given("a raw inline anchor whose href contains a space and an ordinary Markdown hyperlink")
        val source = "Text <a href=\"two words.html\" title=\"Raw title\">Link</a> and [Markdown](guide.md)."
        When("plain and consequence entry points interpret both anchors")
        val document = _parse(source)
        val consequence = MarkdownDocumentParser.parseC(Dox2Parser.Config.markdown, source)
        Then("both succeed with the raw href unchanged, raw children retained and the Markdown link typed")
        consequence.isSuccess shouldBe true
        consequence.toOption shouldBe Some(document)
        val anchor = _nodes(document).collectFirst { case x: Html5Inline if x.name == "a" => x }.get
        anchor.attributes.get("href") shouldBe Some("two words.html")
        anchor.attributes.get("title") shouldBe Some("Raw title")
        anchor.contents shouldBe List(Text("Link"))
        _nodes(document).collect { case x: Hyperlink => x.href.toString -> _text(x) } shouldBe List("guide.md" -> "Markdown")
        document.toString should include("href=\"two words.html\"")
        document.toString should include(">Link</a>")
      }
      "P751-01C1-22 preserve raw flow-content anchor paragraphs through both entry points" in {
        Given("a raw div containing an anchor with a paragraph child and authored attributes")
        val source = "<div><a href=\"guide.html\" data-link=\"flow\"><p class=\"inside\">Block link</p></a></div>"
        When("plain and consequence entry points interpret the flow-content anchor")
        val document = _parse(source)
        val consequence = MarkdownDocumentParser.parseC(Dox2Parser.Config.markdown, source)
        Then("both succeed and retain the anchor's paragraph, content and attributes without inline coercion")
        consequence.isSuccess shouldBe true
        consequence.toOption shouldBe Some(document)
        val container = document.body.contents.head.asInstanceOf[Html5]
        container.name shouldBe "div"
        val anchor = container.contents.head.asInstanceOf[Html5Inline]
        anchor.name shouldBe "a"
        anchor.attributes.toVector.toMap shouldBe Map("href" -> "guide.html", "data-link" -> "flow")
        val paragraph = anchor.contents.head.asInstanceOf[Html5]
        paragraph.name shouldBe "p"
        paragraph.attributes.get("class") shouldBe Some("inside")
        paragraph.contents shouldBe List(Text("Block link"))
        document.toString should include("<p class=\"inside\">Block link</p></a>")
      }
      "P751-01C1-23 retain implied raw table bodies and their ordered rows and cells" in {
        Given("a raw table with two attributed rows and cells but no explicit tbody")
        val source = "<table data-table=\"implicit\"><tr id=\"first\"><td data-cell=\"one\">One</td></tr><tr id=\"second\"><td data-cell=\"two\">Two</td></tr></table>"
        When("both entry points interpret the raw table normalized by the HTML fragment parser")
        val document = _parse(source)
        val consequence = MarkdownDocumentParser.parseC(Dox2Parser.Config.markdown, source)
        Then("the implied tbody keeps both rows, their cells, attributes and content in order")
        consequence.isSuccess shouldBe true
        consequence.toOption shouldBe Some(document)
        val table = document.body.contents.head.asInstanceOf[Html5]
        table.name shouldBe "table"
        table.attributes.get("data-table") shouldBe Some("implicit")
        val body = table.contents.head.asInstanceOf[Html5]
        body.name shouldBe "tbody"
        val rows = body.contents.map(_.asInstanceOf[Html5])
        rows.map(_.name) shouldBe List("tr", "tr")
        rows.map(_.attributes.get("id")) shouldBe List(Some("first"), Some("second"))
        val cells = rows.map(_.contents.head.asInstanceOf[Html5])
        cells.map(_.name) shouldBe List("td", "td")
        cells.map(_.attributes.get("data-cell")) shouldBe List(Some("one"), Some("two"))
        cells.map(_text) shouldBe List("One", "Two")
        val rendered = table.toString
        rendered should include("<tbody>")
        rendered should include("data-cell=\"one\"")
        rendered should include("data-cell=\"two\"")
        rendered.indexOf(">One</td>") should be < rendered.indexOf(">Two</td>")
      }
      "P751-01C1-24 admit a Markdown image inside a raw anchor while retaining an adjacent raw image" in {
        Given("a virtual source origin, a Markdown image nested in a raw inline anchor and an adjacent raw image")
        val inline = DoxInlineParser.Config.markdown._with_virtual_resource_parent("articles/guides").withLocation(ParseLocation.create(24))
        val config = Dox2Parser.Config.markdown.withInlineConfig(inline).withPathname("articles/guides/article.md")
        val source = "Text <a href=\"guide.html\" data-link=\"wrapper\">![日本語 *alt*](images/../images/diagram.png \"MD title\")</a> <img src=\"../raw.png\" alt=\"raw\" data-origin=\"markdown\">."
        When("both entry points interpret the raw wrapper and both image origins")
        val document = MarkdownDocumentParser.parse(config, source)
        val consequence = MarkdownDocumentParser.parseC(config, source)
        Then("the nested Markdown image remains admitted ReferenceImg while the raw image retains its literal attributes")
        consequence.isSuccess shouldBe true
        consequence.toOption shouldBe Some(document)
        val anchor = _nodes(document).collectFirst { case x: Html5Inline if x.name == "a" => x }.get
        anchor.attributes.get("data-link") shouldBe Some("wrapper")
        val image = anchor.contents.head.asInstanceOf[ReferenceImg]
        image.src.toString shouldBe "images/diagram.png"
        image.alt shouldBe Some("日本語 alt")
        image.attributes.get("title") shouldBe Some("MD title")
        image.location shouldBe inline.location
        _nodes(document).collect { case x: ReferenceImg => x } shouldBe List(image)
        val raw = _nodes(document).collect { case x: Html5Inline if x.name == "img" => x }
        raw should have size 1
        raw.head.attributes.toVector.toMap shouldBe Map("src" -> "../raw.png", "alt" -> "raw", "data-origin" -> "markdown")
        document.toString should include("src=\"images/diagram.png\"")
        document.toString should include("src=\"../raw.png\"")
        document.toString should include("data-origin=\"markdown\"")
      }
    }
  }

  "Markdown image origins" should {
    "share established resource admission" which {
      "P751-01C1-13 normalize physical image paths and retain location alt and title" in {
        Given("a scoped physical resource root and source-identified location")
        _with_fixture { root =>
          val location = ParseLocation.create(13)
          val inline = DoxInlineParser.Config.markdown.withResourceRoot(root).withLocation(location)
          val config = Dox2Parser.Config.markdown.withInlineConfig(inline).copy(file = Some(root.resolve("article.md").toFile.toURI))
          When("an image with internal dot segments is interpreted")
          val document = MarkdownDocumentParser.parse(config, "![日本語 *alt*](images/../images/diagram.png \"図\")")
          Then("the shared image normalizes against that root and preserves observable attributes and location")
          val image = _nodes(document).collectFirst { case x: ReferenceImg => x }.get
          image.src.toString shouldBe "images/diagram.png"
          image.alt shouldBe Some("日本語 alt")
          image.attributes.get("title") shouldBe Some("図")
          image.location shouldBe inline.location
          document.location shouldBe inline.location
        }
      }
      "P751-01C1-14 admit resolved reference images under the virtual source parent" in {
        Given("a virtual source origin and a Markdown reference image")
        val config = Dox2Parser.Config.markdown._with_virtual_resource_parent("articles/guides").withPathname("articles/guides/article.md")
        val source = "![reference **alt**][diagram]\n\n[diagram]: images/../images/diagram.png \"Reference title\""
        When("the reference image is interpreted")
        val document = MarkdownDocumentParser.parse(config, source)
        Then("the resolved AST destination enters shared normalization with plain alt and title")
        val image = _nodes(document).collectFirst { case x: ReferenceImg => x }.get
        image.src.toString shouldBe "images/diagram.png"
        image.alt shouldBe Some("reference alt")
        image.attributes.get("title") shouldBe Some("Reference title")
      }
      "P751-01C1-15 reject unsupported resources and malformed destinations with stable diagnostics" in {
        Given("a virtual resource origin carrying an existing location and rejected image destinations")
        val inline = DoxInlineParser.Config.markdown._with_virtual_resource_parent("articles").withLocation(ParseLocation.create(17))
        val config = Dox2Parser.Config.markdown.withInlineConfig(inline).withPathname("articles/article.md")
        val rejected = List(
          "../outside.png" -> "image.markdown.unsupported-resource",
          "/absolute.png" -> "image.markdown.unsupported-resource",
          "https://example.invalid/image.png" -> "image.markdown.unsupported-resource",
          "images/file.txt" -> "image.markdown.unsupported-resource",
          "images/diagram.png?size=1" -> "image.markdown.malformed",
          "images/diagram.png#part" -> "image.markdown.malformed"
        )
        When("plain parse and consequence parse interpret each rejected Markdown image")
        val outcomes = rejected.map { case (path, category) =>
          val source = s"![diagram]($path)"
          val failure = intercept[IllegalArgumentException] { MarkdownDocumentParser.parse(config, source) }
          (path, category, failure, MarkdownDocumentParser.parseC(config, source))
        }
        Then("plain diagnostics keep category, location, image source and raw path while parseC returns failure")
        outcomes.foreach { case (path, category, failure, consequence) =>
          failure.getMessage should include(category)
          failure.getMessage should include(s"location=${inline.location.get}")
          failure.getMessage should include(s"source=![diagram]($path)")
          failure.getMessage should include(s"raw-path=$path")
          consequence.isError shouldBe true
        }
      }
      "P751-01C1-16 reject a physical escape and an absent resource origin" in {
        Given("a scoped physical root and a rootless Markdown configuration")
        _with_fixture { root =>
          val physical = Dox2Parser.Config.markdown.withResourceRoot(root)
          When("a physical escape and a rootless relative image are interpreted")
          val escape = intercept[IllegalArgumentException] { MarkdownDocumentParser.parse(physical, "![escape](../outside.png)") }
          val absent = intercept[IllegalArgumentException] { _parse("![rootless](images/diagram.png)") }
          Then("both retain the existing unsupported-resource category without consulting cwd")
          escape.getMessage should include("image.markdown.unsupported-resource")
          absent.getMessage shouldBe "image.markdown.unsupported-resource: location=<absent> source=![rootless](images/diagram.png) raw-path=images/diagram.png"
        }
      }
      "P751-01C1-17 return the same shared document through both successful entry points" in {
        Given("ordinary Markdown with no resource-dependent images")
        val source = "# Title\n\nA **shared** document."
        When("plain and consequence entry points interpret it")
        val plain = _parse(source)
        val consequence = MarkdownDocumentParser.parseC(Dox2Parser.Config.markdown, source)
        Then("both expose the same shared Document and rendered content")
        consequence.isSuccess shouldBe true
        consequence.toOption shouldBe Some(plain)
        plain.toString should include("<strong>shared</strong>")
      }
    }
  }

  private def _parse(source: String): Document = MarkdownDocumentParser.parse(Dox2Parser.Config.markdown, source)

  private def _nodes(dox: Dox): List[Dox] =
    dox :: (dox match {
      case x: Section => x.title.flatMap(_nodes) ::: x.contents.flatMap(_nodes)
      case x => x.elements.flatMap(_nodes)
    })

  private def _text(dox: Dox): String = dox match {
    case x: Text => x.contents
    case x => x.elements.map(_text).mkString
  }

  private def _with_fixture(body: Path => Unit): Unit = {
    val target = Files.createDirectories(Paths.get("target", "markdown-document-parser").toAbsolutePath)
    val root = Files.createTempDirectory(target, "image-origin-")
    try body(root) finally {
      val paths = Files.walk(root)
      try paths.iterator.asScala.toVector.sortBy(_.getNameCount).reverse.foreach(x => Files.delete(x))
      finally paths.close()
    }
  }
}
