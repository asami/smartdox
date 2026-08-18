package org.smartdox.parser

import scalaz._, Scalaz._
import java.io.{ByteArrayOutputStream, PrintStream}
import org.scalatest.GivenWhenThen
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.parser.{LogicalLine, LogicalParagraph, ParseLocation}
import org.smartdox.{Dfn, Document, Dox, Hyperlink, InlineMacro, NoTerm, ReferenceImg, Span, Term}

/*
 * @since   Oct. 14, 2018
 *  version Nov. 12, 2018
 *  version Dec. 31, 2018
 *  version Sep.  5, 2024
 *  version Aug. 16, 2025
 *  version Apr. 19, 2026
 *  version Jun. 23, 2026
 *  version Jul.  6, 2026
 * @version Aug. 19, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class Dox2ParserSpec extends AnyWordSpec with Matchers with ScalazMatchers with GivenWhenThen with UseDox2Parser {
  "inline tag pipeline" should {
    "defer attributed generic and RDF tags to DoxInlineParser" in {
      Given("a SmartDox document containing generic and RDF inline tags")
      val source =
        """Pipeline
          |========
          |
          |# Terms
          |
          |<span lang="ja">日本語</span>
          |<dfn about="https://example.com/term/entity" id="entity-anchor">Entity</dfn>
          |<term ref="https://example.com/term/entity" form="canonical">Entity</term>
          |""".stripMargin

      When("Dox2Parser parses the document through its SmartDox configuration")
      val document = Dox2Parser.parse(Dox2Parser.Config.smartdox, source).asInstanceOf[Document]
      val nodes = _nodes(document)

      Then("the inline parser retains each tag and its attributes")
      val span = nodes.collectFirst { case m: Span => m }.get
      val definition = nodes.collectFirst { case m: Dfn => m }.get
      val term = nodes.collectFirst { case m: Term => m }.get
      span.attribute("lang") shouldBe Some("ja")
      definition.attribute("about") shouldBe Some("https://example.com/term/entity")
      definition.attribute("id") shouldBe Some("entity-anchor")
      term.attribute("ref") shouldBe Some("https://example.com/term/entity")
      term.attribute("form") shouldBe Some("canonical")
    }

    "attach a supplied logical-line location to common and RDF tags" in {
      Given("a logical paragraph with an explicit source location and common plus RDF tags")
      val location = ParseLocation.create(42)
      val paragraph = LogicalParagraph(
        "<span lang=\"ja\">日本語</span><dfn about=\"https://example.com/term/entity\">Entity</dfn><term ref=\"https://example.com/term/entity\">Entity</term><noterm>literal</noterm>",
        location
      )

      When("Dox2Parser parses the logical paragraph through its SmartDox configuration")
      val document = Dox2Parser.parse(Dox2Parser.Config.smartdox, paragraph).asInstanceOf[Document]
      val nodes = _nodes(document)
      val span = nodes.collectFirst { case m: Span => m }.get
      val definition = nodes.collectFirst { case m: Dfn => m }.get
      val term = nodes.collectFirst { case m: Term => m }.get
      val noterm = nodes.collectFirst { case m: NoTerm => m }.get

      Then("every parsed tag retains the supplied logical-line location without changing its kind")
      span.location shouldBe Some(location)
      definition.location shouldBe Some(location)
      term.location shouldBe Some(location)
      noterm.location shouldBe Some(location)
    }

    "attach a supplied logical-line location to site and generic inline macros" in {
      Given("source-located site and generic inline macro inputs")
      val location = ParseLocation.create(47)
      val siteinput = LogicalLine("site:[overview.dox]", location)
      val genericinput = LogicalLine("prefix generic:[opaque]", location)

      When("DoxInlineParser parses both inputs through its SmartDox configuration")
      val site = DoxInlineParser.parse(
        DoxInlineParser.Config.smartdox.withLocation(siteinput.location),
        siteinput.text
      ).asInstanceOf[Hyperlink]
      val generic = _nodes(DoxInlineParser.parse(
        DoxInlineParser.Config.smartdox.withLocation(genericinput.location),
        genericinput.text
      )).collectFirst { case m: InlineMacro => m }.get

      Then("both concrete macro results retain the supplied location and original payload")
      site.location shouldBe Some(location)
      site.href.toString shouldBe "overview.dox"
      site.contents.map(_.toText).mkString shouldBe "overview.dox"
      generic.location shouldBe Some(location)
      generic.name shouldBe "generic"
      generic.contents shouldBe "opaque"
    }

    "retain logical-line locations across table list quote annotation and image paths" in {
      Given("source-located inputs for every DoxLines inline entry path")
      val tablelocation = ParseLocation.create(51)
      val listlocation = ParseLocation.create(52)
      val quotelocation = ParseLocation.create(53)
      val annotationlocation = ParseLocation.create(54)
      val imagelocation = ParseLocation.create(55)

      When("DoxLinesParser parses each paragraph through its SmartDox configuration")
      val table = DoxLinesParser.parse(
        DoxLinesParser.Config.smartdox,
        LogicalParagraph("| <span lang=\"ja\">表</span> |", tablelocation)
      )
      val list = DoxLinesParser.parse(
        DoxLinesParser.Config.smartdox,
        LogicalParagraph("- <term ref=\"https://example.com/term/list\">List</term>", listlocation)
      )
      val quote = DoxLinesParser.parse(
        DoxLinesParser.Config.smartdox,
        LogicalParagraph("> <dfn about=\"https://example.com/term/quote\">Quote</dfn>", quotelocation)
      )
      val annotation = DoxLinesParser.AnnotationMark.get(
        DoxLinesParser.Config.smartdox,
        LogicalLine("#+TITLE: <span lang=\"ja\">題名</span>", annotationlocation)
      ).collect {
        case DoxLinesParser.TitleAnnotation(title, _) => title
      }.get
      val image = DoxLinesParser.parse(
        DoxLinesParser.Config.smartdox,
        LogicalParagraph("[[image/location.png]]", imagelocation)
      )

      Then("the parsed tags retain their originating logical-line locations")
      _nodes(table).collectFirst { case m: Span => m }.get.location shouldBe Some(tablelocation)
      _nodes(list).collectFirst { case m: Term => m }.get.location shouldBe Some(listlocation)
      _nodes(quote).collectFirst { case m: Dfn => m }.get.location shouldBe Some(quotelocation)
      _nodes(annotation).collectFirst { case m: Span => m }.get.location shouldBe Some(annotationlocation)
      _nodes(image).collectFirst { case m: ReferenceImg => m }.get.location shouldBe Some(imagelocation)
    }
  }

  "HEAD section" should {
    "normalize SmartDox HEAD key-value shorthand into HOCON metadata" in {
      Given("a SmartDox document whose HEAD contains unquoted key-value metadata")
      When("Dox2Parser parses the SmartDox document")
      val dox = parse_dox("""業務報告
===

# HEAD

published_at=2026-06-02
organization=知識基盤開発室
author=山田 太郎

# 本文

本文です。
""").asInstanceOf[Document]
      val meta = dox.head.metadata
      Then("the metadata values are available in the requested locale")
      meta.getPublishedString(java.util.Locale.JAPANESE) should be (Some("2026-06-02"))
      meta.getOrganizationString(java.util.Locale.JAPANESE) should be (Some("知識基盤開発室"))
      meta.getAuthorString(java.util.Locale.JAPANESE) should be (Some("山田 太郎"))
    }

    "keep regular HOCON metadata support beside the shorthand" in {
      Given("a SmartDox document whose HEAD contains quoted HOCON metadata")
      When("Dox2Parser parses the SmartDox document")
      val dox = parse_dox("""業務報告
===

# HEAD

published_at="2026-06-02"
organization="知識基盤開発室"
author="山田 太郎"

# 本文

本文です。
""").asInstanceOf[Document]
      val meta = dox.head.metadata
      Then("the metadata values are preserved beside the shorthand form")
      meta.getPublishedString(java.util.Locale.JAPANESE) should be (Some("2026-06-02"))
      meta.getOrganizationString(java.util.Locale.JAPANESE) should be (Some("知識基盤開発室"))
      meta.getAuthorString(java.util.Locale.JAPANESE) should be (Some("山田 太郎"))
    }

    "parse only the leading metadata paragraph and keep descriptive child sections" in {
      Given("a HEAD section with a leading metadata paragraph and descriptive children")
      val stderr = new ByteArrayOutputStream()
      When("Dox2Parser parses the SmartDox document")
      val dox = Console.withErr(new PrintStream(stderr, true, "UTF-8")) {
        parse_dox("""業務報告
===

# HEAD

status=work-in-progress
published_at=2026-06-08

## SUMMARY

日本語の概要です。

## LEAD

詳しい導入文です。

# 本文

本文です。
""").asInstanceOf[Document]
      }
      val meta = dox.head.metadata
      Then("only the leading paragraph becomes metadata and child sections remain content")
      stderr.toString("UTF-8") should not include ("SmartDox HEAD metadata parse error:")
      meta.getPublishedString(java.util.Locale.JAPANESE) should be (Some("2026-06-08"))
      meta.getEffectiveSummaryString(java.util.Locale.JAPANESE) should be (Some("日本語の概要です。"))
      dox.toString should include ("本文です。")
    }

    "parse SmartDox HEAD metadata inside Markdown documents" in {
      Given("a Markdown document with a SmartDox HEAD metadata section")
      val stderr = new ByteArrayOutputStream()
      When("Dox2Parser parses the document using its Markdown filename")
      val dox = Console.withErr(new PrintStream(stderr, true, "UTF-8")) {
        Dox2Parser.parseWithFilename("published.md", """# HEAD

status=published
published_at=2026-06-23
author=山田 太郎

## SUMMARY

Markdown summary from SmartDox metadata section.

# Body

Markdown body.
""").asInstanceOf[Document]
      }
      val meta = dox.head.metadata
      Then("metadata is interpreted while the body remains Markdown content")
      stderr.toString("UTF-8") should not include ("SmartDox HEAD metadata parse error:")
      meta.status should be (org.smartdox.metadata.DocumentMetaData.Status.Published)
      meta.getPublishedString(java.util.Locale.ENGLISH) should be (Some("2026-06-23"))
      meta.getAuthorString(java.util.Locale.JAPANESE) should be (Some("山田 太郎"))
      meta.getEffectiveSummaryString(java.util.Locale.ENGLISH) should be (Some("Markdown summary from SmartDox metadata section."))
      dox.toString should include ("Markdown body.")
      dox.toString should not include ("status=published")
    }

    "parse leading metadata paragraph through filename based SmartDox parsing" in {
      Given("a filename-addressed SmartDox document with leading HEAD metadata")
      val stderr = new ByteArrayOutputStream()
      When("Dox2Parser parses the document using its SmartDox filename")
      val dox = Console.withErr(new PrintStream(stderr, true, "UTF-8")) {
        Dox2Parser.parseWithFilename("published.dox", """Published Article
=================

# HEAD

status=published
published_at=2026-06-08
title_image="https://example.com/image.jpg?q=80&w=1200"

## SUMMARY

Published summary.

# Body

Published body.
""").asInstanceOf[Document]
      }
      val meta = dox.head.metadata
      Then("publication metadata and body content are both retained")
      stderr.toString("UTF-8") should not include ("SmartDox HEAD metadata parse error:")
      meta.status should be (org.smartdox.metadata.DocumentMetaData.Status.Published)
      meta.getPublishedString(java.util.Locale.ENGLISH) should be (Some("2026-06-08"))
      meta.titleImage.map(_.toString) should be (Some("https://example.com/image.jpg?q=80&w=1200"))
      meta.getEffectiveSummaryString(java.util.Locale.ENGLISH) should be (Some("Published summary."))
      dox.toString should include ("Published body.")
    }

    "report explicit HEAD metadata parse errors" in {
      Given("a SmartDox HEAD section containing malformed metadata")
      val stderr = new ByteArrayOutputStream()
      When("Dox2Parser parses the malformed document")
      val dox = Console.withErr(new PrintStream(stderr, true, "UTF-8")) {
        parse_dox("""業務報告
===

# HEAD

{

# 本文

本文です。
""").asInstanceOf[Document]
      }
      val message = "SmartDox HEAD metadata parse error:"
      Then("the parser exposes the diagnostic while retaining body content")
      stderr.toString("UTF-8") should include (message)
      dox.toString should include (message)
      dox.toString should include ("本文です。")
    }

    "leave malformed metadata-looking text as body when HEAD is absent" in {
      Given("a SmartDox document without a HEAD section that contains malformed metadata-looking text")
      When("Dox2Parser parses the document")
      val dox = parse_dox("""業務報告
===

{

本文です。
""").asInstanceOf[Document]
      Then("the text remains body content and does not become metadata")
      dox.head.metadata.getPublishedString(java.util.Locale.JAPANESE) should be (None)
      dox.toString should include ("{")
      dox.toString should include ("本文です。")
    }
  }

  "Foundation" should {
    "simple" which {
    //   val in = "* OK"
    //   val out = "<!DOCTYPE html><html><head/><body><section><h2>OK</h2></section></body></html>"
    //   "plain" in {
    //     parse_orgmode(in, out)
    //   }
    //   // "scalaz" in {
    //   //   parse_orgmode_z(in, out)
    //   // }
    //   // "short" in {
    //   //   parse_orgmode("* OK", out)
    //   // }
    // }
    // "nest" which {
    //   "first/second" in {
    //     parse_orgmode("* First\n** Second\n",
    //         "<!DOCTYPE html><html><head/><body><section><h2>First</h2><section><h3>Second</h3></section></section></body></html>")
    //   }
    //   "first,contents/second,contents" in { // TODO bold option
    //     parse_orgmode("* First\n1st contents.\n** Second\n2nd *contents*.\n",
    //         "<!DOCTYPE html><html><head/><body><section><h2>First</h2><p>1st contents.</p><section><h3>Second</h3><p>2nd *contents*.</p></section></section></body></html>")
    //   }
    }
    "ul" which {
      "typical" in {
        // parse_orgmode("* First\n - first\n - second\n - third\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ul><li>first</li><li>second</li><li>third</li></ul></section></body></html>")
      }
      "nest" in {
        // parse_orgmode("* First\n - first\n  - first.first\n  - first.second\n - second\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ul><li>first<ul><li>first.first</li><li>first.second</li></ul></li><li>second</li></ul></section></body></html>")
      }
      "typical 2" in {
        Given("a flat unordered SmartDox list")
        When("the parser renders the list")
        Then("the resulting list structure matches the established behavior")
        parse_orgmode_simple_debug("- One\n- Two\n",
          """<ul><li>One</li><li>Two</li></ul>""")
      }
      "continue" in {
        Given("an unordered list item continued on an indented line")
        When("the parser renders the continued item")
        Then("the continuation remains part of that list item")
        parse_orgmode_simple_debug("- This is \n a pen.\n",
                             """<ul><li>This is a pen.</li></ul>""")
      }
      "continue 2" in {
        Given("a nested unordered list with continued content")
        When("the parser renders the nested list")
        Then("the nested item keeps its continued text")
        parse_orgmode_simple("- One\n - Two\n Two-One\n",
                             """<ul><li>One<ul><li>Two Two-One</li></ul></li></ul>""")
      }
      "continue 2 xx" in {
        Given("paragraphs surrounding a nested unordered list")
        When("the parser renders the document")
        Then("paragraph and list boundaries remain unchanged")
        parse_orgmode_simple_debug("abc\n\n- One\n - Two\n - Three\n\nxyz",
          """<p>abc</p><ul><li>One<ul><li>Two</li><li>Three</li></ul></li></ul><p>xyz</p>""")
      }
      "continue 2 x" in {
        Given("a nested unordered list with two child items")
        When("the parser renders the list")
        Then("the child items remain grouped beneath their parent")
        parse_orgmode_simple_debug("- One\n - Two\n - Three\n",
          """<ul><li>One<ul><li>Two</li><li>Three</li></ul></li></ul>""")
      }
    }
    "ol" which {
      "typical" in {
        // parse_orgmode("* First\n 1. first\n 2. second\n 3. third\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ol><li>first</li><li>second</li><li>third</li></ol></section></body></html>")
      }
      "nest" in {
        // parse_orgmode("* First\n 1. first\n  1. first.first\n  2. first.second\n 2. second\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><ol><li>first<ol><li>first.first</li><li>first.second</li></ol></li><li>second</li></ol></section></body></html>")
      }
    }
    "dl" which {
      "typical" in {
        // parse_orgmode("* First\n - first :: one\n - second :: two\n - third :: three\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><dl><dt>first</dt><dd>one</dd><dt>second</dt><dd>two</dd><dt>third</dt><dd>three</dd></dl></section></body></html>")
      }
    }
    "inline" which {
      "typical" in {
        // parse_orgmode_full("* First\n pre *bold* /italic/ _underline_ =code= ~pre~ +del+ post\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><p>pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post</p></section></body></html>")
      }
    }
    "inline xml" which {
      "typical" in {
        // parse_orgmode("* First\n pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post\n",
        //     "<!DOCTYPE html><html><head/><body><section><h2>First</h2><p>pre <b>bold</b> <i>italic</i> <u>underline</u> <code>code</code> <pre>pre</pre> <del>del</del> post</p></section></body></html>")
      }
      "= in code" in {
        // parse_orgmode_simple("""<code>(b >= 0).option(b.toString)</code>""",
        //     """<p><code>(b &gt;= 0).option(b.toString)</code></p>""")
      }
    }
//     "structure" which {
//       "empty" in {
//         parse_orgmode("",
//             "<!DOCTYPE html><html><head/><body/></html>")
//       }
//       "simple in top" in {
//         parse_orgmode("Hello SmartDox",
//             "<!DOCTYPE html><html><head/><body><p>Hello SmartDox</p></body></html>")
//       }
//       "multi line in top" in {
//         parse_orgmode("Hello\nSmartDox\n",
//             "<!DOCTYPE html><html><head/><body><p>Hello SmartDox</p></body></html>")
//       }
//       "auto title" in { FUTURE
//         parse_orgmode_auto_title("Hello SmartDox",
//             "<!DOCTYPE html><html><head><title>Hello SmartDox</title></head><body/></html>")
//       }
//       "auto title and body" in { FUTURE
//         // "Hello\nSmartDox\n"の場合の動きがorg-modeと異なるが、仕様としておく。
//         parse_orgmode_auto_title("Hello\n\nSmartDox\n",
//             "<!DOCTYPE html><html><head><title>Hello</title></head><body><p>SmartDox</p></body></html>")
//       }
//     }
//     "hyperlink" which {
//       "typical" in {
//         parse_orgmode("[[http://www.yahoo.com/][Yahoo]]",
//             """<!DOCTYPE html><html><head/><body><p><a href="http://www.yahoo.com/">Yahoo</a></p></body></html>""")
//       }
//       "simple" in {
//         parse_orgmode("[[http://www.yahoo.com/]]",
//             """<!DOCTYPE html><html><head/><body><p><a href="http://www.yahoo.com/">http://www.yahoo.com/</a></p></body></html>""")
//       }
//       "xml" in {
//         parse_orgmode("""<a href="http://www.yahoo.com/">Yahoo</a>""",
//             """<!DOCTYPE html><html><head/><body><p><a href="http://www.yahoo.com/">Yahoo</a></p></body></html>""")
//       }
//       "not hyperlink" in {
//         parse_orgmode("""[not link]""",
//             """<!DOCTYPE html><html><head/><body><p>[not link]</p></body></html>""")
//       }
//       "implicit hyperlink" in {
//         parse_orgmode("""http://www.yahoo.com/""",
//             """<!DOCTYPE html><html><head/><body><p><a href="http://www.yahoo.com/">http://www.yahoo.com/</a></p></body></html>""")
//       }
//     }
  }
  "Table" should {
    "table" which {
      val tabletypical = """|------
| one | two | three |
|----"""
      val tablesimple = """| one | two | three |"""
      val tablesimple2 = """| one | two | three """
      val tablemulti = """|------
| one | two | three |
| four | five | six |        
|----"""
      val tableheadertypical = """|-----
|h1|h2|h3|
|----
|one | two |three |
|four| five | six|
|---"""
      val tableheadersimple = """|h1|h2|h3|
|----
|one | two |three |
|four| five | six|"""
      val tablefootertypical = """|-----
|h1|h2|h3|
|----
|one | two |three |
|four| five | six|
|---
| sum1 |sum2 | sum3|
|---"""
      val tablefootersimple = """|h1|h2|h3|
|----
|one | two |three |
|four| five | six|
|---
| sum1 |sum2 | sum3|"""
      val tablecaption = """#+CAPTION: Title
#+LABEL: tablelabel
#+ATTR_HTML: width=80%
#+ATTR_LATEX: width=15cm
|-----
|h1|h2|h3|
|----
|one | two |three |
|four| five | six|
|---
| sum1 |sum2 | sum3|
|---"""
      val result = "<table><tbody><tr><td>one</td><td>two</td><td>three</td></tr></tbody></table>"
      val resultmulti = "<table><tbody><tr><td>one</td><td>two</td><td>three</td></tr><tr><td>four</td><td>five</td><td>six</td></tr></tbody></table>"
      val resultheader = "<table><thead><tr><th>h1</th><th>h2</th><th>h3</th></tr></thead><tbody><tr><td>one</td><td>two</td><td>three</td></tr><tr><td>four</td><td>five</td><td>six</td></tr></tbody></table>"
      val resultfooter = "<table><thead><tr><th>h1</th><th>h2</th><th>h3</th></tr></thead><tbody><tr><td>one</td><td>two</td><td>three</td></tr><tr><td>four</td><td>five</td><td>six</td></tr></tbody><tfoot><tr><td>sum1</td><td>sum2</td><td>sum3</td></tr></tfoot></table>"
      val resultcaption = """<table id="tablelabel"><caption>Title</caption><thead><tr><th>h1</th><th>h2</th><th>h3</th></tr></thead><tbody><tr><td>one</td><td>two</td><td>three</td></tr><tr><td>four</td><td>five</td><td>six</td></tr></tbody><tfoot><tr><td>sum1</td><td>sum2</td><td>sum3</td></tr></tfoot></table>"""
      // "typical" in {
      //   parse_orgmode_simple(tabletypical, result)
      // }
      // "simple" in {
      //   parse_orgmode_simple(tablesimple, result)
      // }
      // "simple2" in {
      //   parse_orgmode_simple(tablesimple2, result)
      // }
      // "multi" in {
      //   parse_orgmode_simple(tablemulti, resultmulti)
      // }
      // "header" in {
      //   parse_orgmode_simple(tableheadertypical, resultheader)
      // }
      // "headersimple" in {
      //   parse_orgmode_simple(tableheadersimple, resultheader)
      // }
      // "footer" in {
      //   parse_orgmode_simple(tablefootertypical, resultfooter)
      // }
      // "footersimple" in {
      //   parse_orgmode_simple(tablefootersimple, resultfooter)
      // }
      // "caption" in {
      //   parse_orgmode_simple(tablecaption, resultcaption)
      // }
//       "missing closing / and ] in table" in { future
//         parse_orgmode_simple("""| [[http://example.com/][Some/None] |""",
//             """<table><tbody><tr><td><a href="http://example.com/">Some/None</a></td></tr></tbody></table>""")
//       }
//       "external csv" in { future
//         parse_orgmode_simple("#+table: \"test.csv\" src\n",
//                              """<ttable uri="test.csv" src="true"/>""")
//       }
    }
  }
  val imgdot = "#+begin_dot image/simple.png\nDOT\n#+end_dot\n"
  "Image" should {
    "img" which {
      // "typical" in {
      //   parse_orgmode_simple("[[image/simple.png]]", """<p><img src="image/simple.png"/></p>""")
      // }
//       "embedded dot" in { future
//         parse_orgmode_simple(imgdot, """<p><img src="image/simple.png"/></p>""")
//       }
//       "embedded ditaa" in { future
//         parse_orgmode_simple("#+begin_ditaa image/simple.png\nDITAA\n#+end_ditaa\n", """<p><img src="image/simple.png"/></p>""")
//       }
//       "first,contents/second,contents" in { future
//         parse_orgmode_simple("* First\n1st contents.\n#+begin_dot image/simple.png\nDOT\n#+end_dot\n1st cont.\n** Second\n2nd *contents*.\n\ncont.\n* Next First\none\n\ntwo\n",
//             """<section><h2>First</h2><p>1st contents.<img src="image/simple.png"/>1st cont.</p><section><h3>Second</h3><p>2nd <b>contents</b>.</p><p>cont.</p></section></section><section><h2>Next First</h2><p>one</p><p>two</p></section>""")
//       }
    }
//     "figure" which {
//       val figure = """#+CAPTION: Figure
// #+LABEL: fig
// [[image/simple.png]]"""
//       val result = """<figure id="fig"><img src="image/simple.png"/><figcaption>Figure</figcaption></figure>"""
//       "typical" in {
//         parse_orgmode_simple(figure, result)
//       }
//       val figuredot = """#+CAPTION: Figure
// #+LABEL: fig
// """ + imgdot
//       "typical dot" in {
//         parse_orgmode_simple(figuredot, result)
//       }
//       "first,contents/second,contents" in {
//         parse_orgmode_simple("* First\n1st contents.\n#+CAPTION: Figure\n#+LABEL: fig\n#+begin_dot image/simple.png\nDOT\n#+end_dot\n1st cont.\n** Second\n2nd *contents*.\n\ncont.\n* Next First\none\n\ntwo\n",
//             """<section><h2>First</h2><p>1st contents.</p>%s<p>1st cont.</p><section><h3>Second</h3><p>2nd <b>contents</b>.</p><p>cont.</p></section></section><section><h2>Next First</h2><p>one</p><p>two</p></section>""".format(result))
//       }
//     }
  // }
  // "Paragraph" should {
  //   "prologue" which {
  //     "one paragraph" in {
  //       parse_orgmode_simple("First.\n",
  //           """<p>First.</p>""")
  //     }
  //     "three paragraphs" in {
  //       parse_orgmode_simple("First.\n\nSecond.\n\nThird.\n",
  //           """<p>First.</p><p>Second.</p><p>Third.</p>""")
  //     }
  //     "duplicate empty lines" in {
  //       parse_orgmode_simple("\nFirst.\n\n\nSecond.\n\n\nThird.\n\n",
  //           """<p>First.</p><p>Second.</p><p>Third.</p>""")
  //     }
  //   }
  //   "inside sections" which {
  //     "first,contents/second,contents" in {
  //       parse_orgmode_simple("* First\n1st contents.\n\ncont.\n** Second\n2nd *contents*.\n\ncont.\n* Next First\none\n\ntwo\n",
  //           """<section><h2>First</h2><p>1st contents.</p><p>cont.</p><section><h3>Second</h3><p>2nd <b>contents</b>.</p><p>cont.</p></section></section><section><h2>Next First</h2><p>one</p><p>two</p></section>""")
  //     }
  //   }
  // }
//   "Comment" should {
//     "comment #" in {
//       parse_orgmode_simple("abc\n#def\nghi",
//             """<p>abc ghi</p>""")
//     }
//     "comment section" in {
//       parse_orgmode_simple("* COMMENT abc\ndef\nghi\n* jkl",
//             """<section><h2>jkl</h2></section>""")
//     }
//     "comment block" in {
//       parse_orgmode_simple("abc\n#+BEGIN_COMMENT\ndef\n#+END_COMMENT\nghi\n",
//             """<p>abc ghi</p>""")
//     }
//     "comment block lowercase" in {
//       parse_orgmode_simple("abc\n#+begin_comment\ndef\n#+end_comment\nghi\n",
//             """<p>abc ghi</p>""")
//     }
//     "file:abc.png" in {
//       parse_orgmode_simple("file:abc.png",
//             """<p><img src="abc.png"/></p>""")
//     }
//     "file:abc.doc" in {
//       parse_orgmode_simple("file:abc.doc",
//             """<p><a href="abc.doc">abc.doc</a></p>""")
//     }
//     "<tt>" in {
//       parse_orgmode_simple("""<tt>abc</tt>""",
//             """<p><tt>abc</tt></p>""")
//     }
//   }
//   "Literal" should {
//     "<[ ]>" in {
//       parse_orgmode_simple("""<[<*>]>""",
//             """<p>&lt;*&gt;</p>""")
//     }
//     "<[ ]> with /" in {
//       parse_orgmode_simple("""<[/a/b/c]>""",
//             """<p>/a/b/c</p>""")
//     }
//     "<t>" in {
//       parse_orgmode_simple("""<t>*span*</t>""",
//             """<p>*span*</p>""")
//     }
//     "<span>" in {
//       parse_orgmode_simple("""<span>*span*</span>""",
//             """<p><span><b>span</b></span></p>""")
//     }
  }
//   "Terse" should {
//     "typical" which {
//       "underscoe" in {
//         parse_orgmode("register_provisional",
//           "<!DOCTYPE html><html><head/><body><p>register _provisional</p></body></html>"
//         )
//       }
//     }
//   }
  "include" should {
    "Include" which {
      "asciidoc style" in {
        Given("an AsciiDoc include directive")
        When("the parser resolves the included SmartDox source")
        Then("the included document content is rendered")
        parse_orgmode_simple(
          """include::src/test/resources/abc.dox[]""",
          """<p>X</p>"""
        )
      }
      "orgmode style" in {
        Given("an Org-mode include directive")
        When("the parser resolves the included SmartDox source")
        Then("the included document content is rendered")
        parse_orgmode_simple(
          """#+INCLUDE: src/test/resources/abc.dox""",
          """<p>X</p>"""
        )
      }
      "scala" in {
        Given("an AsciiDoc include directive for Scala source")
        When("the parser resolves the source include")
        Then("the rendered program block retains its established attributes")
        parse_orgmode_simple(
          """include::src/test/resources/sample.scala[]""",
          """<pre kind="scala" caption="sample.scala" kind="scala" caption="sample.scala" class="program">object x {}
</pre>"""
        )
      }
    }
  }

  private def _nodes(p: Dox): Vector[Dox] =
    p +: p.elements.toVector.flatMap(_nodes)
}
