package org.smartdox.converters

import java.io.File
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceMatchers
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._
import org.smartdox.parser.UseDox2Parser

/*
 * @since   Jun.  2, 2026
 * @version Jun.  3, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class Dox2LatexConverterSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDox2Parser with ConsequenceMatchers {
  protected def make_latex(s: String): Consequence[String] = {
    val c = new Dox2LatexConverter()
    val dox = parse_dox(s)
    c.convert(dox)
  }

  "Dox2LatexConverter" should {
    "render SmartDox document title" in {
      val s = make_latex("""日本語PDFテスト
========

これはSmartDoxからLaTeXへ変換するテストです。
""")
      val r = s.take
      r should include ("\\title{日本語PDFテスト}")
      r should include ("\\date{}")
      r should include ("\\maketitle")
      r should include ("これはSmartDoxからLaTeXへ変換するテストです。")
    }

    "render Japanese text with an UpLaTeX preamble" in {
      val s = make_latex("""# 日本語PDFテスト

これはSmartDoxからLaTeXへ変換するテストです。

- 源氏物語
- 岩波書店
""")
      val r = s.take
      r should include ("\\documentclass[a4paper,11pt]{ltjsarticle}")
      r should include ("\\usepackage{luatexja}")
      r should include ("\\section{日本語PDFテスト}")
      r should include ("これはSmartDoxからLaTeXへ変換するテストです。")
      r should include ("\\begin{itemize}")
      r should include ("\\item 源氏物語")
      r should include ("\\item 岩波書店")
      r should include ("\\end{document}")
    }

    "render UpLaTeX preamble when selected" in {
      val c = new Dox2LatexConverter(Dox2LatexConverter.Engine.UpLatex)
      val dox = parse_dox("日本語PDF")
      val r = c.convert(dox).take
      r should include ("\\documentclass[uplatex,a4j,11pt]{jsarticle}")
      r should not include ("\\usepackage{luatexja}")
    }

    "render business document title block when selected" in {
      val c = new Dox2LatexConverter(
        format = Dox2LatexConverter.Format.Business,
        documentDate = Some("2026-06-02"),
        affiliation = Some("知識基盤開発室"),
        author = Some("山田 太郎")
      )
      val dox = parse_dox("""業務報告
========

本文です。
""")
      val r = c.convert(dox).take
      r should include ("\\begin{center}")
      r should include ("{\\Large\\bfseries 業務報告}")
      r should include ("\\vspace{\\baselineskip}")
      r should include ("\\begin{flushright}")
      r should include ("2026-06-02\\\\")
      r should include ("知識基盤開発室 山田 太郎")
      r should not include ("\\maketitle")
      r should include ("本文です。")
    }

    "use SmartDox head properties for business document title block" in {
      val c = new Dox2LatexConverter(format = Dox2LatexConverter.Format.Business)
      val dox = parse_dox("""業務報告
========

date = "2026-06-02"
affiliation = "知識基盤開発室"
author = "山田 太郎"

本文です。
""")
      val r = c.convert(dox).take
      r should include ("{\\Large\\bfseries 業務報告}")
      r should include ("2026-06-02\\\\")
      r should include ("知識基盤開発室 山田 太郎")
      r should include ("本文です。")
    }

    "use HEAD section properties for business document title block" in {
      val c = new Dox2LatexConverter(format = Dox2LatexConverter.Format.Business)
      val dox = parse_dox("""業務報告
===

# HEAD

published_at=2026-06-02
organization=知識基盤開発室
author=山田 太郎

# 本文

本文です。
""")
      val r = c.convert(dox).take
      r should include ("{\\Large\\bfseries 業務報告}")
      r should include ("2026-06-02\\\\")
      r should include ("知識基盤開発室 山田 太郎")
      r should include ("本文です。")
    }

    "allow omitted business author and organization" in {
      val c = new Dox2LatexConverter(format = Dox2LatexConverter.Format.Business)
      val dox = parse_dox("""業務報告
========

date = "2026-06-02"

本文です。
""")
      val r = c.convert(dox).take
      r should include ("{\\Large\\bfseries 業務報告}")
      r should include ("\\begin{flushright}")
      r should include ("2026-06-02")
      r should not include ("Knowledge Hub")
      r should not include ("Taro Yamada")
      r should include ("本文です。")
    }

    "embed Kroki diagrams as images when diagram generation is enabled" in {
      val image = File.createTempFile("smartdox-latex-diagram", ".png")
      try {
        val c = new Dox2LatexConverter(
          isDiagramGeneration = true,
          diagramRenderer = Some(new Dox2LatexConverter.DiagramRenderer {
            def render(kind: String, source: String, format: String): File = {
              kind should be ("plantuml")
              source should include ("@startuml")
              format should be ("png")
              image
            }
          })
        )
        val dox = Document(
          Head(),
          Body(List(Program.create(
            "@startuml\n[要件整理] lasts 3 days\n@enduml\n",
            "kind" -> "plantuml"
          )))
        )
        val r = c.convert(dox).take
        r should include ("\\includegraphics[width=\\linewidth,keepaspectratio]")
        r should include (image.getAbsolutePath)
        r should not include ("\\begin{verbatim}")
        r should not include ("@startuml")
      } finally {
        image.delete()
      }
    }

    "embed fenced Kroki diagrams as images when diagram generation is enabled" in {
      val image = File.createTempFile("smartdox-latex-fenced-diagram", ".png")
      try {
        val c = new Dox2LatexConverter(
          isDiagramGeneration = true,
          diagramRenderer = Some(new Dox2LatexConverter.DiagramRenderer {
            def render(kind: String, source: String, format: String): File = {
              kind should be ("plantuml")
              source should include ("@startuml")
              format should be ("png")
              image
            }
          })
        )
        val dox = parse_dox("""工程
===

```plantuml
@startuml
[要件整理] lasts 3 days
@enduml
```
""")
        val r = c.convert(dox).take
        r should include ("\\includegraphics[width=\\linewidth,keepaspectratio]")
        r should include (image.getAbsolutePath)
        r should not include ("\\begin{verbatim}")
      } finally {
        image.delete()
      }
    }

    "render diagram failures at the source location" in {
      val c = new Dox2LatexConverter(
        isDiagramGeneration = true,
        diagramRenderer = Some(new Dox2LatexConverter.DiagramRenderer {
          def render(kind: String, source: String, format: String): File =
            throw new RuntimeException("Kroki render failed: HTTP 400")
        })
      )
      val dox = parse_dox("""工程
===

```plantuml
@startgantt
[技術検討] starts at [技術検討]'s end
@endgantt
```
""")
      val r = c.convert(dox).take
      r should include ("\\textbf{Diagram render error (plantuml)}")
      r should include ("Kroki render failed: HTTP 400")
      r should include ("Diagram source:")
      r should include ("@startgantt")
      r should include ("[技術検討] starts at [技術検討]'s end")
      r should not include ("\\includegraphics[width=\\linewidth,keepaspectratio]")
    }
  }
}
