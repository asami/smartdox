package org.smartdox.converters

import java.io.File
import java.nio.file.Files
import org.scalatest.GivenWhenThen
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceMatchers
import org.goldenport.io.IoUtils
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._
import org.smartdox.parser.UseDox2Parser

/*
 * @since   Jun.  2, 2026
 *  version Jun.  3, 2026
 * @version Aug. 25, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class Dox2LatexConverterSpec extends AnyWordSpec with Matchers with ScalazMatchers with UseDox2Parser with ConsequenceMatchers with GivenWhenThen {
  protected def make_latex(s: String): Consequence[String] = {
    val c = new Dox2LatexConverter()
    val dox = parse_dox(s)
    c.convert(dox)
  }

  "Dox2LatexConverter" should {
    "preserve public constructors for both converter resource contracts" in {
      Given("the public Dox2LatexConverter constructors")
      val constructors = classOf[Dox2LatexConverter].getConstructors.toList

      When("the former and current constructor descriptors are selected")
      val former = constructors.find(_.getParameterTypes.length == 9)
      val current = constructors.find(_.getParameterTypes.length == 10)

      Then("both public descriptors remain available with their original parameter order")
      former.map(_.getParameterTypes.toList) shouldBe Some(List(
        classOf[Dox2LatexConverter.Engine],
        classOf[Dox2LatexConverter.Format],
        classOf[Option[Any]],
        classOf[Option[Any]],
        classOf[Option[Any]],
        classOf[Option[Any]],
        classOf[Option[Any]],
        java.lang.Boolean.TYPE,
        classOf[Option[Any]]
      ))
      current.map(_.getParameterTypes.toList) shouldBe Some(List(
        classOf[Dox2LatexConverter.Engine],
        classOf[Dox2LatexConverter.Format],
        classOf[Option[Any]],
        classOf[Option[Any]],
        classOf[Option[Any]],
        classOf[Option[Any]],
        classOf[Option[Any]],
        java.lang.Boolean.TYPE,
        classOf[Option[Any]],
        classOf[Option[Any]]
      ))
    }

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

    "render an annotated local Figure in the LaTeX work directory" in {
      Given("an article-local PNG referenced by an annotated SmartDox Figure")
      val articledir = Files.createTempDirectory("smartdox-latex-figure-article").toFile
      val workdir = Files.createTempDirectory("smartdox-latex-figure-work").toFile
      val image = new File(articledir, "knowledge-hub.png")
      Files.write(image.toPath, Array[Byte](0, 1, 2, 3))
      try {
        When("the LaTeX converter receives the article resource base and work directory")
        val c = new Dox2LatexConverter(
          diagramDir = Some(workdir),
          resourceBaseDir = Some(articledir)
        )
        val figure = c.convert(Document.create(Figure(
          ReferenceImg("knowledge-hub.png"),
          Figcaption("Knowledge Hub & Figure"),
          Some("fig:knowledgehub")))).take
        val standalone = new Dox2LatexConverter(
          diagramDir = Some(workdir),
          resourceBaseDir = Some(articledir)
        ).convert(parse_dox("[[knowledge-hub.png]]")).take

        Then("the Figure is emitted once with an escaped caption and a copied local image")
        figure.split("\\\\includegraphics", -1).length shouldBe 2
        figure should include ("\\begin{figure}[htbp]")
        figure should include ("\\caption{Knowledge Hub \\& Figure}")
        figure should include ("\\label{fig:knowledgehub}")
        val copied = workdir.listFiles.filter(_.isFile)
        copied.length shouldBe 1
        copied.head.getName should endWith (".png")
        Files.isRegularFile(copied.head.toPath) shouldBe true
        figure should include (s"\\detokenize{${copied.head.getName}}")
        standalone should include ("\\begin{center}")
        standalone should include ("\\includegraphics")
        standalone should not include ("\\caption{")
      } finally {
        IoUtils.removeDirectory(articledir)
        IoUtils.removeDirectory(workdir)
      }
    }

    "reject a missing local Figure image before emitting LaTeX" in {
      Given("an article resource base with no referenced local image")
      val articledir = Files.createTempDirectory("smartdox-latex-missing-figure").toFile
      val workdir = Files.createTempDirectory("smartdox-latex-missing-figure-work").toFile
      try {
        When("the converter receives a Figure whose local image is absent")
        val failure = intercept[RuntimeException] {
          new Dox2LatexConverter(diagramDir = Some(workdir), resourceBaseDir = Some(articledir)).convert(
            Document.create(Figure(ReferenceImg("missing.png"), Figcaption("Missing"), None))
          ).take
        }

        Then("the Figure is rejected rather than becoming a dangling includegraphics reference")
        failure.getMessage should not be empty
        workdir.listFiles.toVector shouldBe empty
      } finally {
        IoUtils.removeDirectory(articledir)
        IoUtils.removeDirectory(workdir)
      }
    }

    "reject a non-file Figure URI" in {
      Given("a converter with an article resource base and a remote image URI")
      val articledir = Files.createTempDirectory("smartdox-latex-uri-figure").toFile
      val workdir = Files.createTempDirectory("smartdox-latex-uri-figure-work").toFile
      try {
        When("the converter receives the unsupported URI reference")
        val failure = intercept[RuntimeException] {
          new Dox2LatexConverter(diagramDir = Some(workdir), resourceBaseDir = Some(articledir)).convert(
            Document.create(Figure(ReferenceImg("https://example.invalid/image.png"), Figcaption("Remote"), None))
          ).take
        }

        Then("the URI is rejected without any network-backed image output")
        failure.getMessage should not be empty
        workdir.listFiles.toVector shouldBe empty
      } finally {
        IoUtils.removeDirectory(articledir)
        IoUtils.removeDirectory(workdir)
      }
    }

    "convert unsafe Figure labels into deterministic TeX-safe keys" in {
      Given("a directly constructed Figure with an unsafe label")
      val articledir = Files.createTempDirectory("smartdox-latex-unsafe-label-article").toFile
      val workdir = Files.createTempDirectory("smartdox-latex-unsafe-label-work").toFile
      val image = new File(articledir, "knowledge-hub.png")
      val unsafe = "unsafe}{\\input{evil}"
      Files.write(image.toPath, Array[Byte](0, 1, 2, 3))
      try {
        When("the Figure is converted twice with the same local image and label")
        val figure = Figure(ReferenceImg("knowledge-hub.png"), Figcaption("Unsafe Figure"), Some(unsafe))
        val first = new Dox2LatexConverter(
          diagramDir = Some(workdir),
          resourceBaseDir = Some(articledir)
        ).convert(Document.create(figure)).take
        val second = new Dox2LatexConverter(
          diagramDir = Some(workdir),
          resourceBaseDir = Some(articledir)
        ).convert(Document.create(figure)).take

        Then("the emitted label is stable, TeX-safe, and never includes the unsafe source")
        first shouldBe second
        first should include regex ("\\\\label\\{figure-[0-9a-f]{64}\\}".r)
        first should not include unsafe
      } finally {
        IoUtils.removeDirectory(articledir)
        IoUtils.removeDirectory(workdir)
      }
    }

    "derive copied Figure identities from image content rather than article paths" in {
      Given("same-named local images in separate article directories")
      val firstarticledir = Files.createTempDirectory("smartdox-latex-content-first").toFile
      val secondarticledir = Files.createTempDirectory("smartdox-latex-content-second").toFile
      val firstworkdir = Files.createTempDirectory("smartdox-latex-content-first-work").toFile
      val secondworkdir = Files.createTempDirectory("smartdox-latex-content-second-work").toFile
      val changedworkdir = Files.createTempDirectory("smartdox-latex-content-changed-work").toFile
      val firstimage = new File(firstarticledir, "knowledge-hub.png")
      val secondimage = new File(secondarticledir, "knowledge-hub.png")
      Files.write(firstimage.toPath, Array[Byte](0, 1, 2, 3))
      Files.write(secondimage.toPath, Array[Byte](0, 1, 2, 3))
      try {
        When("the same bytes and then changed bytes are converted into LaTeX work directories")
        val firstlatex = new Dox2LatexConverter(
          diagramDir = Some(firstworkdir),
          resourceBaseDir = Some(firstarticledir)
        ).convert(parse_dox("[[knowledge-hub.png]]")).take
        val secondlatex = new Dox2LatexConverter(
          diagramDir = Some(secondworkdir),
          resourceBaseDir = Some(secondarticledir)
        ).convert(parse_dox("[[knowledge-hub.png]]")).take
        val firstfilename = firstworkdir.listFiles.filter(_.isFile).head.getName
        val secondfilename = secondworkdir.listFiles.filter(_.isFile).head.getName
        Files.write(secondimage.toPath, Array[Byte](4, 5, 6, 7))
        val changedlatex = new Dox2LatexConverter(
          diagramDir = Some(changedworkdir),
          resourceBaseDir = Some(secondarticledir)
        ).convert(parse_dox("[[knowledge-hub.png]]")).take
        val changedfilename = changedworkdir.listFiles.filter(_.isFile).head.getName

        Then("same image bytes keep their copied filename and changed bytes receive a new identity")
        firstfilename shouldBe secondfilename
        changedfilename should not be firstfilename
        firstlatex should include (s"\\detokenize{$firstfilename}")
        secondlatex should include (s"\\detokenize{$secondfilename}")
        changedlatex should include (s"\\detokenize{$changedfilename}")
      } finally {
        IoUtils.removeDirectory(firstarticledir)
        IoUtils.removeDirectory(secondarticledir)
        IoUtils.removeDirectory(firstworkdir)
        IoUtils.removeDirectory(secondworkdir)
        IoUtils.removeDirectory(changedworkdir)
      }
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
