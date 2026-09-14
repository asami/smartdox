package org.smartdox.service.operations

import java.util.{Base64, Locale}
import java.net.URI
import java.nio.file.{FileSystems, Files, Paths, StandardWatchEventKinds}
import java.util.concurrent.atomic.AtomicReference
import org.junit.runner.RunWith
import io.circe.Json
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.smartdox._
import org.smartdox.converters.Dox2LatexConverter
import org.smartdox.diagnostics.{RenderingDiagnosticStage, StructuredRenderingDiagnostic, StructuredRenderingDiagnosticException}
import org.smartdox.generator.{Context => GeneratorContext}
import org.goldenport.collection.VectorMap
import org.goldenport.cli.{Environment, Request}
import org.goldenport.context.{InvalidArgumentFault, ResourceNotFoundFault, UnsupportedOperationFault}
import org.goldenport.extension.IRecord
import org.goldenport.i18n.{I18NContext, I18NHangar, I18NString}
import org.goldenport.io.IoUtils
import org.goldenport.tree.TreeTransformer
import org.smartdox.metadata.{DocumentMetaData, Explanation}
import org.smartdox.transformers.LanguageFilterTransformer

/*
 * @since   Aug. 29, 2026
 * @version Sep. 14, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class PdfOperationClassSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "PDF Markdown-image parsing" should {
    "propagate one canonical input parent through filename style selection and image admission" in {
      Given("a temporary Markdown input with an in-root dot-segment image path")
      val root = Files.createTempDirectory("smartdox-pdf-markdown-image")
      val input = root.resolve("article.md")
      Files.write(
        input,
        "![PDF図](images/../images/diagram.png)".getBytes("UTF-8")
      )
      try {
        When("the package-visible PDF parsing seam establishes the input identity")
        val parsed = PdfOperationClass._parse_pdf_input(input.toFile)
        val image = parsed.dox.find { case _: ReferenceImg => true; case _ => false }.
          collect { case m: ReferenceImg => m }.get

        Then("the canonical parent survives Markdown filename style selection into inline image admission")
        parsed.resourceroot.toPath shouldBe input.toFile.getCanonicalFile.getParentFile.toPath
        image.src.toString shouldBe "images/diagram.png"
        image.alt shouldBe Some("PDF図")
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "stage an admitted root-contained Markdown image at each renderer's generated reference" in {
      Given("a temporary Markdown input with an existing image under its canonical resource root")
      val root = Files.createTempDirectory("smartdox-pdf-markdown-image-stage")
      val image = root.resolve("images/collection/diagram.png")
      val input = root.resolve("article.md")
      val payload = Array[Byte](1, 2, 3, 4)
      Files.createDirectories(image.getParent)
      Files.write(image, payload)
      Files.write(input, "![diagram](images/collection/diagram.png)".getBytes("UTF-8"))
      try {
        When("the package-visible renderer-workspace seam stages the parsed document")
        val parsed = PdfOperationClass._parse_pdf_input(input.toFile)
        val chrome = PdfOperationClass._prepare_renderer_workspace(
          PdfOperationClass.PdfRenderer.ChromeHeadless,
          "article.html",
          "<html></html>",
          parsed.dox,
          parsed.resourceroot
        )
        try {
          val asciidoc = PdfOperationClass._prepare_renderer_workspace(
            PdfOperationClass.PdfRenderer.Asciidoc,
            "article.adoc",
            "image::collection:diagram.png[]",
            parsed.dox,
            parsed.resourceroot
          )
          try {
            Then("Chrome receives the normalized relative image URI path")
            Files.readAllBytes(chrome.directory.resolve("images/collection/diagram.png")).toVector shouldBe payload.toVector
            And("Asciidoc receives the current converter target name")
            Files.readAllBytes(asciidoc.directory.resolve("collection:diagram.png")).toVector shouldBe payload.toVector
          } finally {
            asciidoc.dispose()
          }
        } finally {
          chrome.dispose()
        }
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "reject root-escaping Markdown images at the PDF parsing boundary" in {
      Given("a temporary Markdown PDF input with a path that escapes its canonical parent")
      val root = Files.createTempDirectory("smartdox-pdf-markdown-image-escape")
      val input = root.resolve("escaping.md")
      Files.write(input, "![escape](../outside.png)".getBytes("UTF-8"))
      try {
        When("the PDF parsing seam admits the Markdown image before any external renderer is selected")
        val failure = intercept[IllegalArgumentException] {
          PdfOperationClass._parse_pdf_input(input.toFile)
        }

        Then("the parser-owned unsupported-resource diagnostic rejects the escaped path")
        failure.getMessage should include ("image.markdown.unsupported-resource")
        failure.getMessage should include ("raw-path=../outside.png")
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "reject a missing Markdown image before Chrome, Asciidoc, or LaTeX can start an external renderer" in {
      Given("normal local-renderer PDF requests whose admitted Markdown image is absent")
      val renderers = Vector(
        "chrome-headless" -> "--chrome",
        "asciidoc" -> "--asciidoctor-pdf",
        "latex" -> "--latexmk"
      )

      When("each PDF renderer receives its normal command with a deliberately unavailable renderer path")
      val failures = renderers.map {
        case (renderer, rendereroption) => renderer -> _missing_markdown_image_failure(renderer, rendereroption)
      }

      Then("every renderer fails with the missing-resource diagnostic before attempting its unavailable binary")
      failures.foreach {
        case (_, failure) =>
          failure.getMessage should include ("image.local.missing-resource")
          failure.getMessage should include ("source=images/missing.png")
      }
    }
  }

  "PDF local LaTeX rendering" should {
    "render a Markdown image after launching latexmk in the generated TeX directory" in {
      Given("a temporary Markdown input with one valid root-contained image and a fake local latexmk")
      val root = Files.createTempDirectory("smartdox-pdf-local-latex-image")
      val image = root.resolve("images/diagram.png")
      val input = root.resolve("article.md")
      val latexmk = root.resolve("latexmk")
      Files.createDirectories(image.getParent)
      Files.write(
        image,
        Base64.getDecoder.decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQIHWP4z8DwHwAFgAI/ScL7WQAAAABJRU5ErkJggg==")
      )
      Files.write(input, "![diagram](images/diagram.png)".getBytes("UTF-8"))
      _write_local_latexmk(latexmk)
      latexmk.toFile.setExecutable(true) shouldBe true
      val request = Request.create(
        PdfOperationClass.specification,
        Array(
          "--renderer",
          "latex",
          "--dependency-mode",
          "local",
          "--latexmk",
          latexmk.toString,
          input.toString
        )
      )
      val command = PdfOperationClass.PdfCommand.create(request)
      try {
        When("local LaTeX PDF generation executes the generated TeX input")
        val result = PdfOperationClass.execute(Environment.createJaJp(), command)

        Then("the renderer creates a non-empty PDF artifact from the generated output path")
        Files.isRegularFile(result.artifact.toFile.toPath) shouldBe true
        Files.size(result.artifact.toFile.toPath) should be > 0L
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "preserve normalized Japanese Markdown-image semantics across repeated local PDF conversions" in {
      Given("a temporary Markdown input with a Japanese alt text, a dot-segment image path, and a fake local latexmk")
      val root = Files.createTempDirectory("smartdox-pdf-local-latex-markdown-image")
      val image = root.resolve("images/diagram.png")
      val input = root.resolve("article.md")
      val latexmk = root.resolve("latexmk")
      Files.createDirectories(image.getParent)
      Files.write(
        image,
        Base64.getDecoder.decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQIHWP4z8DwHwAFgAI/ScL7WQAAAABJRU5ErkJggg==")
      )
      Files.write(input, "![日本語の図](images/../images/diagram.png)".getBytes("UTF-8"))
      _write_local_latexmk(latexmk)
      latexmk.toFile.setExecutable(true) shouldBe true
      val request = Request.create(
        PdfOperationClass.specification,
        Array(
          "--renderer",
          "latex",
          "--dependency-mode",
          "local",
          "--latexmk",
          latexmk.toString,
          input.toString
        )
      )
      val command = PdfOperationClass.PdfCommand.create(request)
      try {
        When("the PDF input is parsed and the same local LaTeX command executes twice")
        val parsed = PdfOperationClass._parse_pdf_input(input.toFile)
        val first = PdfOperationClass.execute(Environment.createJaJp(), command)
        val second = PdfOperationClass.execute(Environment.createJaJp(), command)

        val image = parsed.dox.find { case _: ReferenceImg => true; case _ => false }.
          collect { case m: ReferenceImg => m }.get
        Then("the parsed image keeps its Japanese alt text and normalized relative source")
        image.src.toString shouldBe "images/diagram.png"
        image.alt shouldBe Some("日本語の図")
        And("both local PDF artifacts are regular, non-empty, and byte-for-byte equal")
        Files.isRegularFile(first.artifact.toFile.toPath) shouldBe true
        Files.isRegularFile(second.artifact.toFile.toPath) shouldBe true
        Files.size(first.artifact.toFile.toPath) should be > 0L
        Files.size(second.artifact.toFile.toPath) should be > 0L
        Files.readAllBytes(first.artifact.toFile.toPath).toVector shouldBe
          Files.readAllBytes(second.artifact.toFile.toPath).toVector
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "resolve a relative latexmk path before switching to the generated TeX directory" in {
      Given("a temporary Markdown input with one root-contained image and a fake latexmk below the invocation directory")
      val root = Files.createTempDirectory("smartdox-pdf-relative-latex-image")
      val image = root.resolve("images/diagram.png")
      val input = root.resolve("article.md")
      val invocationroot = Paths.get("").toAbsolutePath.normalize
      val commandroot = Files.createTempDirectory(invocationroot, "smartdox-pdf-relative-latexmk")
      val latexmk = commandroot.resolve("tools/latexmk")
      val configuredlatexmk = invocationroot.relativize(latexmk).toString
      Files.createDirectories(image.getParent)
      Files.createDirectories(latexmk.getParent)
      Files.write(
        image,
        Base64.getDecoder.decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQIHWP4z8DwHwAFgAI/ScL7WQAAAABJRU5ErkJggg==")
      )
      Files.write(input, "![diagram](images/diagram.png)".getBytes("UTF-8"))
      _write_local_latexmk(latexmk)
      latexmk.toFile.setExecutable(true) shouldBe true
      val request = Request.create(
        PdfOperationClass.specification,
        Array(
          "--renderer",
          "latex",
          "--dependency-mode",
          "local",
          "--latexmk",
          configuredlatexmk,
          input.toString
        )
      )
      val command = PdfOperationClass.PdfCommand.create(request)
      try {
        When("local LaTeX PDF generation starts the relative configured executable from its invocation location")
        val result = PdfOperationClass.execute(Environment.createJaJp(), command)

        Then("the fake observes the generated TeX directory and staged PNG before producing a PDF artifact")
        Files.isRegularFile(result.artifact.toFile.toPath) shouldBe true
        Files.size(result.artifact.toFile.toPath) should be > 0L
      } finally {
        IoUtils.removeDirectory(root.toFile)
        IoUtils.removeDirectory(commandroot.toFile)
      }
    }
  }

  "PDF site publication context" should {
    "resolve a Site link to its localized HTTPS projection without a raw Dox fallback" in {
      val root = Files.createTempDirectory("smartdox-pdf-site-link")
      try {
        Given("an explicit multi-locale site fixture with a Document Project source, sibling target, and malformed private review sources")
        val input = _write_site_publication_fixture(root)
        val request = Request.create(
          PdfOperationClass.specification,
          Array(
            "--site-root", root.toString,
            "--site-config", root.resolve("site.conf").toString,
            "--locale", "ja",
            input.toString
          )
        )
        val command = PdfOperationClass.PdfCommand.create(request)
        val parsed = PdfOperationClass._parse_pdf_input(input.toFile)

        When("the selected Japanese PDF document applies the explicit publication context")
        val resolved = PdfOperationClass._resolve_site_links(
          GeneratorContext.create(Environment.createJaJp()),
          command,
          PdfOperationClass._select_locale(parsed.dox, Some("ja")).take,
          Some("ja")
        ).take
        val latex = new Dox2LatexConverter().convert(resolved).take

        Then("the accepted Japanese FQN and localized visible title are projected as a hyperlink")
        latex should include ("https://www.simplemodeling.org/ja/development-process/literate-modeling.html")
        latex should include ("文芸モデリング")
        And("neither the visible Dox nor LaTeX projection falls back to the authored Dox filename")
        resolved.toPlainText should not include "literate-modeling.dox"
        latex should not include "literate-modeling.dox"
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "fail closed before a configured renderer starts when a Site link lacks context" in {
      val root = Files.createTempDirectory("smartdox-pdf-site-context-missing")
      val input = root.resolve("article.dox")
      val latexmk = root.resolve("latexmk")
      val marker = root.resolve("renderer-started")
      Files.write(input, "site:[target.dox]".getBytes("UTF-8"))
      _write_renderer_marker(latexmk, marker)
      latexmk.toFile.setExecutable(true) shouldBe true
      try {
        Given("a Site-link PDF command with a fake local renderer but no site-root or site-config inputs")
        val request = Request.create(
          PdfOperationClass.specification,
          Array(
            "--renderer", "latex",
            "--dependency-mode", "local",
            "--latexmk", latexmk.toString,
            input.toString
          )
        )
        val command = PdfOperationClass.PdfCommand.create(request)

        When("the PDF operation is started")
        val failure = intercept[RuntimeException] {
          PdfOperationClass.execute(Environment.createJaJp(), command)
        }

        Then("the stable missing-context diagnostic is returned before renderer invocation")
        failure.getMessage should include ("pdf.site-context.missing")
        Files.exists(marker) shouldBe false
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "retain ordinary PDF compatibility while rejecting malformed, escaping, and unresolved Site contexts" in {
      val root = Files.createTempDirectory("smartdox-pdf-site-context-validation")
      try {
        Given("a no-site PDF input and explicit Site-link fixtures with invalid context conditions")
        val plain = root.resolve("plain.dox")
        Files.write(plain, "ordinary PDF text".getBytes("UTF-8"))
        val plainrequest = Request.create(PdfOperationClass.specification, Array(plain.toString))
        val plaincommand = PdfOperationClass.PdfCommand.create(plainrequest)
        val plainparsed = PdfOperationClass._parse_pdf_input(plain.toFile)
        val validinput = _write_site_publication_fixture(root.resolve("valid"))
        val escaping = root.resolve("valid/development-process/domain-modeling.dox/escaping.dox")
        Files.write(escaping, "site:[../../../outside.dox]".getBytes("UTF-8"))
        val unresolved = root.resolve("valid/development-process/domain-modeling.dox/unresolved.dox")
        Files.write(unresolved, "site:[missing.dox]".getBytes("UTF-8"))
        val malformedroot = root.resolve("malformed")
        val malformedinput = _write_site_publication_fixture(malformedroot)
        Files.write(
          malformedroot.resolve("site.conf"),
          "site.metadata.url = \"http://example.test/\"\nsite.output.locale_mode = \"multi_locale_subdirs\"\n".getBytes("UTF-8")
        )

        When("the PDF resolver receives no Site link, an escaping target, an unresolved target, a non-HTTPS base URL, and a source outside its root")
        val plainresult = PdfOperationClass._resolve_site_links(
          GeneratorContext.create(Environment.createJaJp()),
          plaincommand,
          plainparsed.dox,
          None
        )
        val escapingfailure = _site_resolution(escaping, root.resolve("valid"), root.resolve("valid/site.conf"))
        val unresolvedfailure = _site_resolution(unresolved, root.resolve("valid"), root.resolve("valid/site.conf"))
        val malformedfailure = _site_resolution(malformedinput, malformedroot, malformedroot.resolve("site.conf"))
        val outsideroot = Files.createDirectories(root.resolve("other-root"))
        val outsidefailure = _site_resolution(validinput, outsideroot, root.resolve("valid/site.conf"))

        Then("ordinary documents remain compatible without context")
        plainresult.take shouldBe plainparsed.dox
        And("every Site-link failure is deterministic and does not degrade to a source filename")
        escapingfailure.message should include ("pdf.site-link.target.outside-root")
        unresolvedfailure.message should include ("pdf.site-link.target.unresolved")
        malformedfailure.message should include ("pdf.site-context.base-url.invalid")
        outsidefailure.message should include ("pdf.site-context.config.outside-root")
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }
  }

  "PDF structured rendering diagnostics" should {
    "convert an external PlantUML generation failure before a configured latexmk starts" in {
      Given("a valid PlantUML Dox input, a throwing diagram renderer, and an executable latexmk marker")
      val root = Files.createTempDirectory("smartdox-pdf-diagram-generation-diagnostic")
      val input = root.resolve("diagram.dox")
      val latexmk = root.resolve("latexmk")
      val marker = root.resolve("latexmk-started")
      Files.write(
        input,
        """|Diagram
           |=======
           |
           |```plantuml
           |@startuml
           |Alice -> Bob: render
           |@enduml
           |```
           |""".stripMargin.getBytes("UTF-8")
      )
      _write_renderer_marker(latexmk, marker)
      latexmk.toFile.setExecutable(true) shouldBe true
      val command = PdfOperationClass.PdfCommand.create(_pdf_request(input, latexmk, None))
      val renderer = new Dox2LatexConverter.DiagramRenderer {
        def render(kind: String, source: String, format: String): java.io.File =
          throw new RuntimeException("external diagram renderer detail")
      }
      try {
        When("the package-visible LaTeX PDF seam converts the document before typesetter startup")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass._execute_latex_with_diagram_renderer(
            Environment.createJaJp(),
            command,
            renderer
          )
        }

        Then("the external generation failure has the safe typed diagram-generation diagnostic")
        failure.diagnostic.code shouldBe "pdf.diagram-generation.failed"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.DiagramGeneration
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.line shouldBe None
        failure.diagnostic.column shouldBe None
        failure.diagnostic.tokenContext shouldBe Some("plantuml")
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe true
        failure.getMessage should not include "external diagram renderer detail"
        And("Record and JSON projections retain the complete diagram-generation diagnostic")
        _assert_pdf_diagnostic_projection(
          failure.diagnostic,
          expectedcode = "pdf.diagram-generation.failed",
          expectedstage = "diagram-generation",
          expectedsource = Some(input.toFile.getPath),
          expectedtoken = Some("plantuml"),
          expectedcause = "external-diagram-generation-failed",
          expectedterminal = true,
          expectedretryable = true
        )
        And("the latexmk marker does not exist because typesetting has not started")
        Files.exists(marker) shouldBe false
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "translate a configured nonexistent local latexmk into a safe process-start diagnostic" in {
      Given("a valid PDF input and a configured local latexmk path that does not exist")
      val root = Files.createTempDirectory("smartdox-pdf-typesetting-process-start")
      val input = root.resolve("typesetting.dox")
      val latexmk = root.resolve("missing-latexmk")
      Files.write(input, "Valid typesetting input".getBytes("UTF-8"))
      val command = PdfOperationClass.PdfCommand.create(_pdf_request(input, latexmk, None))
      try {
        When("the local LaTeX renderer process is started")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.execute(Environment.createJaJp(), command)
        }

        Then("the failure has the terminal retryable typesetting process-start identity")
        failure.diagnostic.code shouldBe "pdf.typesetting.process-start-failed"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.tokenContext shouldBe Some("latex")
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe true
        And("Record and JSON projections retain the complete process-start diagnostic")
        _assert_pdf_diagnostic_projection(
          failure.diagnostic,
          expectedcode = "pdf.typesetting.process-start-failed",
          expectedstage = "typesetting",
          expectedsource = Some(input.toFile.getPath),
          expectedtoken = Some("latex"),
          expectedcause = "external-typesetting-process-start-failed",
          expectedterminal = true,
          expectedretryable = true
        )
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "translate an untrusted local latexmk nonzero exit without exposing its stderr detail" in {
      Given("a valid PDF input and a local latexmk that writes an untrusted stderr detail before failing")
      val root = Files.createTempDirectory("smartdox-pdf-typesetting-nonzero-exit")
      val input = root.resolve("typesetting.dox")
      val latexmk = root.resolve("latexmk")
      Files.write(input, "Valid typesetting input".getBytes("UTF-8"))
      _write_nonzero_latexmk(latexmk)
      latexmk.toFile.setExecutable(true) shouldBe true
      val command = PdfOperationClass.PdfCommand.create(_pdf_request(input, latexmk, None))
      try {
        When("the local LaTeX renderer exits with a nonzero status")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.execute(Environment.createJaJp(), command)
        }

        Then("the failure has the terminal retryable typesetting nonzero-exit identity")
        failure.diagnostic.code shouldBe "pdf.typesetting.nonzero-exit"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.tokenContext shouldBe Some("latex")
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe true
        And("Record and JSON projections retain the complete nonzero-exit diagnostic")
        _assert_pdf_diagnostic_projection(
          failure.diagnostic,
          expectedcode = "pdf.typesetting.nonzero-exit",
          expectedstage = "typesetting",
          expectedsource = Some(input.toFile.getPath),
          expectedtoken = Some("latex"),
          expectedcause = "external-typesetting-nonzero-exit",
          expectedterminal = true,
          expectedretryable = true
        )
        And("the safe CLI projection omits the untrusted renderer stderr detail")
        failure.getMessage should not include "untrusted renderer stderr detail"
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "translate a successful local latexmk with no PDF into an output-missing diagnostic" in {
      Given("a valid PDF input and a successful local latexmk that emits no PDF")
      val root = Files.createTempDirectory("smartdox-pdf-typesetting-output-missing")
      val input = root.resolve("typesetting.dox")
      val latexmk = root.resolve("latexmk")
      Files.write(input, "Valid typesetting input".getBytes("UTF-8"))
      _write_no_output_latexmk(latexmk)
      latexmk.toFile.setExecutable(true) shouldBe true
      val command = PdfOperationClass.PdfCommand.create(_pdf_request(input, latexmk, None))
      try {
        When("the local LaTeX renderer exits successfully without its required PDF")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.execute(Environment.createJaJp(), command)
        }

        Then("the failure has the terminal retryable typesetting output-missing identity")
        failure.diagnostic.code shouldBe "pdf.typesetting.output-missing"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.tokenContext shouldBe Some("latex")
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe true
        And("Record and JSON projections retain the complete output-missing diagnostic")
        _assert_pdf_diagnostic_projection(
          failure.diagnostic,
          expectedcode = "pdf.typesetting.output-missing",
          expectedstage = "typesetting",
          expectedsource = Some(input.toFile.getPath),
          expectedtoken = Some("latex"),
          expectedcause = "external-typesetting-output-missing",
          expectedterminal = true,
          expectedretryable = true
        )
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "reject a Docker LaTeX renderer whose generated PDF is a nonempty symlink" in {
      Given("a valid PDF input and a fake Docker executable that creates a nonempty generated-PDF symlink")
      val root = Files.createTempDirectory("smartdox-pdf-docker-latex-symlink-output")
      val input = root.resolve("typesetting.dox")
      val docker = root.resolve("docker")
      val marker = root.resolve("docker-latex-symlink-created")
      Files.write(input, "Valid typesetting input".getBytes("UTF-8"))
      _write_symlink_output_docker(docker, marker)
      docker.toFile.setExecutable(true) shouldBe true
      val request = Request.create(
        PdfOperationClass.specification,
        Array(
          "--renderer", "latex",
          "--dependency-mode", "docker",
          "--docker", docker.toString,
          input.toString
        )
      )
      val command = PdfOperationClass.PdfCommand.create(request)
      try {
        When("the public PDF operation runs the Docker LaTeX renderer")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.execute(Environment.createJaJp(), command)
        }

        Then("the generated symlink is rejected with the typesetting output-missing diagnostic")
        failure.diagnostic.code shouldBe "pdf.typesetting.output-missing"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.tokenContext shouldBe Some("latex")
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe true
        And("the fake Docker executable created and verified its generated-PDF symlink")
        Files.exists(marker) shouldBe true
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "translate a successful local Chrome renderer with no PDF into an output-missing diagnostic" in {
      Given("a valid PDF input and a successful local Chrome renderer that emits no PDF")
      val root = Files.createTempDirectory("smartdox-pdf-chrome-output-missing")
      val input = root.resolve("typesetting.dox")
      val chrome = root.resolve("chrome")
      Files.write(input, "Valid typesetting input".getBytes("UTF-8"))
      _write_no_output_renderer(chrome)
      chrome.toFile.setExecutable(true) shouldBe true
      val request = Request.create(
        PdfOperationClass.specification,
        Array(
          "--renderer", "chrome-headless",
          "--dependency-mode", "local",
          "--chrome", chrome.toString,
          input.toString
        )
      )
      val command = PdfOperationClass.PdfCommand.create(request)
      try {
        When("the local Chrome renderer exits successfully without its required PDF")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.execute(Environment.createJaJp(), command)
        }

        Then("the failure has the terminal retryable typesetting output-missing identity for Chrome")
        failure.diagnostic.code shouldBe "pdf.typesetting.output-missing"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.tokenContext shouldBe Some("chrome-headless")
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe true
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "reject a local Chrome renderer that replaces the pre-created PDF with a nonempty symlink" in {
      Given("a valid PDF input and a successful local Chrome renderer that replaces its PDF target with a symlink")
      val root = Files.createTempDirectory("smartdox-pdf-chrome-symlink-output")
      val input = root.resolve("typesetting.dox")
      val chrome = root.resolve("chrome")
      val marker = root.resolve("chrome-symlink-created")
      Files.write(input, "Valid typesetting input".getBytes("UTF-8"))
      _write_symlink_output_chrome(chrome, marker)
      chrome.toFile.setExecutable(true) shouldBe true
      val request = Request.create(
        PdfOperationClass.specification,
        Array(
          "--renderer", "chrome-headless",
          "--dependency-mode", "local",
          "--chrome", chrome.toString,
          input.toString
        )
      )
      val command = PdfOperationClass.PdfCommand.create(request)
      try {
        When("the local Chrome renderer exits successfully after replacing the required PDF with a symlink")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.execute(Environment.createJaJp(), command)
        }

        Then("the failure has the terminal retryable typesetting output-missing identity for Chrome")
        failure.diagnostic.code shouldBe "pdf.typesetting.output-missing"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.tokenContext shouldBe Some("chrome-headless")
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe true
        And("the fake renderer created and verified its symlink replacement")
        Files.exists(marker) shouldBe true
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "translate a successful local Asciidoctor renderer with no PDF into an output-missing diagnostic" in {
      Given("a valid PDF input and a successful local Asciidoctor renderer that emits no PDF")
      val root = Files.createTempDirectory("smartdox-pdf-asciidoc-output-missing")
      val input = root.resolve("typesetting.dox")
      val asciidoctor = root.resolve("asciidoctor-pdf")
      Files.write(input, "Valid typesetting input".getBytes("UTF-8"))
      _write_no_output_renderer(asciidoctor)
      asciidoctor.toFile.setExecutable(true) shouldBe true
      val request = Request.create(
        PdfOperationClass.specification,
        Array(
          "--renderer", "asciidoc",
          "--dependency-mode", "local",
          "--asciidoctor-pdf", asciidoctor.toString,
          input.toString
        )
      )
      val command = PdfOperationClass.PdfCommand.create(request)
      try {
        When("the local Asciidoctor renderer exits successfully without its required PDF")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.execute(Environment.createJaJp(), command)
        }

        Then("the failure has the terminal retryable typesetting output-missing identity for Asciidoctor")
        failure.diagnostic.code shouldBe "pdf.typesetting.output-missing"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.tokenContext shouldBe Some("asciidoc")
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe true
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "return from a timeout only after its observed local renderer process tree terminates" in {
      Given("a valid PDF input, a local latexmk that waits for a long-lived child, and a short deadline")
      val root = Files.createTempDirectory("smartdox-pdf-typesetting-timeout")
      val input = root.resolve("typesetting.dox")
      val latexmk = root.resolve("latexmk")
      val childpid = root.resolve("typesetting-child.pid")
      Files.write(input, "Valid typesetting input".getBytes("UTF-8"))
      _write_timeout_latexmk(latexmk, childpid)
      latexmk.toFile.setExecutable(true) shouldBe true
      val request = Request.create(
        PdfOperationClass.specification,
        Array(
          "--renderer", "latex",
          "--dependency-mode", "local",
          "--latexmk", latexmk.toString,
          "--typesetting-timeout-ms", "1000",
          input.toString
        )
      )
      val command = PdfOperationClass.PdfCommand.create(request)
      try {
      When("the public local LaTeX operation reaches its configured deadline and forcefully terminates the observed tree")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.execute(Environment.createJaJp(), command)
        }
        val pid = new String(Files.readAllBytes(childpid), "UTF-8").trim.toLong
        val childalive = {
          val handle = ProcessHandle.of(pid)
          handle.isPresent && handle.get().isAlive
        }

        Then("the timeout exposes the terminal retryable safe typesetting diagnostic")
        failure.diagnostic.code shouldBe "pdf.typesetting.timeout"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.line shouldBe None
        failure.diagnostic.column shouldBe None
        failure.diagnostic.tokenContext shouldBe Some("latex")
        failure.diagnostic.cause shouldBe "external-typesetting-timeout"
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe true
        And("Record and JSON projections retain the complete expiry diagnostic")
        _assert_pdf_diagnostic_projection(
          failure.diagnostic,
          expectedcode = "pdf.typesetting.timeout",
          expectedstage = "typesetting",
          expectedsource = Some(input.toFile.getPath),
          expectedtoken = Some("latex"),
          expectedcause = "external-typesetting-timeout",
          expectedterminal = true,
          expectedretryable = true
        )
        And("the recorded renderer descendant is no longer alive before the public timeout operation returns")
        childalive shouldBe false
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "preserve an interruption that arrives during timeout cleanup after terminating the observed renderer tree" in {
      Given("a renderer timeout whose child writes a cleanup marker before resisting graceful termination")
      val root = Files.createTempDirectory("smartdox-pdf-typesetting-timeout-interruption")
      val input = root.resolve("typesetting.dox")
      val latexmk = root.resolve("latexmk")
      val childpid = root.resolve("typesetting-child.pid")
      val cleanupmarker = root.resolve("timeout-cleanup-active")
      val watcher = FileSystems.getDefault.newWatchService()
      Files.write(input, "Valid typesetting input".getBytes("UTF-8"))
      _write_interruption_timeout_latexmk(latexmk, childpid, cleanupmarker)
      latexmk.toFile.setExecutable(true) shouldBe true
      root.register(watcher, StandardWatchEventKinds.ENTRY_CREATE)
      val request = Request.create(
        PdfOperationClass.specification,
        Array(
          "--renderer", "latex",
          "--dependency-mode", "local",
          "--latexmk", latexmk.toString,
          "--typesetting-timeout-ms", "1000",
          input.toString
        )
      )
      val command = PdfOperationClass.PdfCommand.create(request)
      val outcome = new AtomicReference[Throwable]()
      val runner = new Thread(new Runnable {
        override def run(): Unit =
          try PdfRendererExecution.runProcess(command, Vector(latexmk.toString))
          catch {
            case e: Throwable => outcome.set(e)
          }
      })
      try {
        When("the cleanup marker proves process-tree termination is active and the renderer thread is interrupted")
        runner.start()
        val key = watcher.take()
        Files.isRegularFile(cleanupmarker) shouldBe true
        key.reset() shouldBe true
        runner.interrupt()
        runner.join()
        val pid = new String(Files.readAllBytes(childpid), "UTF-8").trim.toLong
        val childalive = {
          val handle = ProcessHandle.of(pid)
          handle.isPresent && handle.get().isAlive
        }

        Then("the interruption is propagated only after the observed renderer tree has terminated")
        outcome.get shouldBe a [InterruptedException]
        childalive shouldBe false
        And("the interrupted cleanup does not acquire the typesetting-timeout failure identity")
        outcome.get should not be a [PdfRendererExecution.PdfTypesettingTimeout]
        outcome.get should not be a [StructuredRenderingDiagnosticException]
      } finally {
        watcher.close()
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "reject malformed and nonpositive typesetting timeouts before a configured renderer can start" in {
      Given("a configured local latexmk marker and invalid timeout option values")
      val root = Files.createTempDirectory("smartdox-pdf-typesetting-timeout-invalid")
      val input = root.resolve("typesetting.dox")
      val latexmk = root.resolve("latexmk")
      val marker = root.resolve("latexmk-started")
      val timeouts = Vector("", "milliseconds", "0", "-1", "9223372036854775808")
      _write_renderer_marker(latexmk, marker)
      latexmk.toFile.setExecutable(true) shouldBe true
      try {
        When("each PDF command is created with its supplied timeout")
        val failures = timeouts.map { timeout =>
          val request = Request.create(
            PdfOperationClass.specification,
            Array(
              "--renderer", "latex",
              "--dependency-mode", "local",
              "--latexmk", latexmk.toString,
              "--typesetting-timeout-ms", timeout,
              input.toString
            )
          )
          timeout -> intercept[StructuredRenderingDiagnosticException] {
            PdfOperationClass.PdfCommand.create(request)
          }
        }

        Then("each supplied value has the terminal non-retryable timeout-configuration diagnostic")
        failures.foreach { case (timeout, failure) =>
          failure.diagnostic.code shouldBe "pdf.typesetting.timeout.invalid"
          failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
          failure.diagnostic.sourceIdentity shouldBe None
          failure.diagnostic.line shouldBe None
          failure.diagnostic.column shouldBe None
          failure.diagnostic.tokenContext shouldBe Some(timeout)
          failure.diagnostic.cause shouldBe "invalid-typesetting-timeout"
          failure.diagnostic.terminal shouldBe true
          failure.diagnostic.retryable shouldBe false
          And("Record and JSON projections retain the null source and invalid-timeout facets")
          _assert_pdf_diagnostic_projection(
            failure.diagnostic,
            expectedcode = "pdf.typesetting.timeout.invalid",
            expectedstage = "typesetting",
            expectedsource = None,
            expectedtoken = Some(timeout),
            expectedcause = "invalid-typesetting-timeout",
            expectedterminal = true,
            expectedretryable = false
          )
        }
        And("command creation starts neither the configured renderer nor a typesetting process")
        Files.exists(marker) shouldBe false
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "convert invalid and unsupported locale selectors before parsing or renderer startup" in {
      Given("invalid and unsupported locale selectors with a configured executable marker renderer")
      val root = Files.createTempDirectory("smartdox-pdf-locale-selector-diagnostic")
      val input = root.resolve("must-not-be-parsed.dox")
      val latexmk = root.resolve("latexmk")
      val marker = root.resolve("renderer-started")
      _write_renderer_marker(latexmk, marker)
      latexmk.toFile.setExecutable(true) shouldBe true
      try {
        val invalidrequest = _pdf_request(input, latexmk, Some("EN"))
        val unsupportedrequest = _pdf_request(input, latexmk, Some("fr"))

        When("the public PDF operation validates each selector")
        val invalidfailure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.apply(Environment.createJaJp(), invalidrequest)
        }
        val unsupportedfailure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.apply(Environment.createJaJp(), unsupportedrequest)
        }

        Then("the invalid selector has the terminal non-retryable locale-selection diagnostic")
        invalidfailure.diagnostic.code shouldBe "pdf.locale.invalid"
        invalidfailure.diagnostic.stage shouldBe RenderingDiagnosticStage.LocaleSelection
        invalidfailure.diagnostic.tokenContext shouldBe Some("EN")
        invalidfailure.diagnostic.terminal shouldBe true
        invalidfailure.diagnostic.retryable shouldBe false
        And("the unsupported selector retains its distinct stable diagnostic identity")
        unsupportedfailure.diagnostic.code shouldBe "pdf.locale.unsupported"
        unsupportedfailure.diagnostic.stage shouldBe RenderingDiagnosticStage.LocaleSelection
        unsupportedfailure.diagnostic.tokenContext shouldBe Some("fr")
        unsupportedfailure.diagnostic.terminal shouldBe true
        unsupportedfailure.diagnostic.retryable shouldBe false
        And("neither selector causes parsing or the configured renderer to start")
        Files.exists(marker) shouldBe false
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "convert an unavailable document locale before a configured renderer starts" in {
      Given("a document with no exact English source and a configured executable marker renderer")
      val root = Files.createTempDirectory("smartdox-pdf-locale-unavailable-diagnostic")
      val input = root.resolve("japanese-only.dox")
      val latexmk = root.resolve("latexmk")
      val marker = root.resolve("renderer-started")
      Files.write(input, "日本語".getBytes("UTF-8"))
      _write_renderer_marker(latexmk, marker)
      latexmk.toFile.setExecutable(true) shouldBe true
      try {
        val request = _pdf_request(input, latexmk, Some("en"))

        When("the public PDF operation selects the requested document locale")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass.apply(Environment.createJaJp(), request)
        }

        Then("the failure binds the input identity in a terminal non-retryable locale-selection diagnostic")
        failure.diagnostic.code shouldBe "pdf.locale.unavailable"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.LocaleSelection
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getPath)
        failure.diagnostic.tokenContext shouldBe Some("en")
        failure.diagnostic.terminal shouldBe true
        failure.diagnostic.retryable shouldBe false
        And("the configured renderer does not start")
        Files.exists(marker) shouldBe false
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }

    "report an unsupported renderer while PdfCommand is created" in {
      Given("a PDF request with an unsupported nonblank renderer token")
      val request = Request.create(
        PdfOperationClass.specification,
        Array("--renderer", "unsupported-renderer", "input.dox")
      )

      When("PdfCommand is created before PDF execution")
      val failure = intercept[StructuredRenderingDiagnosticException] {
        PdfOperationClass.PdfCommand.create(request)
      }

      Then("the renderer failure is terminal and non-retryable at the typesetting stage")
      failure.diagnostic.code shouldBe "pdf.renderer.unsupported"
      failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Typesetting
      failure.diagnostic.tokenContext shouldBe Some("unsupported-renderer")
      failure.diagnostic.terminal shouldBe true
      failure.diagnostic.retryable shouldBe false
    }

    "preserve a parser structured diagnostic without relabeling it as typesetting" in {
      Given("a filename-aware parser failure, a diagram renderer marker, and an executable latexmk marker")
      val root = Files.createTempDirectory("smartdox-pdf-parser-diagnostic")
      val input = root.resolve("invalid-inline.dox")
      val latexmk = root.resolve("latexmk")
      val latexmarker = root.resolve("latexmk-started")
      val diagrammarker = root.resolve("diagram-renderer-started")
      Files.write(input, "~~~text".getBytes("UTF-8"))
      _write_renderer_marker(latexmk, latexmarker)
      latexmk.toFile.setExecutable(true) shouldBe true
      val command = PdfOperationClass.PdfCommand.create(_pdf_request(input, latexmk, None))
      val diagramrenderer = new Dox2LatexConverter.DiagramRenderer {
        def render(kind: String, source: String, format: String): java.io.File = {
          Files.write(diagrammarker, Array.emptyByteArray)
          diagrammarker.toFile
        }
      }
      try {
        When("the package-visible LaTeX PDF seam parses the input")
        val failure = intercept[StructuredRenderingDiagnosticException] {
          PdfOperationClass._execute_latex_with_diagram_renderer(
            Environment.createJaJp(),
            command,
            diagramrenderer
          )
        }

        Then("the parser-owned document syntax identity and parse stage remain intact")
        failure.diagnostic.code shouldBe "document.syntax.invalid"
        failure.diagnostic.stage shouldBe RenderingDiagnosticStage.Parse
        failure.diagnostic.sourceIdentity shouldBe Some(input.toFile.getCanonicalFile.getPath.stripPrefix("/"))
        failure.diagnostic.tokenContext shouldBe Some("~text")
        And("neither diagram generation nor typesetting starts")
        Files.exists(diagrammarker) shouldBe false
        Files.exists(latexmarker) shouldBe false
      } finally {
        IoUtils.removeDirectory(root.toFile)
      }
    }
  }

  "PDF locale selection" should {
    "select exact Japanese content while retaining neutral content" which {
      "exclude English and regional English branches" in {
        Given("one document containing neutral, Japanese, English, and en-US paragraphs")
        val source = _source

        When("the document is selected for the canonical Japanese locale")
        val result = PdfOperationClass._select_locale(source, Some("ja"))
        val selected = result.getOrElse(fail(result.message))

        Then("neutral and exactly Japanese-tagged content remain")
        selected.toPlainText should include ("neutral")
        selected.toPlainText should include ("日本語")
        And("English and en-US content are excluded")
        selected.toPlainText should not include "English"
        selected.toPlainText should not include "regional English"
      }
    }

    "select exact English content" which {
      "exclude Japanese and en-US branches without fallback" in {
        Given("the same bilingual document with an en-US branch")
        val source = _source

        When("the document is selected for the canonical English locale")
        val result = PdfOperationClass._select_locale(source, Some("en"))
        val selected = result.getOrElse(fail(result.message))

        Then("neutral and exactly English-tagged content remain")
        selected.toPlainText should include ("neutral")
        selected.toPlainText should include ("English")
        And("Japanese and en-US content are excluded")
        selected.toPlainText should not include "日本語"
        selected.toPlainText should not include "regional English"
      }
    }

    "select localized HEAD metadata when the body is neutral" in {
      Given("a document with localized title and organization metadata and a neutral body")
      val metadata = DocumentMetaData(
        title = Some(I18NFragment.createDox(List(
          Locale.ENGLISH -> List[Dox](Text("English title")),
          Locale.JAPANESE -> List[Dox](Text("日本語タイトル"))
        ))),
        organization = Some(I18NFragment.createDox(List(
          Locale.ENGLISH -> List[Dox](Text("English organization")),
          Locale.JAPANESE -> List[Dox](Text("日本語組織"))
        )))
      )
      val source = Document(
        Head(metadata = metadata),
        Body(List(Paragraph(List(Text("neutral body")))))
      )

      When("the document is selected for the canonical English locale")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message)) match {
        case document: Document => document
        case _ => fail("Expected localized selection to preserve document HEAD")
      }

      Then("the neutral body remains in the selected document")
      selected.toPlainText should include ("neutral body")
      And("localized HEAD title and organization stay English through downstream Japanese metadata accessors")
      selected.head.metadata.getTitleString(Locale.JAPANESE) shouldBe Some("English title")
      selected.head.metadata.getOrganizationString(Locale.JAPANESE) shouldBe Some("English organization")
    }

    "recognize source content stored in an I18NFragment" in {
      Given("a document whose bilingual source is held by canonical I18NFragment locales")
      val source = Document(Head.empty, Body(List(_bilingual_fragment)))

      When("the document is selected for the canonical English locale")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message))

      Then("the selected fragment content remains available")
      selected.toPlainText should include ("fragment English")
      selected.toPlainText should not include "fragment Japanese"
    }

    "exclude a regional I18NFragment from an exact English selection" in {
      Given("an ordinary English branch and an en-US I18NFragment branch")
      val source = Document(Head.empty, Body(List(
        Paragraph(List(Text("ordinary English")), VectorMap("lang" -> "en")),
        _regional_fragment
      )))

      When("the document is selected for the canonical English locale")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message))

      Then("the ordinary English branch remains")
      selected.toPlainText should include ("ordinary English")
      And("the regional fragment is not used as same-language fallback")
      selected.toPlainText should not include "fragment regional English"
    }

    "report an unavailable locale when an I18NFragment lacks the selection" in {
      Given("a document whose only localized fragment content is en-US")
      val source = Document(Head.empty, Body(List(
        _regional_fragment
      )))

      When("the canonical English locale is selected")
      val result = PdfOperationClass._select_locale(source, Some("en"))

      Then("the structured diagnostic identifies unavailable selected source content")
      result.isError shouldBe true
      result.message should include ("pdf.locale.unavailable")
    }

    "reject regional-only I18NString I18NFragment construction" in {
      Given("an en-US-only fragment built through the I18NString route")
      val regional = Locale.forLanguageTag("en-US")
      val fromi18nstring = I18NFragment.create(I18NString(Map(
        regional -> "regional I18NString English"
      )))
      val stringsource = Document(Head.empty, Body(List(fromi18nstring)))

      When("canonical English PDF selection is requested")
      val stringresult = PdfOperationClass._select_locale(stringsource, Some("en"))

      Then("the regional-only constructor route reports unavailable exact English content")
      stringresult.isError shouldBe true
      stringresult.message should include ("pdf.locale.unavailable")
    }

    "select distinct canonical I18NString English without regional fallback" in {
      Given("an I18NString fragment with canonical English and distinct en-US sources")
      val fragment = I18NFragment.create(I18NString(
        "",
        "canonical I18NString English",
        "",
        Map(Locale.forLanguageTag("en-US") -> "regional I18NString English")
      ))
      val source = Document(Head.empty, Body(List(fragment)))

      When("canonical English PDF selection is requested")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message))

      Then("the explicit canonical source remains available")
      result.isError shouldBe false
      selected.toPlainText should include ("canonical I18NString English")
      And("the regional source is excluded from strict English selection")
      selected.toPlainText should not include "regional I18NString English"
    }

    "select one exact canonical I18NFragment without cross-locale leakage" in {
      Given("a document containing only one canonical English I18NFragment")
      val single = Document(Head.empty, Body(List(_english_fragment)))

      When("English locale selection is requested")
      val english = PdfOperationClass._select_locale(single, Some("en"))
      val selected = english.getOrElse(fail(english.message))
      And("the opposite canonical locale selection is requested")
      val japanese = PdfOperationClass._select_locale(single, Some("ja"))

      Then("the exact English fragment is selected")
      selected.toPlainText should include ("fragment English")
      And("the opposite canonical selection is unavailable")
      japanese.isError shouldBe true
      japanese.message should include ("pdf.locale.unavailable")
    }

    "not leak one canonical I18NFragment into another selected locale" in {
      Given("one English and one Japanese canonical I18NFragment")
      val source = Document(Head.empty, Body(List(_english_fragment, _japanese_fragment)))

      When("the Japanese locale is selected")
      val result = PdfOperationClass._select_locale(source, Some("ja"))
      val selected = result.getOrElse(fail(result.message))

      Then("only the Japanese fragment remains")
      selected.toPlainText should include ("fragment Japanese")
      selected.toPlainText should not include "fragment English"
    }

    "select nested bilingual section titles as an effective exact source" in {
      Given("a title-only document with neutral, canonical, and regional title content inside a span")
      val title = List[Inline](Span(List(
        Text("neutral title "),
        I18NFragment.createDox(List(
          Locale.ENGLISH -> List[Dox](Text("English title")),
          Locale.JAPANESE -> List[Dox](Text("日本語タイトル")),
          Locale.forLanguageTag("en-US") -> List[Dox](Text("regional title"))
        ))
      )))
      val source = Document(Head.empty, Body(List(Section(title, Nil))))

      When("the canonical English title is selected")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message))
      val selectedtitle = Dox.toPlainText(_first_section(selected).title)

      Then("title-only exact content makes the document available")
      selectedtitle should include ("neutral title")
      selectedtitle should include ("English title")
      And("nested Japanese and regional English title content is excluded")
      selectedtitle should not include "日本語タイトル"
      selectedtitle should not include "regional title"
    }

    "select strict Value.I18N body and title values without a regional fallback" in {
      Given("body and title values with commons, canonical values, and an en-US-only value")
      val bodyvalue = _value(
        commons = Vector("common body"),
        localized = Map(
          Locale.ENGLISH -> Vector("English body"),
          Locale.JAPANESE -> Vector("日本語本文")
        )
      )
      val titlevalue = _value(
        commons = Vector("common title"),
        localized = Map(
          Locale.ENGLISH -> Vector("English title value"),
          Locale.JAPANESE -> Vector("日本語タイトル値")
        )
      )
      val regionalvalue = _value(
        commons = Vector("common regional"),
        localized = Map(Locale.forLanguageTag("en-US") -> Vector("regional value"))
      )
      val source = Document(Head.empty, Body(List(
        Paragraph(List(bodyvalue, regionalvalue)),
        Section(List(Span(List(titlevalue))), Nil)
      )))

      When("English is selected")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message))
      val selectedtitle = Dox.toPlainText(_first_section(selected).title)

      Then("commons and only the exact English value remain in the body")
      selected.toPlainText should include ("common body")
      selected.toPlainText should include ("English body")
      selected.toPlainText should include ("common regional")
      selected.toPlainText should not include "日本語本文"
      selected.toPlainText should not include "regional value"
      And("the same strict value selection applies recursively in the title")
      selectedtitle should include ("common title")
      selectedtitle should include ("English title value")
      selectedtitle should not include "日本語タイトル値"
    }

    "accept Value.I18N commons inside an exact selected fragment source" in {
      Given("an exact English fragment containing a common value and only regional localized values")
      val source = Document(Head.empty, Body(List(
        I18NFragment.createDox(List(
          Locale.ENGLISH -> List[Dox](_value(
            commons = Vector("common exact fragment value"),
            localized = Map(Locale.forLanguageTag("en-US") -> Vector("regional fragment value"))
          ))
        ))
      )))

      When("canonical English is selected")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message))

      Then("the common value is available because the fragment source is exactly English")
      selected.toPlainText should include ("common exact fragment value")
      selected.toPlainText should not include "regional fragment value"
    }

    "reject Value.I18N common values without an exact selected vector" in {
      Given("a document whose only localized Value.I18N entry is en-US")
      val source = Document(Head.empty, Body(List(
        Paragraph(List(_value(
          commons = Vector("common only"),
          localized = Map(Locale.forLanguageTag("en-US") -> Vector("regional only"))
        )))
      )))

      When("canonical English is selected")
      val result = PdfOperationClass._select_locale(source, Some("en"))

      Then("common output alone does not make the locale available")
      result.isError shouldBe true
      result.message should include ("pdf.locale.unavailable")
    }

    "reject blank exact source and exact descendants beneath an opposite-language ancestor" in {
      Given("one blank English fragment, empty English structural containers and Body, and an English descendant inside a Japanese container")
      val blank = Document(Head.empty, Body(List(
        Paragraph(List(Text("neutral"))),
        I18NFragment.createDox(List(Locale.ENGLISH -> List[Dox](Text("   "))))
      )))
      val oppositeancestor = Document(Head.empty, Body(List(
        Div(List(Paragraph(List(Text("hidden English")), VectorMap("lang" -> "en"))), VectorMap("lang" -> "ja"))
      )))
      val emptycontainers = Document(Head.empty, Body(List(
        Paragraph(Nil, VectorMap("lang" -> "en")),
        Div(Nil, VectorMap("lang" -> "en"))
      )))
      val emptybody = Document(Head.empty, Body(Nil, VectorMap("lang" -> "en")))

      When("canonical English selection is requested for each document")
      val blankresult = PdfOperationClass._select_locale(blank, Some("en"))
      val ancestorresult = PdfOperationClass._select_locale(oppositeancestor, Some("en"))
      val emptycontainerresult = PdfOperationClass._select_locale(emptycontainers, Some("en"))
      val emptybodyresult = PdfOperationClass._select_locale(emptybody, Some("en"))

      Then("no ineffective exact source makes either document available")
      blankresult.isError shouldBe true
      blankresult.message should include ("pdf.locale.unavailable")
      ancestorresult.isError shouldBe true
      ancestorresult.message should include ("pdf.locale.unavailable")
      And("empty exact structural containers do not make the locale available")
      emptycontainerresult.isError shouldBe true
      emptycontainerresult.message should include ("pdf.locale.unavailable")
      And("an empty exact Body does not make the locale available")
      emptybodyresult.isError shouldBe true
      emptybodyresult.message should include ("pdf.locale.unavailable")
    }

    "select author and explanation metadata while retaining a neutral body" in {
      Given("a neutral body and exact localized author and explanation metadata")
      val metadata = DocumentMetaData(
        author = Some(I18NFragment.createDox(List(
          Locale.ENGLISH -> List[Dox](Text("English author")),
          Locale.JAPANESE -> List[Dox](Text("日本語著者"))
        ))),
        explanation = Explanation(
          summary = Some(I18NFragment.createDox(List(
            Locale.ENGLISH -> List[Dox](Text("English summary")),
            Locale.JAPANESE -> List[Dox](Text("日本語概要"))
          )))
        )
      )
      val source = Document(Head(metadata = metadata), Body(List(
        Paragraph(List(Text("neutral body")))
      )))

      When("English metadata selection is requested")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message)) match {
        case m: Document => m
        case _ => fail("Expected localized selection to preserve document HEAD")
      }

      Then("the neutral body is retained")
      selected.toPlainText should include ("neutral body")
      And("author and explanation fields contain only exact English values")
      selected.head.metadata.author.map(_.distillStringDefault) shouldBe Some("English author")
      selected.head.metadata.summary.map(_.distillStringDefault) shouldBe Some("English summary")
    }

    "select an exact localized renderable image source" in {
      Given("canonical and regional English image leaves without textual selected content")
      val englishimage = ReferenceImg(
        new URI("english.png"),
        attributes = VectorMap("lang" -> "en")
      )
      val regionalimage = ReferenceImg(
        new URI("regional.png"),
        attributes = VectorMap("lang" -> "en-US")
      )
      val source = Document(Head.empty, Body(List(englishimage, regionalimage)))

      When("the canonical English locale is selected")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message))

      Then("the exact renderable image makes the selected source available")
      selected.find {
        case m: ReferenceImg => m.src == englishimage.src
        case _ => false
      } should not be empty
      And("the regional image is excluded without locale fallback")
      selected.find {
        case m: ReferenceImg => m.src == regionalimage.src
        case _ => false
      } shouldBe empty
    }

    "select every exact localized explanation field" in {
      Given("a neutral body and every explanation field in canonical English and Japanese")
      def _localized_(english: String, japanese: String): I18NFragment =
        I18NFragment.createDox(List(
          Locale.ENGLISH -> List[Dox](Text(english)),
          Locale.JAPANESE -> List[Dox](Text(japanese))
        ))
      val metadata: DocumentMetaData = DocumentMetaData(
        explanation = Explanation(
          headline = Some(_localized_("English headline", "日本語見出し")),
          brief = Some(_localized_("English brief", "日本語概要")),
          summary = Some(_localized_("English summary", "日本語要約")),
          description = Some(_localized_("English description", "日本語説明")),
          lead = Some(_localized_("English lead", "日本語導入")),
          `abstract` = Some(_localized_("English abstract", "日本語抄録")),
          remarks = Some(_localized_("English remarks", "日本語注記")),
          tooltip = Some(_localized_("English tooltip", "日本語ツールチップ"))
        )
      )
      val source = Document(Head.empty.withDocumentMetaData(metadata), Body(List(
        Paragraph(List(Text("neutral body")))
      )))

      When("canonical English metadata selection is requested")
      val result = PdfOperationClass._select_locale(source, Some("en"))
      val selected = result.getOrElse(fail(result.message)) match {
        case m: Document => m
        case _ => fail("Expected localized selection to preserve document HEAD")
      }
      val explanation = selected.head.metadata.explanation

      Then("every explanation field contains only its exact English source")
      List(
        explanation.headline -> "English headline",
        explanation.brief -> "English brief",
        explanation.summary -> "English summary",
        explanation.description -> "English description",
        explanation.lead -> "English lead",
        explanation.`abstract` -> "English abstract",
        explanation.remarks -> "English remarks",
        explanation.tooltip -> "English tooltip"
      ).foreach { case (actual, expected) =>
        actual.map(_.distillStringDefault) shouldBe Some(expected)
      }
    }

    "preserve the unfiltered source when the selector is omitted" in {
      Given("the bilingual document and no locale selector")
      val source = _source

      When("PDF locale selection is applied")
      val result = PdfOperationClass._select_locale(source, None)
      val unfiltered = result.getOrElse(fail(result.message))

      Then("the original document is returned unchanged")
      unfiltered shouldBe source
      unfiltered.toPlainText should include ("日本語")
      unfiltered.toPlainText should include ("English")
      unfiltered.toPlainText should include ("regional English")
    }

    "transport the locale option outside the public PDF command product" in {
      Given("a PDF request with the canonical English locale")
      val request = Request.create(PdfOperationClass.specification, Array("--locale", "en", "input.dox"))

      When("the request is interpreted at the command and operation boundaries")
      val command = PdfOperationClass.PdfCommand.cCreate(request)
      val selector = PdfOperationClass._locale_selector(request)

      Then("the command retains its baseline fields plus its default typesetting deadline")
      command.take.productArity shouldBe 17
      command.take.typesettingTimeoutMillis shouldBe 300000L
      And("the operation boundary transports the parsed locale separately")
      selector.take shouldBe Some("en")
      And("the one-argument language filter constructor remains source-compatible")
      new LanguageFilterTransformer(TreeTransformer.Context.default[Dox]).treeTransformerContext shouldBe
        TreeTransformer.Context.default[Dox]
    }

    "preserve legacy non-strict filtering through the public transformer constructor" in {
      Given("a public-create fragment with neutral, English, and regional English source")
      val fragment = I18NFragment.create(List[Dox](
        Text("neutral"),
        Span.create(Locale.ENGLISH, "English"),
        Span.create(Locale.forLanguageTag("en-US"), "regional English")
      ))
      val context = TreeTransformer.Context.default[Dox].
        withI18NContext(I18NContext.default.withLocale(Locale.JAPANESE))

      When("the public one-argument language filter transforms the fragment for Japanese")
      val selected = Dox.transform(
        Fragment(List(fragment)),
        new LanguageFilterTransformer(context)
      )

      Then("the established non-strict English fallback is retained")
      selected.toPlainText shouldBe "neutralEnglish"
      And("the regional English source does not displace that legacy fallback")
      selected.toPlainText should not include "regional English"
    }


    "report typed malformed locale diagnostics at the public request boundary" in {
      Given("public PDF requests with empty, whitespace, and uppercase locale values")
      val requests = List(
        Request.create(PdfOperationClass.specification, Array("--locale", "", "input.dox")),
        Request.create(PdfOperationClass.specification, Array("--locale", " en", "input.dox")),
        Request.create(PdfOperationClass.specification, Array("--locale", "EN", "input.dox"))
      )

      When("each public locale request is interpreted")
      val results = requests.map(PdfOperationClass._locale_selector)

      Then("each has the exact invalid-argument diagnostic")
      (results zip List(
        "pdf.locale.invalid: ",
        "pdf.locale.invalid:  en",
        "pdf.locale.invalid: EN"
      )).foreach { case (result, message) =>
        result.isError shouldBe true
        result.message shouldBe message
        result.code shouldBe 400
        result.conclusion.faults.faults.head shouldBe a [InvalidArgumentFault]
      }
    }

    "report a typed unsupported locale diagnostic at the public request boundary" in {
      Given("a public PDF request with a valid but unsupported locale")
      val request = Request.create(
        PdfOperationClass.specification,
        Array("--locale", "fr", "input.dox")
      )

      When("the public locale request is interpreted")
      val result = PdfOperationClass._locale_selector(request)

      Then("the exact unsupported-operation diagnostic is returned")
      result.isError shouldBe true
      result.message shouldBe "pdf.locale.unsupported: fr"
      result.code shouldBe 400
      result.conclusion.faults.faults.head shouldBe a [UnsupportedOperationFault]
    }

    "report a typed unavailable locale diagnostic for missing selected source" in {
      Given("a document with only Japanese language-tagged source")
      val source = Document(Head.empty, Body(List(
        Paragraph(List(Text("日本語")), VectorMap("lang" -> "ja"))
      )))

      When("canonical English selection is requested")
      val result = PdfOperationClass._select_locale(source, Some("en"))

      Then("the exact resource-not-found diagnostic is returned")
      result.isError shouldBe true
      result.message shouldBe "pdf.locale.unavailable: en"
      result.code shouldBe 500
      result.conclusion.faults.faults.head shouldBe a [ResourceNotFoundFault]
    }

    "report an invalid locale selector" in {
      Given("a selector with noncanonical casing")
      val source = _source

      When("the selector is interpreted")
      val result = PdfOperationClass._select_locale(source, Some("EN"))

      Then("the structured diagnostic identifies invalid PDF locale input")
      result.isError shouldBe true
      result.message should include ("pdf.locale.invalid")
    }

    "report a valid but unsupported locale selector" in {
      Given("a canonical BCP-47 locale outside the PDF delivery set")
      val source = _source

      When("the selector is interpreted")
      val result = PdfOperationClass._select_locale(source, Some("fr"))

      Then("the structured diagnostic identifies unsupported PDF locale input")
      result.isError shouldBe true
      result.message should include ("pdf.locale.unsupported")
    }

    "report the und locale selector as unsupported" in {
      Given("the canonical BCP-47 und locale")
      val source = _source

      When("the selector is interpreted")
      val result = PdfOperationClass._select_locale(source, Some("und"))

      Then("the structured diagnostic identifies an unsupported PDF locale")
      result.isError shouldBe true
      result.message should include ("pdf.locale.unsupported")
    }

    "report when selected language-tagged source content is unavailable" in {
      Given("a document containing only Japanese language-tagged content")
      val source = Document(Head.empty, Body(List(
        Paragraph(List(Text("日本語")), VectorMap("lang" -> "ja"))
      )))

      When("English locale selection is requested")
      val result = PdfOperationClass._select_locale(source, Some("en"))

      Then("the structured diagnostic identifies unavailable selected source content")
      result.isError shouldBe true
      result.message should include ("pdf.locale.unavailable")
    }
  }

  private lazy val _source: Document = Document(Head.empty, Body(List(
    Paragraph(List(Text("neutral"))),
    Paragraph(List(Text("日本語")), VectorMap("lang" -> "ja")),
    Paragraph(List(Text("English")), VectorMap("lang" -> "en")),
    Paragraph(List(Text("regional English")), VectorMap("lang" -> "en-US"))
  )))

  private def _assert_pdf_diagnostic_projection(
    diagnostic: StructuredRenderingDiagnostic,
    expectedcode: String,
    expectedstage: String,
    expectedsource: Option[String],
    expectedtoken: Option[String],
    expectedcause: String,
    expectedterminal: Boolean,
    expectedretryable: Boolean
  ): Unit = {
    val record = diagnostic.toIRecord
    val json = diagnostic.toJson

    record shouldBe IRecord.data(
      "code" -> expectedcode,
      "stage" -> expectedstage,
      "sourceIdentity" -> expectedsource,
      "line" -> None,
      "column" -> None,
      "tokenContext" -> expectedtoken,
      "cause" -> expectedcause,
      "terminal" -> expectedterminal,
      "retryable" -> expectedretryable
    )
    json shouldBe Json.obj(
      "code" -> Json.fromString(expectedcode),
      "stage" -> Json.fromString(expectedstage),
      "sourceIdentity" -> expectedsource.map(Json.fromString).getOrElse(Json.Null),
      "line" -> Json.Null,
      "column" -> Json.Null,
      "tokenContext" -> expectedtoken.map(Json.fromString).getOrElse(Json.Null),
      "cause" -> Json.fromString(expectedcause),
      "terminal" -> Json.fromBoolean(expectedterminal),
      "retryable" -> Json.fromBoolean(expectedretryable)
    )
  }

  private def _pdf_request(
    input: java.nio.file.Path,
    latexmk: java.nio.file.Path,
    locale: Option[String]
  ): Request = {
    val localeargs = locale.toVector.flatMap(value => Vector("--locale", value))
    val args = Vector(
      "--renderer", "latex",
      "--dependency-mode", "local",
      "--latexmk", latexmk.toString
    ) ++ localeargs ++ Vector(input.toString)
    Request.create(PdfOperationClass.specification, args.toArray)
  }

  private def _missing_markdown_image_failure(renderer: String, rendereroption: String): RuntimeException = {
    val root = Files.createTempDirectory("smartdox-pdf-markdown-image-missing")
    val input = root.resolve("missing.md")
    Files.write(input, "![missing](images/missing.png)".getBytes("UTF-8"))
    val request = Request.create(
      PdfOperationClass.specification,
      Array(
        "--renderer",
        renderer,
        "--dependency-mode",
        "local",
        rendereroption,
        root.resolve("unavailable-renderer").toString,
        input.toString
      )
    )
    val command = PdfOperationClass.PdfCommand.create(request)
    try {
      intercept[RuntimeException] {
        PdfOperationClass.execute(Environment.createJaJp(), command)
      }
    } finally {
      IoUtils.removeDirectory(root.toFile)
    }
  }

  private def _site_resolution(
    input: java.nio.file.Path,
    root: java.nio.file.Path,
    config: java.nio.file.Path
  ) = {
    val request = Request.create(
      PdfOperationClass.specification,
      Array(
        "--site-root", root.toString,
        "--site-config", config.toString,
        input.toString
      )
    )
    val command = PdfOperationClass.PdfCommand.create(request)
    val parsed = PdfOperationClass._parse_pdf_input(input.toFile)
    PdfOperationClass._resolve_site_links(
      GeneratorContext.create(Environment.createJaJp()),
      command,
      parsed.dox,
      None
    )
  }

  private def _write_site_publication_fixture(root: java.nio.file.Path): java.nio.file.Path = {
    val source = root.resolve("development-process/domain-modeling.dox/index.dox")
    _write(root.resolve("site.conf"),
      """|site.metadata.url = "https://www.simplemodeling.org/"
         |site.output.locale_mode = "multi_locale_subdirs"
         |site.output.default_locale = "ja"
         |""".stripMargin)
    _write(source,
      """|Domain Modeling｜ドメインモデリング
         |====================================
         |
         |# HEAD
         |
         |status=published
         |
         |# Body
         |
         |Previous article: site:[literate-modeling.dox]
         |""".stripMargin)
    _write(root.resolve("development-process/domain-modeling.dox/review/private.md"), "~~~text")
    _write(root.resolve("development-process/domain-modeling.dox/review/private.dox"), "~~~text")
    _write(root.resolve("development-process/literate-modeling.dox"),
      """|Literate Modeling｜文芸モデリング
         |==================================
         |
         |# HEAD
         |
         |status=published
         |
         |# Body
         |
         |Target article.
         |""".stripMargin)
    source
  }

  private def _write(path: java.nio.file.Path, content: String): Unit = {
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, content.getBytes("UTF-8"))
  }

  private def _write_renderer_marker(renderer: java.nio.file.Path, marker: java.nio.file.Path): Unit =
    Files.write(
      renderer,
      s"#!/bin/sh\ntouch '${marker.toString}'\nexit 1\n".getBytes("UTF-8")
    )

  private def _write_local_latexmk(latexmk: java.nio.file.Path): Unit =
    Files.write(
      latexmk,
      """|#!/bin/sh
         |set -eu
         |outdir=
         |tex=
         |for arg in "$@"; do
         |  case "$arg" in
         |    -outdir=*) outdir="${arg#-outdir=}" ;;
         |    *.tex) tex="$arg" ;;
         |  esac
         |done
         |test -n "$outdir"
         |test -n "$tex"
         |actualdir=$(pwd -P)
         |expectedtexdir=$(cd "$(dirname "$tex")" && pwd -P)
         |test "$actualdir" = "$expectedtexdir"
         |if grep -F '!' "$tex" >/dev/null; then
         |  exit 1
         |fi
         |found_png=
         |for png in ./*.png; do
         |  if test -f "$png"; then
         |    found_png=$png
         |    break
         |  fi
         |done
         |test -n "$found_png"
         |basename="${tex##*/}"
         |printf '%%PDF-1.4\\n' > "$outdir/${basename%.tex}.pdf"
         |""".stripMargin.getBytes("UTF-8")
    )

  private def _write_nonzero_latexmk(latexmk: java.nio.file.Path): Unit =
    Files.write(
      latexmk,
      "#!/bin/sh\nprintf '%s\\n' 'untrusted renderer stderr detail' >&2\nexit 17\n".getBytes("UTF-8")
    )

  private def _write_no_output_latexmk(latexmk: java.nio.file.Path): Unit =
    Files.write(
      latexmk,
      "#!/bin/sh\nexit 0\n".getBytes("UTF-8")
    )

  private def _write_no_output_renderer(renderer: java.nio.file.Path): Unit =
    Files.write(
      renderer,
      "#!/bin/sh\nexit 0\n".getBytes("UTF-8")
    )

  private def _write_timeout_latexmk(
    latexmk: java.nio.file.Path,
    childpid: java.nio.file.Path
  ): Unit = {
    val script = Vector(
      "#!/bin/sh",
      "set -eu",
      "sleep 30 &",
      "child=$!",
      "printf '%s\\n' \"$child\" > '" + childpid.toString + "'",
      "wait \"$child\""
    ).mkString("", "\n", "\n")
    Files.write(latexmk, script.getBytes("UTF-8"))
  }

  private def _write_interruption_timeout_latexmk(
    latexmk: java.nio.file.Path,
    childpid: java.nio.file.Path,
    cleanupmarker: java.nio.file.Path
  ): Unit = {
    val script = Vector(
      "#!/bin/sh",
      "set -eu",
      "(",
      "  trap 'touch \"" + cleanupmarker.toString + "\"; while :; do :; done' TERM",
      "  while :; do sleep 30; done",
      ") &",
      "child=$!",
      "printf '%s\\n' \"$child\" > '" + childpid.toString + "'",
      "wait \"$child\""
    ).mkString("", "\n", "\n")
    Files.write(latexmk, script.getBytes("UTF-8"))
  }

  private def _write_symlink_output_chrome(
    renderer: java.nio.file.Path,
    marker: java.nio.file.Path
  ): Unit = {
    val script = Vector(
      "#!/bin/sh",
      "set -eu",
      "out=",
      "for arg in \"$@\"; do",
      "  case \"$arg\" in",
      "    --print-to-pdf=*) out=\"${arg#--print-to-pdf=}\" ;;",
      "  esac",
      "done",
      "test -n \"$out\"",
      "payload=\"$(dirname \"$out\")/renderer-payload.pdf\"",
      "printf '%%PDF-1.4\\n' > \"$payload\"",
      "rm -f \"$out\"",
      "ln -s \"$payload\" \"$out\"",
      "test -L \"$out\"",
      s"touch '${marker.toString}'"
    ).mkString("", "\n", "\n")
    Files.write(renderer, script.getBytes("UTF-8"))
  }

  private def _write_symlink_output_docker(
    docker: java.nio.file.Path,
    marker: java.nio.file.Path
  ): Unit = {
    val script = Vector(
      "#!/bin/sh",
      "set -eu",
      "inputdir=",
      "outputdir=",
      "for arg in \"$@\"; do",
      "  case \"$arg\" in",
      "    *:/work/input:ro) inputdir=\"${arg%:/work/input:ro}\" ;;",
      "    *:/work/output) outputdir=\"${arg%:/work/output}\" ;;",
      "  esac",
      "done",
      "test -d \"$inputdir\"",
      "test -d \"$outputdir\"",
      "tex=",
      "for candidate in \"$inputdir\"/*.tex; do",
      "  if test -f \"$candidate\"; then",
      "    tex=$candidate",
      "    break",
      "  fi",
      "done",
      "test -n \"$tex\"",
      "body=\"${tex##*/}\"",
      "body=\"${body%.tex}\"",
      "payload=\"$outputdir/docker-latex-payload.pdf\"",
      "generated=\"$outputdir/$body.pdf\"",
      "printf '%%PDF-1.4\\n' > \"$payload\"",
      "ln -s \"$payload\" \"$generated\"",
      "test -L \"$generated\"",
      s"touch '${marker.toString}'"
    ).mkString("", "\n", "\n")
    Files.write(docker, script.getBytes("UTF-8"))
  }

  private lazy val _bilingual_fragment: I18NFragment = I18NFragment.createDox(List(
    Locale.ENGLISH -> List[Dox](Text("fragment English")),
    Locale.JAPANESE -> List[Dox](Text("fragment Japanese"))
  ))

  private lazy val _english_fragment: I18NFragment = I18NFragment.createDox(List(
    Locale.ENGLISH -> List[Dox](Text("fragment English"))
  ))

  private lazy val _japanese_fragment: I18NFragment = I18NFragment.createDox(List(
    Locale.JAPANESE -> List[Dox](Text("fragment Japanese"))
  ))

  private lazy val _regional_fragment: I18NFragment = I18NFragment.createDox(List(
    Locale.forLanguageTag("en-US") -> List[Dox](Text("fragment regional English"))
  ))

  private def _value(
    commons: Vector[String],
    localized: Map[Locale, Vector[String]]
  ): Value.I18N =
    Value.I18N(I18NHangar(localized, commons))

  private def _first_section(p: Dox): Section =
    Dox.findSection(p).getOrElse(fail("Expected a selected section"))
}
