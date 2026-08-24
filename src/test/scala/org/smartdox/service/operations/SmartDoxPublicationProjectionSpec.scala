package org.smartdox.service.operations

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/*
 * @since   Aug. 24, 2026
 * @version Aug. 24, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SmartDoxPublicationProjectionSpec
    extends AnyWordSpec
    with Matchers
    with GivenWhenThen
    with ScalaCheckPropertyChecks {
  "SmartDox publication projection" should {
    "project canonical .dox, .md, and .markdown sources and reject legacy suffixes" which {
      "project a canonical .dox source" in {
        Given("a canonical SmartDox source with an authored heading and body")
        val sourcename = "publication.dox"
        val source = _source

        When("the source is projected")
        val result = SmartDoxPublicationProjection.project(sourceName = sourcename, source = source)

        Then("the canonical source produces a projected result with attribution")
        result shouldBe a[SmartDoxPublicationProjection.Projected]
        val projected = result.asInstanceOf[SmartDoxPublicationProjection.Projected]
        projected.sourceName shouldBe sourcename
        projected.sourceDigest shouldBe _sha256(source)
        projected.html should include ("SmartDox Projection Heading")
        projected.html should include ("Authored publication body")
      }

      "project canonical Markdown suffixes with the same deterministic result shape" in {
        Given("one authored source and a canonical Markdown filename")
        val sourcename = "publication.md"
        val source = _source

        When("the Markdown source is projected")
        val result = SmartDoxPublicationProjection.project(sourceName = sourcename, source = source)

        Then("the result is projected and keeps the source attribution")
        result shouldBe a[SmartDoxPublicationProjection.Projected]
        val projected = result.asInstanceOf[SmartDoxPublicationProjection.Projected]
        projected.sourceName shouldBe sourcename
        projected.sourceDigest shouldBe _sha256(source)
      }

"reject canonical include directives before resolving unavailable targets" in {
        Given("canonical suffixes and an include directive targeting an unavailable file")
        val source = "# SmartDox Projection Heading\n\ninclude::/smartdox-projection-target-that-does-not-exist.dox[]"
        val sourcenames = Vector("publication.dox", "publication.md", "publication.markdown")

        When("each canonical source is projected")
        val results = sourcenames.map(sourcename => SmartDoxPublicationProjection.project(sourceName = sourcename, source = source))

        Then("every canonical source is rejected with source attribution and digest")
        sourcenames.zip(results).foreach { case (sourcename, result) =>
          result shouldBe a[SmartDoxPublicationProjection.Rejected]
          val rejected = result.asInstanceOf[SmartDoxPublicationProjection.Rejected]
          rejected.sourceName shouldBe sourcename
          rejected.sourceDigest shouldBe _sha256(source)
          rejected.sourceDigest should fullyMatch regex "[0-9a-f]{64}"
        }
      }

      "reject preserved legacy .adoc, .asciidoc, and .html material without rendering it" in {
        Given("legacy source material that remains preserved but is not canonical")
        val source = _source
        val sourcenames = Vector("publication.adoc", "publication.asciidoc", "publication.html")

        When("each legacy source is offered to the projection boundary")
        val results = sourcenames.map(sourcename => SmartDoxPublicationProjection.project(sourceName = sourcename, source = source))

        Then("every legacy input is rejected while retaining source attribution and digest")
        sourcenames.zip(results).foreach { case (sourcename, result) =>
          result shouldBe a[SmartDoxPublicationProjection.Rejected]
          val rejected = result.asInstanceOf[SmartDoxPublicationProjection.Rejected]
          rejected.sourceName shouldBe sourcename
          rejected.sourceDigest shouldBe _sha256(source)
        }
      }
    }

    "provide deterministic HTML and a recorded PDF renderer without invoking a renderer" which {
      "return equal results for repeated canonical projection" in {
        Given("the same canonical source projected twice")
        val sourcename = "repeat.md"
        val source = _source

        When("the canonical source is projected repeatedly")
        val first = SmartDoxPublicationProjection.project(sourceName = sourcename, source = source)
        val second = SmartDoxPublicationProjection.project(sourceName = sourcename, source = source)

        Then("the projections are equal and carry the LaTeX renderer profile")
        first shouldBe second
        val projected = first.asInstanceOf[SmartDoxPublicationProjection.Projected]
        projected.html should include ("SmartDox Projection Heading")
        projected.html should include ("Authored publication body")
        projected.pdfRenderer shouldBe PdfOperationClass.PdfRenderer.Latex
      }
    }

    "expose attributable UTF-8 SHA-256 digests for every boundary outcome" should {
      "report the lowercase digest for projected and rejected inputs" in {
        Given("one canonical source and one preserved legacy source")
        val source = "# UTF-8 見出し\n\n本文"
        val canonicalname = "utf8.markdown"
        val legacyname = "utf8.html"

        When("both sources are projected")
        val projectedresult = SmartDoxPublicationProjection.project(sourceName = canonicalname, source = source)
        val rejectedresult = SmartDoxPublicationProjection.project(sourceName = legacyname, source = source)

        Then("both outcomes expose their source name and lowercase UTF-8 SHA-256 digest")
        val projected = projectedresult.asInstanceOf[SmartDoxPublicationProjection.Projected]
        val rejected = rejectedresult.asInstanceOf[SmartDoxPublicationProjection.Rejected]
        projected.sourceName shouldBe canonicalname
        projected.sourceDigest shouldBe _sha256(source)
        rejected.sourceName shouldBe legacyname
        rejected.sourceDigest shouldBe _sha256(source)
        projected.sourceDigest should fullyMatch regex "[0-9a-f]{64}"
        rejected.sourceDigest should fullyMatch regex "[0-9a-f]{64}"
      }
    }

    "project every canonical suffix" in {
      val canonicalsuffixgenerator = Gen.oneOf(".dox", ".md", ".markdown")

      forAll(canonicalsuffixgenerator) { suffix =>
        Given("a finite generated canonical suffix and an authored source")
        val sourcename = s"generated$suffix"
        val source = _source

        When("the generated canonical source is projected")
        val result = SmartDoxPublicationProjection.project(sourceName = sourcename, source = source)

        Then("the result has the deterministic projected shape")
        result shouldBe a[SmartDoxPublicationProjection.Projected]
        val projected = result.asInstanceOf[SmartDoxPublicationProjection.Projected]
        projected.sourceName shouldBe sourcename
        projected.sourceDigest shouldBe _sha256(source)
        projected.html should include ("SmartDox Projection Heading")
        projected.pdfRenderer shouldBe PdfOperationClass.PdfRenderer.Latex
      }
    }

    "reject parser-recognized Org include directive variants before resolving unavailable targets" in {
      Given("Org include directive variants and the canonical source suffixes")
      val sources = Vector(
        "#+INCLUDE target.dox",
        "#+INCLUDE: target.dox",
        "#+include :   target.dox",
        "#+INCLUDE\ttarget.dox"
      )
      val sourcenames = Vector("publication.dox", "publication.md", "publication.markdown")
      val cases = sources.flatMap(source => sourcenames.map(sourcename => (source, sourcename)))

      When("each variant and canonical source is projected")
      val results = cases.map { case (source, sourcename) =>
        (source, sourcename, SmartDoxPublicationProjection.project(sourceName = sourcename, source = source))
      }

      Then("every variant is rejected with source attribution and lowercase SHA-256 digest")
      results.foreach { case (source, sourcename, result) =>
        result shouldBe a[SmartDoxPublicationProjection.Rejected]
        val rejected = result.asInstanceOf[SmartDoxPublicationProjection.Rejected]
        rejected.sourceName shouldBe sourcename
        rejected.sourceDigest shouldBe _sha256(source)
        rejected.sourceDigest should fullyMatch regex "[0-9a-f]{64}"
      }
    }
  }

  private lazy val _source: String = "# SmartDox Projection Heading\n\nAuthored publication body"

  private def _sha256(source: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(source.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"$byte%02x")
      .mkString
}
