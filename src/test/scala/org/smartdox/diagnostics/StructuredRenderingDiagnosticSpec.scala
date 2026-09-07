package org.smartdox.diagnostics

import io.circe.Json
import org.goldenport.extension.IRecord
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/*
 * @since   Sep.  7, 2026
 * @version Sep.  7, 2026
 */
class StructuredRenderingDiagnosticSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "StructuredRenderingDiagnostic" should {
    "project one syntax diagnostic consistently to CLI Record and JSON" in {
      Given("a source-located terminal document syntax diagnostic")
      val diagnostic = StructuredRenderingDiagnostic.documentSyntaxInvalid(
        sourceIdentity = "articles/diagnostic.dox",
        line = 1,
        column = 1,
        tokenContext = "~~~text"
      )

      When("the typed diagnostic is projected for CLI Record and JSON consumers")
      val cli = diagnostic.toCliText
      val record = diagnostic.toIRecord
      val json = diagnostic.toJson

      Then("all projections retain the same stable diagnostic field values")
      cli should include ("document.syntax.invalid")
      cli should include ("stage=parse")
      cli should include ("source=articles/diagnostic.dox")
      cli should include ("line=1")
      cli should include ("column=1")
      cli should include ("token=~~~text")
      record shouldBe IRecord.data(
        "code" -> "document.syntax.invalid",
        "stage" -> "parse",
        "sourceIdentity" -> Some("articles/diagnostic.dox"),
        "line" -> Some(1),
        "column" -> Some(1),
        "tokenContext" -> Some("~~~text"),
        "cause" -> "unclosed-inline-delimiter",
        "terminal" -> true,
        "retryable" -> false
      )
      json shouldBe Json.obj(
        "code" -> Json.fromString("document.syntax.invalid"),
        "stage" -> Json.fromString("parse"),
        "sourceIdentity" -> Json.fromString("articles/diagnostic.dox"),
        "line" -> Json.fromInt(1),
        "column" -> Json.fromInt(1),
        "tokenContext" -> Json.fromString("~~~text"),
        "cause" -> Json.fromString("unclosed-inline-delimiter"),
        "terminal" -> Json.fromBoolean(true),
        "retryable" -> Json.fromBoolean(false)
      )
    }
  }
}
