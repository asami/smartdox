package org.smartdox.service.operations

import java.util.Locale
import java.net.URI
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.smartdox._
import org.goldenport.collection.VectorMap
import org.goldenport.cli.Request
import org.goldenport.context.{InvalidArgumentFault, ResourceNotFoundFault, UnsupportedOperationFault}
import org.goldenport.i18n.{I18NContext, I18NHangar, I18NString}
import org.goldenport.tree.TreeTransformer
import org.smartdox.metadata.{DocumentMetaData, Explanation}
import org.smartdox.transformers.LanguageFilterTransformer

/*
 * @since   Aug. 29, 2026
 * @version Aug. 29, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class PdfOperationClassSpec extends AnyWordSpec with Matchers with GivenWhenThen {
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

      Then("the command retains its baseline fourteen-field product")
      command.take.productArity shouldBe 14
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
