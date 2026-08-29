package org.smartdox

import java.util.Locale

import org.scalatestplus.junit.JUnitRunner
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.i18n.{I18NString, LocaleUtils}

/*
 * @since   Aug. 16, 2025
 * @version Aug. 29, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class I18NFragmentSpec extends AnyWordSpec with Matchers with ScalazMatchers with GivenWhenThen with ScalaCheckPropertyChecks {
  "I18NFragment source construction" should {
    "preserve exact source membership without changing factory or Z legacy projection" in {
      Given("neutral source content and one or two canonical localized sources")
      val factory = I18NFragment.createDox(List(
        LocaleUtils.C -> List[Dox](Text("neutral")),
        LocaleUtils.en -> List[Dox](Text("English")),
        LocaleUtils.ja -> List[Dox](Text("日本語"))
      ))
      val one = I18NFragment.createDox(List(
        LocaleUtils.C -> List[Dox](Text("neutral")),
        LocaleUtils.en -> List[Dox](Text("English"))
      ))
      val zfragment = ((I18NFragment.Z() + Text("neutral")) + Span.create(LocaleUtils.en, "English")).r
      val zproduct = I18NFragment.Z(
        Vector[Dox](Text("product")),
        Map.empty[Locale, Vector[Dox]]
      )

      When("strict and legacy projections are requested")
      val factoryenglish = factory.distillExact(LocaleUtils.en)
      val factoryjapanese = factory.distillExact(LocaleUtils.ja)
      val onefallback = one.distill(LocaleUtils.ja)
      val zdefault = zfragment.distill(None)

      Then("only authored canonical sources are reported as exact")
      factory.hasExactSource(LocaleUtils.en) shouldBe true
      factory.hasExactSource(LocaleUtils.ja) shouldBe true
      one.hasExactSource(LocaleUtils.en) shouldBe true
      one.hasExactSource(LocaleUtils.ja) shouldBe false
      And("strict projection retains neutral content plus only the exact source")
      Dox.toPlainText(factoryenglish) shouldBe "neutralEnglish"
      Dox.toPlainText(factoryjapanese) shouldBe "neutral日本語"
      Dox.toPlainText(one.distillExact(LocaleUtils.ja)) shouldBe "neutral"
      And("factory defaults and locale fallback retain their legacy container behavior")
      one.distillStringDefault shouldBe "neutral"
      onefallback.map(_.toPlainText).mkString shouldBe "English"
      one.distillI18NFragmentDefault.distillStringDefault shouldBe "neutral"
      Dox.toPlainText(one.distillInlineContentsDefault) shouldBe "neutral"
      And("Z-based construction retains its legacy distributed neutral-plus-localized projection")
      Dox.toPlainText(zdefault) shouldBe "neutralEnglish"
      zfragment.distillString(LocaleUtils.ja) shouldBe "neutralEnglish"
      zfragment.toI18NString.en shouldBe "neutralEnglish"
      zfragment.toVectorMapString.get(LocaleUtils.en) shouldBe Some("neutralEnglish")
      And("Z retains its public two-field product ABI")
      zproduct.productArity shouldBe 2
    }

    "preserve public create provenance and serialized source boundaries" in {
      Given("a public-create fragment with neutral, English, and regional English elements")
      val fragment = I18NFragment.create(List[Dox](
        Text("neutral"),
        Span.create(LocaleUtils.en, "English"),
        Span.create(Locale.forLanguageTag("en-US"), "regional English")
      ))
      val buffer = new StringBuilder

      When("exact, legacy, and serialized projections are requested")
      val english = Dox.toPlainText(fragment.distillExact(LocaleUtils.en))
      val japaneselegacy = fragment.distillString(LocaleUtils.ja)
      fragment.printDox(buffer)
      val serialized = buffer.toString

      Then("the public construction route records only authored exact locales")
      fragment.hasExactSource(LocaleUtils.en) shouldBe true
      fragment.hasExactSource(LocaleUtils.ja) shouldBe false
      And("strict English output keeps neutral and exact English but excludes regional English")
      english should include ("neutral")
      english should include ("English")
      english should not include "regional English"
      And("the legacy Japanese fallback remains the established neutral-English projection")
      japaneselegacy shouldBe "neutralEnglish"
      And("neutral source is emitted once before and never inside localized tags")
      serialized.indexOf("neutral") should be < serialized.indexOf("<en>")
      serialized.lastIndexOf("neutral") shouldBe serialized.indexOf("neutral")
      serialized.substring(serialized.indexOf("<en>")) should not include "neutral"
    }

    "preserve regional I18NString provenance and canonical XML construction" in {
      Given("regional and simple I18NString inputs plus an XML input authored in canonical English")
      val regional = Locale.forLanguageTag("en-US")
      val fromi18nstring = I18NFragment.create(I18NString(Map(
        regional -> "regional I18NString English"
      )))
      val simple = I18NFragment.create(I18NString("plain"))
      val fromxml = I18NFragment.getC(
        "description",
        <metadata><description><en>canonical XML English</en></description></metadata>
      ).take.get

      When("canonical English exact and legacy projections are requested")
      val stringexact = Dox.toPlainText(fromi18nstring.distillExact(Locale.ENGLISH))
      val xmlexact = Dox.toPlainText(fromxml.distillExact(Locale.ENGLISH))
      val stringlegacy = fromi18nstring.distillString(Locale.ENGLISH)
      val simpleenglish = Dox.toPlainText(simple.distillExact(Locale.ENGLISH))

      Then("the regional I18NString does not report en-US as an exact English source")
      fromi18nstring.hasExactSource(Locale.ENGLISH) shouldBe false
      stringexact should not include "regional I18NString English"
      And("the established legacy I18NContainer projection remains available")
      stringlegacy should include ("regional I18NString English")
      And("a simple I18NString remains neutral rather than synthetic English or Japanese")
      simple.hasExactSource(Locale.ENGLISH) shouldBe false
      simple.hasExactSource(Locale.JAPANESE) shouldBe false
      simpleenglish shouldBe "plain"
      And("the supported canonical XML route remains an exact English source")
      fromxml.hasExactSource(Locale.ENGLISH) shouldBe true
      xmlexact should include ("canonical XML English")
    }

    "retain a distinct canonical I18NString field beside regional provenance" in {
      Given("an I18NString with explicit canonical English and distinct en-US values")
      val fragment = I18NFragment.create(I18NString(
        "",
        "canonical I18NString English",
        "",
        Map(Locale.forLanguageTag("en-US") -> "regional I18NString English")
      ))

      When("strict English projection is requested")
      val english = Dox.toPlainText(fragment.distillExact(Locale.ENGLISH))

      Then("the distinct canonical English field remains an exact authored source")
      fragment.hasExactSource(Locale.ENGLISH) shouldBe true
      english should include ("canonical I18NString English")
      And("the regional English map source is excluded from strict canonical selection")
      english should not include "regional I18NString English"
    }

    "serialize and transform only authored sources" in {
      Given("a neutral and English source fragment")
      val fragment = I18NFragment.createDox(List(
        LocaleUtils.C -> List[Dox](Text("neutral")),
        LocaleUtils.en -> List[Dox](Text("English"))
      ))

      When("it is serialized and transformed through inline, paragraph, map, trim, and prepend operations")
      val buffer = new StringBuilder
      fragment.printDox(buffer)
      val serialized = buffer.toString
      val prepended = "prefix" +: fragment
      val mapped = fragment.mapValues(_ :+ Text("!"))
      val trimmed = I18NFragment.createDox(List(
        LocaleUtils.C -> List[Dox](Text("neutral\nignored")),
        LocaleUtils.en -> List[Dox](Text("English\nignored"))
      )).trimSingleLine
      val inlines = fragment.makeInlines
      val paragraphs = fragment.makeParagraphs

      Then("neutral source is untagged and precedes only actual localized tags")
      serialized should include ("neutral")
      serialized should include ("<en>")
      serialized.indexOf("neutral") should be < serialized.indexOf("<en>")
      serialized should not include "<und>"
      serialized should not include "<ja>"
      And("prepend is held in the neutral stream once without creating a Japanese source")
      Dox.toPlainText(prepended.distillExact(LocaleUtils.en)) shouldBe "prefixneutralEnglish"
      Dox.toPlainText(prepended.distillExact(LocaleUtils.ja)) shouldBe "prefixneutral"
      prepended.hasExactSource(LocaleUtils.ja) shouldBe false
      And("map and trim preserve source-specific strict projection")
      Dox.toPlainText(mapped.distillExact(LocaleUtils.en)) shouldBe "neutral!English!"
      Dox.toPlainText(trimmed.distillExact(LocaleUtils.en)) shouldBe "neutral\nignoredEnglish\nignored"
      And("neutral inline and paragraph projections are not labelled as a language")
      inlines.collect { case m: Text => m.contents } should contain ("neutral")
      inlines.collect { case m: Span => m.getLanguage }.flatten.toSet shouldBe Set(LocaleUtils.en)
      paragraphs.filter(_.getLanguage.isEmpty).map(_.toPlainText.trim) should contain ("neutral")
      paragraphs.collect { case m if m.getLanguage.nonEmpty => m.getLanguage.get }.toSet shouldBe Set(LocaleUtils.en)
    }

    "keep exact selection independent of arbitrary neutral and localized text" in {
      val textgenerator = Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString)

      forAll(textgenerator, textgenerator) { (neutral, english) =>
        Given("generated nonempty neutral and English source text")
        val fragment = I18NFragment.createDox(List(
          LocaleUtils.C -> List[Dox](Text(neutral)),
          LocaleUtils.en -> List[Dox](Text(english))
        ))

        When("English and Japanese exact projections are compared")
        val selected = Dox.toPlainText(fragment.distillExact(LocaleUtils.en))
        val excluded = Dox.toPlainText(fragment.distillExact(LocaleUtils.ja))

        Then("only the exact English projection contains the English source")
        selected shouldBe s"$neutral$english"
        excluded shouldBe neutral
        fragment.hasExactSource(LocaleUtils.ja) shouldBe false
      }
    }
  }
}
