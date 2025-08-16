package org.smartdox

import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.i18n.LocaleUtils

/*
 * @since   Aug. 16, 2025
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class I18NFragmentSpec extends AnyWordSpec with Matchers with ScalazMatchers {
  "I18NFragment" should {
    "typical" in {
      val a = List(Span.create(LocaleUtils.ja, "J"), Text(" "), Span.create(LocaleUtils.en, "E"))
      val b = I18NFragment.create(a)
      val s = b.toI18NString
      println(s.en)
      println(s.ja)
    }
  }
}

