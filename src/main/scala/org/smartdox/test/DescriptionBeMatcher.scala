package org.smartdox.test

import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.test.MatchResultHelper
import org.goldenport.context.test.ConsequenceBeMatcher
import org.goldenport.values.Designation.test.DesignationBeMatcher
import org.smartdox._
import org.smartdox.test._

/*
 * @since   Dec.  3, 2024
 * @version Dec. 25, 2024
 * @author  ASAMI, Tomoharu
 */
case class DescriptionBeMatcher(label: Option[String], expected: Description) extends BeMatcher[Description] with DoxMatchResultHelper {
  def apply(actual: Description): MatchResult = {
    val d = DesignationBeMatcher(expected.designation)(actual.designation)
    val t: MatchResult = match_be_option[Dox]("titleOption")(DoxBeMatcher.run, expected.titleOption, actual.titleOption)
    val r: MatchResult = be(expected.resume)(actual.resume)
    val c: MatchResult = match_be_dox("content", expected.content, actual.content)
    println(d)
    println(t)
    println(r)
    println(c)
    println(expected.content)
    println(actual.content)
    match_result(label, d, t, r, c)
  }
}

object DescriptionBeMatcher {
  trait Matchers {
    protected final def dox_description(p: Description) = DescriptionBeMatcher(p)

    protected final def dox_consequence_description(p: Description) = ConsequenceBeMatcher(DescriptionBeMatcher(p))
  }

  def apply(label: String, expected: Description): DescriptionBeMatcher =
    DescriptionBeMatcher(Some(label), expected)

  def apply(expected: Description): DescriptionBeMatcher =
    DescriptionBeMatcher(None, expected)
}
