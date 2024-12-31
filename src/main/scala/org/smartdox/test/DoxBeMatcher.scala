package org.smartdox.test

import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.smartdox._

/*
 * @since   Dec.  3, 2024
 * @version Dec. 23, 2024
 * @author  ASAMI, Tomoharu
 */
case class DoxBeMatcher(label: Option[String], exptected: Dox) extends BeMatcher[Dox] with DoxMatchResultHelper {
  def apply(actual: Dox): MatchResult = {
    val r = be(exptected)(actual)
    println(s"Dox: ${exptected == actual}")
    println(exptected.hashCode)
    println(actual.hashCode)
    println(exptected)
    println(actual)
    match_result(label, r)
  }
}

object DoxBeMatcher {
  trait Matchers {
  }

  def apply(label: String, expected: Dox): DoxBeMatcher = DoxBeMatcher(Some(label), expected)

  def apply(expected: Dox): DoxBeMatcher = DoxBeMatcher(None, expected)

  def run(expected: Dox, actual: Dox): MatchResult = DoxBeMatcher(expected)(actual)
}
