package org.smartdox.structure.test

import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.test.MatchResultHelper
import org.goldenport.context.test.ConsequenceBeMatcher
import org.goldenport.values.Designation.test.DesignationBeMatcher
import org.smartdox._
import org.smartdox.structure._
import org.smartdox.test._

/*
 * @since   Dec. 21, 2024
 * @version Dec. 21, 2024
 * @author  ASAMI, Tomoharu
 */
case class DefinitionBeMatcher(label: Option[String], expected: Definition) extends BeMatcher[Definition] with DoxMatchResultHelper {
  def apply(actual: Definition): MatchResult = {
    match_be(label, expected, actual)
  }
}

object DefinitionBeMatcher {
  trait Matchers {
  }

  def apply(label: String, expected: Definition): DefinitionBeMatcher =
    DefinitionBeMatcher(Some(label), expected)
}
