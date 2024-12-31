package org.smartdox.structure.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.test.MatchResultHelper
import org.goldenport.test.MatchResultUtils.Implicits._
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
case class DescriptionDefinitionsBeMatcher(expected: DescriptionDefinitions) extends BeMatcher[DescriptionDefinitions] with DoxMatchResultHelper {
  def apply(actual: DescriptionDefinitions): MatchResult = {
    val s = match_be_dox("section", expected.section, actual.section)
    val d = match_be_description("description", expected.description, actual.description)
    val ds = match_be_definitions("definitions", expected.definitions, actual.definitions)
    Vector(s, d, ds).concatenate
  }
}

object DescriptionDefinitionsBeMatcher {
  trait Matchers {
    protected final def dox_description_definitions(
      dox: Dox,
      description: Description,
      definitions: Definition*
    ): DescriptionDefinitionsBeMatcher = DescriptionDefinitionsBeMatcher(
      DescriptionDefinitions(
        Dox.makeSection(dox),
        description,
        definitions.toVector
      )
    )

    protected final def dox_description_definitions(
      dox: Dox,
      name: String,
      paragraph: String,
      definitions: Definition*
    ): DescriptionDefinitionsBeMatcher = dox_description_definitions(
      dox,
      Description.nameParagraph(name, paragraph),
      definitions: _*
    )
  }
}
