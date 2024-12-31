package org.smartdox.test

import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.test._
import org.smartdox._
import org.smartdox.structure._
import org.smartdox.structure.test._

/*
 * @since   Dec.  9, 2024
 * @version Dec. 24, 2024
 * @author  ASAMI, Tomoharu
 */
trait DoxMatchResultHelper extends MatchResultHelper {
  protected final def match_be_dox(label: String, expected: Dox, actual: Dox): MatchResult =
    DoxBeMatcher(label, expected)(actual)

  protected final def match_be_dox_option(
    label: String,
    expected: Option[Dox],
    actual: Option[Dox]
  ): MatchResult =
    OptionBeMatcher(label, DoxBeMatcher.run _)(expected)(actual)

  protected final def match_be_dox_list(
    label: String,
    expected: Seq[Dox],
    actual: Seq[Dox]
  ): MatchResult =
    SeqBeMatcher(label, DoxBeMatcher.run _)(expected)(actual)

  protected final def match_be_description(label: String, expected: Description, actual: Description): MatchResult =
    DescriptionBeMatcher(label, expected)(actual)

  protected final def match_be_description(expected: Description, actual: Description): MatchResult =
    DescriptionBeMatcher(expected)(actual)

  protected final def match_be_definition(label: String, expected: Definition, actual: Definition): MatchResult =
    DefinitionBeMatcher(label, expected)(actual)

  protected final def match_be_definitions(label: String, expected: Seq[Definition], actual: Seq[Definition]): MatchResult = {
    @annotation.tailrec
    def _go_(ps: Seq[(Definition, Definition)]): MatchResult = ps.toList match {
      case Nil => MatchResultUtils.success
      case x :: xs =>
        val mr = DefinitionBeMatcher(label, x._1)(x._2)
        if (mr.matches)
          _go_(xs)
        else
          mr
    }

    if (expected.length != actual.length) {
      match_error(s"""Exptected definitions length: ${expected.length} does not match actural definitions length: ${actual.length}""")
    } else {
      _go_(expected.zip(actual))
    }
  }
}
