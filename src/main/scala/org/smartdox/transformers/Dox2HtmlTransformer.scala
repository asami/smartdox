package org.smartdox.transformers

import scalaz._, Scalaz._
import java.net.URI
import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.Parsers
import scala.collection.mutable.ArrayBuffer
import org.w3c.dom.{Node}
import org.goldenport.parser.ParseResult
import org.goldenport.xml.dom.DomUtils
import org.smartdox._
import Dox._
import org.smartdox.generator.Context
import org.smartdox.transformer._

/*
 * @since   Nov.  3, 2020
 *  version Nov.  8, 2020
 *  version Jan.  1, 2021
 *  version Feb.  3, 2021
 * @version Apr.  3, 2025
 * @author  ASAMI, Tomoharu
 */
case class Dox2HtmlTransformer(
  context: Context,
  rule: Dox2HtmlTransformer.Rule
) extends HtmlTransformerBase {
  def isPretty = rule.isPretty
  def isDocument = rule.isDocument

  def transform(in: Dox): ParseResult[String] = {
    val domrule = rule.toDox2DomHtmlTransformerRule
    for {
      dom <- new Dox2DomHtmlTransformer(context, domrule).transformG(in)
      r <- ParseResult(to_html(dom))
    } yield r
  }
}

object Dox2HtmlTransformer {
  case class Rule(
    isPretty: Boolean = true,
    isDocument: Boolean = true,
    sectionBaseNumber: Option[Int] = None,
    tacticses: Vector[Dox2DomHtmlTransformer.Rule.Tactics] = Vector.empty,
    isDefaultCss: Boolean = true
  ) {
    def toDox2DomHtmlTransformerRule = Dox2DomHtmlTransformer.Rule(
      sectionBaseNumber,
      tacticses,
      isDefaultCss
    )
  }
  object Rule {
    val default = Rule()
    val noCss = default.copy(isDefaultCss = false)
  }
}
