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
 * @version Nov.  8, 2020
 * @author  ASAMI, Tomoharu
 */
case class Dox2HtmlTransformer(context: Context) {
  def transform(in: Dox): ParseResult[String] = {
    for {
      dom <- new Dox2DomHtmlTransformer(context).transformG(in)
      r <- ParseResult(DomUtils.toText(dom))
    } yield r
  }
}
