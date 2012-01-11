package org.smartdox.transformer

import scala.util.parsing.combinator.Parsers
import org.smartdox._
import scalaz._
import Scalaz._
import scala.collection.mutable.ArrayBuffer
import java.net.URI
import Dox._
import scala.util.parsing.input.Reader

/*
 * @since   Jan. 12, 2012
 * @version Jan. 12, 2012
 * @author  ASAMI, Tomoharu
 */
trait Dox2StringTransformer extends DoxTransformer {
  type Out = String

  def document_Out(d: Document): String = {
    ""
  }
}
