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
 * @since   Jan. 11, 2012
 * @version Jan. 11, 2012
 * @author  ASAMI, Tomoharu
 */
trait DoxTransformer extends Parsers {
  type Elem = Dox

  def parseDox(in: Dox) = {
    document(new DoxReader(in))
  } 

  def parseDoxZ(in: Dox) = parseDox(in) match {
    case s: Success[_] => s.get.success[String].liftFailNel
    case n: NoSuccess => n.msg.fail[Dox].liftFailNel
  }

  def document: Parser[Dox] = {
    sys.error("not implemented yet.")
  }
}
