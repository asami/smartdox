package org.smartdox.parser

import scalaz._, Scalaz._, Validation._, Tree._
import org.smartdox._
import Dox._

/**
 * @since   Jul.  1, 2017
 * @version Aug. 28, 2017
 * @author  ASAMI, Tomoharu
 */
class Dox2Parser() {
  import Dox2Parser._

  def parse(s: String): ParseResult = ???
}

object Dox2Parser {
  case class ParseError()
  case class ParseWarning()

  case class ParseResult(
    errors: Vector[ParseError],
    warnings: Vector[ParseWarning],
    doc: Dox
  )

  sealed trait ParseState {

  }
}
