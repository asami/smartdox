package org.smartdox.metadata

import org.goldenport.i18n.I18NString
import org.smartdox._

/*
 * @since   Mar.  4, 2025
 * @version Mar.  4, 2025
 * @author  ASAMI, Tomoharu
 */
case class Index(
  definitions: Vector[Index.Definition] = Vector.empty
) {

}

object Index {
  val empty = Index()

  case class Term(term: I18NString) {
  }

  case class Definition(
    term: Term,
    description: Dox
  )

  case class Builder() {
    def build(): Index = ???
  }
}
