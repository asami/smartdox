package org.smartdox.metadata

import scalaz._
import Scalaz._
import java.net.URI
import org.goldenport.i18n.I18NString
import org.smartdox._
import org.smartdox.doxsite.LinkEnabler

/*
 * @since   Feb. 23, 2025
 *  version Feb. 24, 2025
 * @version Mar.  9, 2025
 * @author  ASAMI, Tomoharu
 */
case class Glossary(
  definitions: Vector[Glossary.Definition] = Vector.empty
) {
  import Glossary._

  def +(rhs: Glossary): Glossary = Glossary(definitions ++ rhs.definitions)

  def candidates(tokens: Term.Tokens): Vector[Definition] =
    definitions.filter(_.isAvailable(tokens))
}

object Glossary {
  val empty = Glossary()

  implicit val GlossaryMonoid: Monoid[Glossary] = new Monoid[Glossary] {
    def zero: Glossary = Glossary.empty
    def append(f1: Glossary, f2: => Glossary): Glossary = f1 + f2
  }

  case class Term(term: I18NString) {
    def isAvailable(tokens: Term.Tokens): Boolean =
      tokens.contains(term.terms)
  }
  object Term {
    case class Tokens(tokens: Vector[String]) {
      def contains(ps: Vector[String]) = tokens.intersect(ps).nonEmpty
    }
  }

  case class Definition(
    term: Term,
    page: URI,
    id: Dox.Id,
    description: Dox
  ) {
    def isAvailable(tokens: Term.Tokens): Boolean = {
      term.isAvailable(tokens)
    }

    def terms: Vector[String] = term.term.terms
  }

  class Builder() {
    import Builder._

    private var _slots: Map[String, Vector[Slot]] = Map.empty

    def build(): Glossary = {
      val xs = _slots.foldLeft(Vector.empty[Definition]) { (z, x) =>
        val (k, v) = x
        z :+ v.head.definition
      }
      Glossary(xs)
    }

    def add(term: String, uri: URI, id: Dox.Id, description: Dox): Unit = {
      val s = Slot(term, Definition(Term(I18NString(term)), uri, id, description))
      _slots = _slots |+| Map(term -> Vector(s))
    }
  }
  object Builder {
    case class Slot(term: String, definition: Definition)
  }
}
