package org.smartdox.metadata

import scalaz._
import Scalaz._
import java.net.URI
import java.util.Locale
import org.goldenport.i18n.I18NString
import org.goldenport.collection.VectorMap
import org.smartdox._
import org.smartdox.doxsite.Page

/*
 * @since   Feb. 23, 2025
 *  version Feb. 24, 2025
 *  version Mar.  9, 2025
 * @version Aug. 23, 2025
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

  case class Term(
    name: I18NString,
    aliases: VectorMap[Locale, Vector[String]] = VectorMap.empty
  ) {
    val key: String = name.en

    val candidates: Vector[String] = {
      val a = name.terms
      val b = aliases.valueVector.flatten
      (a ++ b).distinct
    }

    def isAvailable(tokens: Term.Tokens): Boolean = tokens.contains(candidates)

    def toTitle: List[Inline] =
      if (name.isSimple)
        List(Text(name.en))
      else
        List(I18NFragment.create(name))
  }
  object Term {
    case class Tokens(tokens: Vector[String]) {
      def contains(ps: Vector[String]) = tokens.intersect(ps).nonEmpty
    }

    def create(name: String): Term = Term(I18NString(name))
  }

  sealed trait Definition {
    def term: Term
    def page: URI
    def getId: Option[Dox.Id]
    def description: Dox

    def isAvailable(tokens: Term.Tokens): Boolean = {
      term.isAvailable(tokens)
    }

    def candidates: Vector[String] = term.candidates

    def createPage: Page = {
      val title = term.toTitle
      val explanation = Explanation.empty
      val meta = DocumentMetaData.create(title, explanation)
      val head = Head(metadata = meta)
      val body = Body(List(description))
      val dox = Document(head, body)
      val name = term.key
      val lastmodefied = None
      Page(name, dox, None)
    }
  }
  object Definition {
    case class InDocument(
      term: Term,
      page: URI,
      id: Dox.Id,
      description: Dox
    ) extends Definition {
      def getId = Some(id)
    }

    case class InGlossary(
      term: Term,
      page: URI,
      description: Dox,
      status: DocumentMetaData.Status
    ) extends Definition {
      def getId = None
    }
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
      val t = Term.create(term)
      val d = Definition.InDocument(t, uri, id, description)
      add(term, d)
    }

    def add(term: String, d: Definition): Unit = {
      val s = Slot(term, d)
      _slots = _slots |+| Map(term -> Vector(s))
    }

    def register(name: String, p: Document): Unit =
      for (term <- _make_term(p)) {
        val title = p.head.title
        val uri = new URI(s"glossary/$name.html")
        val status = p.head.metadata.status
        val dox = p.body.elements
        val d = Definition.InGlossary(term, uri, dox, status)
        add(name, d)
      }

    def register(name: String, tag: Tag.TagName, p: Document): Unit = {
      for (term <- _make_term(p)) {
        val uri = new URI(s"""glossary/${tag.name.replace(".", "/")}/$name.html""")
        val status = p.head.metadata.status
        val dox = p.body.elements
        val d = Definition.InGlossary(term, uri, dox, status)
        add(name, d)
      }
    }

    private def _make_term(p: Document): Option[Term] =
      for (title <- p.head.title) yield {
        Term(title.toI18NString)
      }
  }
  object Builder {
    case class Slot(term: String, definition: Definition)
  }
}
