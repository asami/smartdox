package org.smartdox.metadata

import scalaz.{Value => _, _}
import Scalaz._
import java.net.URI
import java.util.Locale
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NHangar
import org.goldenport.i18n.LocaleUtils
import org.goldenport.collection.VectorMap
import org.goldenport.tree.TreeNode
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.metadata.Notices.Notice
import org.smartdox.doxsite.Node
import org.smartdox.doxsite.Page
import org.smartdox.structure.StructureObject

/*
 * @since   Feb. 23, 2025
 *  version Feb. 24, 2025
 *  version Mar.  9, 2025
 *  version Aug. 31, 2025
 * @version Sep.  6, 2025
 * @author  ASAMI, Tomoharu
 */
case class Glossary(
  definitions: Vector[Glossary.Definition] = Vector.empty
) {
  import Glossary._

  def +(rhs: Glossary): Glossary = Glossary(definitions ++ rhs.definitions)

  def candidates(tokens: Term.Tokens): Vector[Definition] =
    definitions.filter(_.isAvailable(tokens))

  def toHistory: History = {
    val slots = definitions.flatMap(_.toHistorySlot)
    History(slots)
  }
}

object Glossary {
  final val PROP_DEFINITION = "definition"
  final val PROP_ALIASES = "aliases"
  final val PROP_ACRONYM = "acronym"
  final val PROP_REFERENCE = "reference"

  val empty = Glossary()

  implicit val GlossaryMonoid: Monoid[Glossary] = new Monoid[Glossary] {
    def zero: Glossary = Glossary.empty
    def append(f1: Glossary, f2: => Glossary): Glossary = f1 + f2
  }

  case class Term(
    name: I18NString,
    aliases: I18NHangar[String] = I18NHangar.empty,
    acronym: Option[String] = None,
    summary: Option[I18NString] = None
  ) {
    val key: String = name.en

    val candidates: Vector[String] = {
      val a = name.terms
      val b = aliases.valueVector
      (a ++ b).distinct
    }

    def isAvailable(tokens: Term.Tokens): Boolean = tokens.contains(candidates)

    def toTitle: List[Inline] =
      if (name.isSimple)
        List(Text(name.en))
      else
        List(I18NFragment.create(name))

    def words: Either[Vector[String], I18NHangar[String]] = {
      (name.getIfNoLocale, aliases.unify) match {
        case (Some(s), Left(vs)) => Left(s +: vs)
        case (Some(s), Right(map)) => Right(_unify(s, map))
        case (None, Left(vs)) => Right(_unify(name, vs))
        case (None, Right(map)) => Right(_unify(name, map))
      }
    }

    private def _unify(s: String, a: Map[Locale, Vector[String]]): I18NHangar[String] = {
      val x: Map[Locale, Vector[String]] = a.mapValues(x => s +: x)
      I18NHangar.create(x)
    }

    private def _unify(s: I18NString, a: Vector[String]): I18NHangar[String] = {
      val x0 = s.localeMapWithoutC
      val x = x0.mapValues(_ +: a)
      I18NHangar.create(x)
    }

    private def _unify(s: I18NString, a: Map[Locale, Vector[String]]): I18NHangar[String] = {
      val x0 = s.localeMapWithoutC
      val x: Map[Locale, Vector[String]] = x0.map {
        case (k, v) => k -> (v +: a.get(k).toVector.flatten)
      }
      I18NHangar.create(x)
    }

    def wordsWithoutWord(word: String): Either[Vector[String], I18NHangar[String]] =
      words match {
        case Left(l) => Left(l.filterNot(_ == word))
        case Right(r) => Right(r.filterNot(_ == word))
      }
  }
  object Term {
    case class Tokens(tokens: Vector[String]) {
      def contains(ps: Vector[String]) =
        if (tokens.isEmpty || ps.isEmpty)
          false
        else
          tokens.exists(x => tokens.exists(y => x.equalsIgnoreCase(y)))
    }

    def create(name: String): Term = Term(I18NString(name))
  }

  sealed trait Definition {
    def term: Term
    def page: URI
    def getId: Option[Dox.Id]
    def description: Dox
    def toHistorySlot: Vector[History.Slot]

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
      val name = StringUtils.pathLastComponent(page.toString)
      val lastmodefied = None
      Page(name, dox, None)
    }
  }
  object Definition {
    import History._

    case class Ingredients(
      term: Term,
      page: URI,
      description: Dox
    )
    object Ingredients {
      trait Holder {
        def ingredients: Ingredients
        def term = ingredients.term
        def page = ingredients.page
        def description = ingredients.description
      }
    }

    case class InDocument(
      id: Dox.Id,
      ingredients: Ingredients
    ) extends Definition with Ingredients.Holder {
      def getId = Some(id)

      def toHistorySlot: Vector[History.Slot] = Vector.empty
    }

    case class InGlossary(
      ingredients: Ingredients,
      node: TreeNode[Node],
      metadata: DocumentMetaData
    ) extends Definition with Ingredients.Holder {
      def getId = None
      def status = metadata.status
      def publishedAt = metadata.publishedAt
      def modifiedAt = metadata.modifiedAt

      private def _notice_option =
        Option(node.content).flatMap(Notice.createOption(node, _))

      def toHistorySlot: Vector[History.Slot] =
        _notice_option.toVector.flatMap(n =>
          modifiedAt match {
            case Some(s) => Vector(Slot(EventKind.Updated, s.toLocalDate, ContentKind.Glossary, n))
            case None => publishedAt match {
              case Some(s) => Vector(Slot(EventKind.Created, s.toLocalDate, ContentKind.Glossary, n))
              case None => Vector.empty
            }
          }
        )
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
      val d = Definition.InDocument(id, Definition.Ingredients(t, uri, description))
      add(term, d)
    }

    def add(term: String, d: Definition): Unit = {
      val s = Slot(term, d)
      _slots = _slots |+| Map(term -> Vector(s))
    }

    def register(node: TreeNode[Node], name: String, p: Document): Unit =
      for (term <- _make_term(p)) {
        val title = p.head.title
        val uri = new URI(s"glossary/$name.html")
        val meta = p.head.metadata
        val dox = p.body.elements
        val d = Definition.InGlossary(Definition.Ingredients(term, uri, dox), node, meta)
        add(name, d)
      }

    def register(node: TreeNode[Node], name: String, tag: Tag.TagName, p: Document): Unit = {
      for (so <- _make_structure(p)) {
        val term = _make_term(so)
        val uri = new URI(s"""glossary/${tag.name.replace(".", "/")}/$name.html""")
        val meta = p.head.metadata
        val dox = _make_description(term, so)
        val d = Definition.InGlossary(Definition.Ingredients(term, uri, dox), node, meta)
        add(name, d)
      }
    }

    private def _make_structure(p: Document): Option[StructureObject] = {
      val config = StructureObject.Builder.Config(
        StructureObject.Builder.Config.Schema.create(
          List(PROP_ALIASES, PROP_ACRONYM),
          List(PROP_DEFINITION, PROP_REFERENCE)
        )
      )
      val a = StructureObject.create(config, p)
      Some(a)
    }

    private def _make_term(p: StructureObject): Term = {
      val title = p.title
      val aliases = p.getAsI18NValue(PROP_ALIASES)
      val acronym = p.getAsI18NValue(PROP_ACRONYM)
      val summary = p.getAsI18NFragment(PROP_DEFINITION).map(_.toI18NString)
      Term(
        title.contents.toI18NString,
        aliases.map(_.toI18NHangar) getOrElse I18NHangar.empty,
        acronym.map(_.toPlainText),
        summary
      )
    }

    private def _make_term(p: Document): Option[Term] =
      for (title <- p.head.title) yield {
        Term(title.toI18NString)
      }

    private def _make_description(term: Term, so: StructureObject): Dox = {
      val ja = Div.create(LocaleUtils.ja, _make_ja_table(term))
      val en = Div.create(LocaleUtils.en, _make_en_table(term))
      val definition = so.getAsI18NFragment(PROP_DEFINITION).map { s =>
        Section.create(_enja("Definition", "定義"), s)
      }.toList
      val reference = so.getAsI18NFragment(PROP_REFERENCE).map { s =>
        Section.create(_enja("Reference", "参照"), s)
      }.toList
      val rs = ja +: en +: definition ::: so.contents
      Fragment(rs)
    }

    private def _make_en_table(term: Term) = {
      val aliasesen = _to_string(LocaleUtils.en, term.aliases)
      val tb = new Table.Builder()
      tb.append("Term", term.name.en)
      tb.append("Aliases", aliasesen)
      term.acronym.map(acronym =>
        tb.append("Acronym", acronym)
      )
      tb.apply()
    }

    private def _make_ja_table(term: Term) = {
      val aliasesja = _to_string(LocaleUtils.ja, term.aliases)
      val aliasesen = _to_string_option(LocaleUtils.en, term.aliases)
      val tb = new Table.Builder()
      tb.append("用語", term.name.ja)
      if (!term.name.isSimple)
        tb.append("用語(英)", term.name.en)
      tb.append("別名", aliasesja)
      aliasesen.foreach(x =>
        tb.append("別名(英)", x)
      )
      term.acronym.foreach(x =>
        tb.append("略語", x)
      )
      tb.apply()
    }

    private def _enja(e: String, j: String) = I18NString.enja(e, j)

    private def _to_string(l: Locale, p: VectorMap[Locale, Vector[String]]) =
      p.get(l).map(_.mkString(", ")).getOrElse("-")

    private def _to_string(l: Locale, p: I18NHangar[String]) =
      p.get(l).map(_.mkString(", ")).getOrElse("-")

    private def _to_string_option(l: Locale, p: I18NHangar[String]): Option[String] =
      p.get(l).map(_.mkString(", "))
  }
  object Builder {
    case class Slot(term: String, definition: Definition)
  }
}
