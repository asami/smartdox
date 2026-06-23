package org.smartdox.metadata

import scalaz.{Value => _, _}
import Scalaz._
import java.net.URI
import java.util.Locale
import scala.util.control.NonFatal
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
import org.smartdox.semanticweb.Site._

/*
 * @since   Feb. 23, 2025
 *  version Feb. 24, 2025
 *  version Mar.  9, 2025
 *  version Aug. 31, 2025
 *  version Sep. 22, 2025
 *  version Oct. 28, 2025
 *  version Nov. 27, 2025
 * @version Jun. 23, 2026
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
  final val PROP_BRIEF = "brief"
  final val PROP_ALIASES = "aliases"
  final val PROP_ABBREVIATION = "abbreviation"
  final val PROP_ACRONYM = "acronym"
  final val PROP_REMARKS = "remarks"
  final val PROP_REFERENCE = "reference"
  final val PROP_READING = "reading"
  final val PROP_YOMI = "yomi"

  val empty = Glossary()

  implicit val GlossaryMonoid: Monoid[Glossary] = new Monoid[Glossary] {
    def zero: Glossary = Glossary.empty
    def append(f1: Glossary, f2: => Glossary): Glossary = f1 + f2
  }

  case class Term(
    kind: Term.Kind,
    name: I18NString,
    aliases: I18NHangar[String] = I18NHangar.empty,
    abbreviation: Option[String] = None,
    summary: Option[I18NString] = None,
    brief: Option[I18NString] = None
  ) {
    val key: String = name.en

    val candidates: Vector[String] = {
      val a = name.terms
      val b = abbreviation.toVector
      val c = aliases.valueVector
      (a ++ b ++ c).distinct
    }

    def isAvailable(tokens: Term.Tokens): Boolean = tokens.contains(candidates)

    def toTitle: List[Inline] =
      if (name.isSimple)
        List(Text(StringUtils.makeTitle(name.en)))
      else
        List(I18NFragment.create(name.map(_to_title(_))))

    private def _to_title(p: (Locale, String)): (Locale, String) = {
      val (locale, s) = p
      locale -> StringUtils.makeTitle(s)
    }

    def effectiveBrief: Option[I18NString] = brief orElse summary

    def effectiveSummary: Option[I18NString] = summary orElse brief

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

    def wordRelation(word: String): Term.WordRelation = {
      val a = abbreviation match {
        case Some(s) =>
          if (s == word)
            Some(name.en)
          else
            Some(s)
        case None => None
      }
      Term.WordRelation(word, a, name, aliases)
    }
  }
  object Term {
    sealed trait Kind
    object Kind {
      case object CommonNoun extends Kind
      case object ProperNoun extends Kind

      def make(p: String): Kind =
        if (StringUtils.isAsciiAlphabetString(p) && p.forall(_.isLower))
          CommonNoun
        else
          ProperNoun

      def make(p: I18NString): Kind = make(p.en)
    }

    case class Tokens(tokens: Vector[String]) {
      def contains(ps: Vector[String]) =
        if (tokens.isEmpty || ps.isEmpty)
          false
        else
          tokens.exists(x => tokens.exists(y => x.equalsIgnoreCase(y)))
    }

    case class WordRelation(
      word: String,
      abbreviation: Option[String],
      name: I18NString,
      aliases: I18NHangar[String]
    ) {
      def en: Vector[String] = {
        val a = abbreviation.toVector
        val b = Vector(name.en)
        val c = aliases.valueVectorEn
        val d: Vector[String] = (a ++ b ++ c).distinct
        d.filterNot(_ equalsIgnoreCase word)
      }

      def ja: Vector[String] = abbreviation match {
        case Some(s) => _ja_abbreviation(s)
        case None => _ja_simple
      }

      private def _ja_abbreviation(abbreviation: String) = {
        val a = Vector(abbreviation)
        val b = name.en match {
          case m if m == abbreviation => Vector.empty
          case m => Vector(m)
        }
        val c = Vector(name.ja)
        val d = aliases.valueVectorJa
        val z: Vector[String] = (a ++ b ++ c ++ d).distinct
        z.filterNot(_ equalsIgnoreCase word)
      }

      private def _ja_simple = {
        val a = Vector(name.en)
        val b = Vector(name.ja)
        val c = aliases.valueVectorJa
        val z: Vector[String] = (a ++ b ++ c).distinct
        z.filterNot(_ equalsIgnoreCase word)
      }
    }

    def make(name: String): Term = Term(
      Kind.make(name),
      I18NString(name)
    )

    def make(
      name: I18NString,
      aliases: I18NHangar[String] = I18NHangar.empty,
      abbreviation: Option[String] = None,
      summary: Option[I18NString] = None,
      brief: Option[I18NString] = None,
      remarks: Option[I18NString] = None
    ): Term = {
      val kind = Kind.make(name)
      Term(kind, name, aliases, abbreviation, summary, brief)
    }
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

      private def _notice_option: Option[Notices.Notice] =
        for {
          c <- Option(node.content)
          n <- Notice.createOption(node, c)
        } yield {
          n.withSummaryDescription(ingredients.term.summary, ingredients.description).
            withBrief(ingredients.term.brief)
        }

      def toHistorySlot: Vector[History.Slot] =
        _notice_option.toVector.flatMap { n =>
          val desc = None
          modifiedAt match {
            case Some(s) =>
              Vector(Slot(EventKind.Updated, s.toLocalDate, ContentKind.Glossary, n, desc))
            case None => publishedAt match {
              case Some(s) =>
                Vector(Slot(EventKind.Created, s.toLocalDate, ContentKind.Glossary, n, desc))
              case None => Vector.empty
            }
          }
        }

      def toSiteResource: SiteResource =
        SiteResource.Glossary.create(page, metadata)
    }

    sealed trait TokenKind
    object TokenKind {
      case class LocaleToken(locale: Locale) extends TokenKind
      case object AbbreviationPrimary extends TokenKind
      case object AbbreviationSecondary extends TokenKind
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
      val t = Term.make(term)
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
        val meta = p.head.metadata
        val term = _make_term(so, meta)
        val uri = new URI(s"""glossary/${tag.name.replace(".", "/")}/$name.html""")
        val dox = _make_description(term, so, p.body.elements)
        val d = Definition.InGlossary(Definition.Ingredients(term, uri, dox), node, meta)
        add(name, d)
      }
    }

    private def _make_structure(p: Document): Option[StructureObject] = {
      val config = StructureObject.Builder.Config(
        StructureObject.Builder.Config.Schema.create(
          List(PROP_ALIASES, PROP_ABBREVIATION, PROP_ACRONYM),
          List(PROP_DEFINITION, PROP_REFERENCE, PROP_BRIEF, PROP_REMARKS)
        )
      )
      val a = StructureObject.create(config, p)
      Some(a)
    }

    private def _make_term(p: StructureObject, metadata: DocumentMetaData): Term = {
      val title = p.title
      val aliases = p.getAsI18NValue(PROP_ALIASES)
      val abbreviation = p.getAsI18NValue(PROP_ABBREVIATION) orElse p.getAsI18NValue(PROP_ACRONYM)
      val summary = _metadata_i18n_string(metadata, "summary", PROP_BRIEF, "description", PROP_DEFINITION).
        orElse(metadata.getEffectiveSummary).
        orElse(p.getAsI18NFragment(PROP_DEFINITION).map(_.toI18NString))
      val brief = _metadata_i18n_string(metadata, PROP_BRIEF, "summary", "description").
        orElse(metadata.getEffectiveBrief).
        orElse(p.getAsI18NFragment(PROP_BRIEF).map(_.toI18NString))
      val remarks = p.getAsI18NFragment(PROP_REMARKS).map(_.toI18NString)
      val titlei18n = title.contents.toI18NString
      val name = _reading(metadata).filterNot(_ == titlei18n.en).
        map(reading => I18NString.enja(titlei18n.en, reading)).
        getOrElse(titlei18n)
      Term.make(
        name,
        aliases.map(_.toI18NHangar) getOrElse I18NHangar.empty,
        abbreviation.map(_.toPlainText),
        summary,
        brief,
        remarks
      )
    }

    private def _make_term(p: Document): Option[Term] = {
      val metadata = p.head.metadata
      val title = p.head.title.map(_.toI18NString).orElse(metadata.title.map(_.toI18NString))
      title.map { titlei18n =>
        val name = _reading(metadata).filterNot(_ == titlei18n.en).
          map(reading => I18NString.enja(titlei18n.en, reading)).
          getOrElse(titlei18n)
        Term.make(
          name,
          summary = _metadata_i18n_string(metadata, "summary", PROP_BRIEF, "description", PROP_DEFINITION).orElse(metadata.getEffectiveSummary),
          brief = _metadata_i18n_string(metadata, PROP_BRIEF, "summary", "description").orElse(metadata.getEffectiveBrief)
        )
      }
    }

    private def _reading(metadata: DocumentMetaData): Option[String] =
      _metadata_string(metadata, PROP_READING, PROP_YOMI, "読み")

    private def _metadata_i18n_string(metadata: DocumentMetaData, keys: String*): Option[I18NString] =
      _metadata_string(metadata, keys: _*).map(I18NString(_))

    private def _metadata_string(metadata: DocumentMetaData, keys: String*): Option[String] =
      metadata.properties.flatMap { hocon =>
        keys.toStream.flatMap { key =>
          try {
            if (hocon.hasPath(key))
              Some(hocon.getString(key)).filter(_.nonEmpty)
            else
              None
          } catch {
            case NonFatal(_) => None
          }
        }.headOption
      }

    private def _make_description(term: Term, so: StructureObject, fallbackcontents: List[Dox]): Dox = {
      val ja = Div.create(LocaleUtils.ja, _make_ja_table(term))
      val en = Div.create(LocaleUtils.en, _make_en_table(term))
      val definition = so.getAsI18NFragment(PROP_DEFINITION).map { s =>
        Section.create(_enja("Definition", "定義"), s)
      }.toList
      val remarks = so.getAsI18NFragment(PROP_REMARKS).map { s =>
        Section.create(_enja("Remarks", "備考"), s)
      }.toList
      val reference = so.getAsI18NFragment(PROP_REFERENCE).map { s =>
        Section.create(_enja("Reference", "参照"), s)
      }.toList
      val contents = if (so.contents.nonEmpty) so.contents else fallbackcontents
      val rs = ja +: en +: definition ::: contents ++ reference ++ reference
      Fragment(rs)
    }

    private def _make_en_table(term: Term) = {
      val aliasesen = _to_string(LocaleUtils.en, term.aliases)
      val tb = new Table.Builder()
      tb.append("Term", term.name.en)
      tb.append("Aliases", aliasesen)
      term.abbreviation.map(abbreviation =>
        tb.append("Abbreviation", abbreviation)
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
      term.abbreviation.foreach(x =>
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
