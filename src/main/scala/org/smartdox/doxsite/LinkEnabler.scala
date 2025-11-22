package org.smartdox.doxsite

import scala.collection.immutable.ListSet
import java.io._
import java.net.URI
import java.nio.file.Paths
import java.util.Locale
import org.goldenport.RAISE
import org.goldenport.tree._
import org.goldenport.i18n.I18NHangar
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.LocaleUtils
import org.goldenport.values.PathName
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.transformer._
import org.smartdox.metadata._

/*
 * @since   Mar.  7, 2025
 *  version Mar.  9, 2025
 *  version Apr.  5, 2025
 *  version May. 21, 2025
 *  version Jun. 16, 2025
 *  version Jul. 26, 2025
 *  version Aug. 23, 2025
 *  version Sep. 28, 2025
 *  version Oct. 28, 2025
 * @version Nov. 22, 2025
 * @author  ASAMI, Tomoharu
 */
class LinkEnabler(
  val context: DoxSiteTransformer.Context,
  val links: LinkCollection,
  val site: Tree[Node]
) extends DoxSiteTransformer {
  import LinkEnabler._

  override protected def dox_Transformers(
    context: DoxSiteTransformer.Context,
    node: TreeNode[Node],
    p: Page
  ): List[HomoTreeTransformer[Dox]] =
    if (context.config.doxsiteConfig.fold(true)(_.isLinkEnable(p)))
      List(new LinkEmbedder(context, node, this))
    else
      Nil

  def getMetaData(path: String): Option[DocumentMetaData] =
    site.getContent(path).flatMap {
      case m: Page => Dox.getMetadata(m.dox)
      case _ => None
    }
}

object LinkEnabler {
  import scala.collection.JavaConverters._
  import com.atilika.kuromoji.TokenizerBase
  import com.atilika.kuromoji.ipadic.Token
  import com.atilika.kuromoji.ipadic.Tokenizer

  class LinkEmbedder(
    val context: DoxSiteTransformer.Context,
    pageNode: TreeNode[Node],
    enabler: LinkEnabler
  ) extends DoxInSiteTransformer {
    import LinkEmbedder._

    private def _link_collection = enabler.links

//    private val _link_mark = "▸" // 軽量で自然: [▸ Glossary]
//    private val _link_mark = "⮕" // 見出し: [⮕ Error Concept]
//    private val _link_mark = "📄" // Markdown/記事リンク: [📄ドメイン・モデル構成要素]

//    private val _link_mark = "📝" // 文書リンク: [📝定義済みデータ型]
    private val _link_mark = context.textMark.article

//    private val _link_mark = "🔗" // 文中リンク例: [🔗エンティティ]
//    private val _link_mark = "🡒 " // [?] 文中: [🡒 Value Object]
//    private val _link_mark = "🔍" // 詳細記事: [🔍現象と観測記録]
//    private val _link_mark = "↳" // ネスト構造: [↳ サブモデル]

    private var _definitions: Vector[Glossary.Definition] = Vector.empty

    // Shared across all glossary terms within the same page
    private val expandedDefinitions = scala.collection.mutable.Set.empty[Glossary.Definition]

    private var _internal_links: LinkHolder = LinkHolder.empty

    private var _external_links: LinkHolder = LinkHolder.empty

    private val _is_document_stable = pageNode.getContent.fold(false) {
      case m: Page => m.dox.head.metadata.isStable
      case _ => false
    }

    private def _is_unstable(p: Dox): Boolean = !_is_stable(p)

    private def _is_stable(p: Dox): Boolean =
      if (p.isStable)
        true
      else
        stack.toStream.flatMap(_.getContent).flatMap(_.getStable).headOption getOrElse {
          _is_document_stable
        }

    def addGlossary(ps: Set[Glossary.Definition]): Unit = {
      _definitions = _definitions ++ ps
    }

    def usedDefinitions: ListSet[Glossary.Definition] = ListSet(_definitions: _*)

    override protected def make_Node(
      node: TreeNode[Dox],
      content: Dox
    ): TreeTransformer.Directive[Dox] = {
      def _locale_ = Dox.getLocaleInContext(node)
      content match {
        case m: Text =>
          if (_is_unstable(m))
            _transform(m)
          else
            directive_node(m)
        case m: Dfn => directive_node(m)
        case m: Dt => directive_node(m)
        case m: Hyperlink => _transform_hyperlink(_locale_, m)
        case m: Preserve => directive_node(m)
        case m if m.isStable => directive_node(m) // CAUTION
        case m: Body => directive_container_content_after(m, _create_references)
        case m: I18NFragment => _transform_i18nfragment(m)
        case m => directive_container_content(m)
      }
    }

    private def _create_references(p: TreeNode[Dox]): TreeNode[Dox] = {
      val xs = Vector(
        _create_reference_to,
        _create_reference_from,
        _create_bibliography
      ).flatten
      if (xs.isEmpty) {
        p
      } else {
        for (x <- xs) {
          val a = Dox.toTreeNode(x)
          p.addChild(a)
        }
        p
      }
    }

    private def _create_reference_to: Vector[Dox] = {
      val xs = Vector(
        _create_reference_relation,
        _create_reference_internal,
        _create_reference_external,
        _create_reference_glossary
      ).flatten
      _create_section(xs, "reference-section", "References", "参照")
    }

    private def _create_reference_relation: Vector[Dox] =
      Vector(
        _create_reference_relation_next,
        _create_reference_relation_prev,
        _create_reference_relation_related
      ).flatten

    private def _create_reference_relation_next: Vector[Dox] = {
      val xs = Vector.empty
      _create_section(xs, "Next Article", "次の記事")
    }

    private def _create_reference_relation_prev: Vector[Dox] = {
      val xs = Vector.empty
      _create_section(xs, "Prev Article", "前の記事")
    }

    private def _create_reference_relation_related: Vector[Dox] = {
      val a = _link_collection.get(pageNode.pathname) match {
        case Some(s) => s.incomingLinks.toListContents(pageNode.pathnameValue)
        case None => Vector.empty
      }
      val xs = _create_ul(a)
      _create_section(xs, "Related Articles", "関連記事")
    }

    private def _create_reference_internal: Vector[Dox] = {
      val xs = _reference_internal_links
      _create_section(xs, "In Site", "サイト内")
    }

    private def _reference_internal_links: Vector[Dox] =
      _create_ul(_internal_links.toListContents)

    private def _create_reference_external: Vector[Dox] = {
      val xs = _reference_external_links
      _create_section(xs, "External Links", "外部参照")
    }

    private def _reference_external_links: Vector[Dox] =
      _create_ul(_external_links.toListContents)

    private def _create_reference_glossary: Vector[Dox] = {
      val xs = _glossary_links
      _create_section(xs, "Glossary", "用語集")
    }

    private def _glossary_links: Vector[Dox] = {
      val xs: Vector[(Dt, Dd)] = _definitions.distinct.map { x =>
        val l = TextLinkProcessor.makeGlossaryLinkExpand(_create_href, x)
        val summary = x.term.effectiveBrief getOrElse I18NString("Undefined")
        (Dt.make(l), Dd.create(summary))
      }
      if (xs.isEmpty)
        Vector.empty
      else
        Vector(Dl(xs.toList))
    }

    private def _create_href(p: Glossary.Definition): URI =
      create_href(pageNode, p.page, p.getId)

    private def _create_reference_from: Vector[Dox] = {
      val xs = Vector(
        _create_reference_internal_from
      ).flatten
      _create_section(xs, "References From", "参照元")
    }

    // TODO
    private def _create_reference_internal_from: Vector[Dox] = {
      val xs = Vector.empty
      _create_section(xs, "In Site", "サイト内")
    }

    private def _create_bibliography: Vector[Dox] = {
      val xs = Vector(
      ).flatten
      _create_section(xs, "Bibliography", "参考文献")
    }

    private def _create_section(xs: Vector[Dox], classname: String, en: String, ja: String): Vector[Dox] =
      _create_section(xs, Map("class" -> classname), en, ja)

    private def _create_section(xs: Vector[Dox], en: String, ja: String): Vector[Dox] =
      _create_section(xs, Map.empty[String, String], en, ja)

    private def _create_section(xs: Vector[Dox], attrs: Map[String, String], en: String, ja: String): Vector[Dox] =
      if (xs.isEmpty)
        Vector.empty
      else
        Vector(Section.create(I18NString.enja(en, ja), attrs, xs))

    private def _create_ul(xs: Vector[ListContent]): Vector[Dox] =
      if (xs.isEmpty)
        Vector.empty
      else
        Vector(Ul.create(xs))

    private def _is_inside_table: Boolean = {
      stack.exists { n =>
        n.getContent.exists {
          case _: Table => true
          case _ => false
        }
      }
    }

    private def _transform(m: Text) = _transform_simple(m)

    private def _transform_simple(m: Text): TreeTransformer.Directive[Dox] = {
      def _create_href_(definition: Glossary.Definition): URI =
        create_href(pageNode, definition.page, definition.getId)

      // Always link glossary terms, but only expand outside tables
      val expand = !_is_inside_table
      val candidates = context.metadata.glossary.definitions
      val used = usedDefinitions
      val x = candidates.foldLeft(
        TextLinkProcessor(
          _create_href_,
          Vector(m),
          used,
          expandedDefinitions = this.expandedDefinitions,
          expandDefinition = expand
        )
      )(_+_)
      addGlossary(x.definitions)
      x.dox match {
        case Vector() => TreeTransformer.Directive.Empty()
        case Vector(m) => directive_node(m)
        case ms => directive_nodes(ms)
      }
    }

    private def _transform_kuromoji(m: Text): TreeTransformer.Directive[Dox] = {
      val tokens0 = _to_tokens(_tokenize(m.contents))
      val tokens = Glossary.Term.Tokens(tokens0.map(_.text))
      val candidates = context.metadata.glossary.candidates(tokens)
      if (candidates.isEmpty) {
        directive_container_content(m)
      } else {
        case class ZZ(definition: Glossary.Definition, xs: Vector[Dox]) {
          def r = xs

          def +(term: String) = {
            def _doxes_(ps: Vector[Dox]): Vector[Dox] = ps.flatMap {
              case m: Text => _enlink_(m)
              case m => Vector(m)
            }

            def _enlink_(p: Text): Vector[Dox] = {
              val a = _split(p.contents, term)
              a.map {
                case m if m == term =>
                  //                      val pagenode = context.pageNode getOrElse RAISE.noReachDefect
                  val href = create_href(pageNode, definition.page, definition.getId)
                  val titleoption = definition.term.summary
                  titleoption match {
                    case Some(title) =>
                      if (title.isSimple) {
                        Hyperlink.createGlossary(m, href, title.en)
                      } else {
                        I18NFragment.createDox(
                          List(
                            LocaleUtils.ja -> List(Hyperlink.createGlossary(m, href, title.ja)),
                            LocaleUtils.en -> List(Hyperlink.createGlossary(m, href, title.en))
                          )
                        )
                      }
                    case None => Hyperlink.createGlossary(m, href)
                  }
                case m => Text(m)
              }
            }

            copy(xs = _doxes_(xs))
          }

          private def _split(input: String, delimiter: String) =
            input.split(s"(?=$delimiter)|(?<=$delimiter)").toVector
        }
        case class Z(xs: Vector[Dox]) {
          def r: TreeTransformer.Directive[Dox] = xs match {
            case Vector() => TreeTransformer.Directive.Empty()
            case Vector(m) => directive_node(m)
            case ms => directive_nodes(ms)
          }

          def +(definition: Glossary.Definition) =
            Z(definition.candidates.foldLeft(ZZ(definition, xs))(_+_).r)
        }
        candidates.foldLeft(Z(Vector(m)))(_+_).r
      }
    }

    private def _transform_hyperlink(
      locale: Option[Locale],
      p: Hyperlink
    ): TreeTransformer.Directive[Dox] =
      if (p.isLocalOrRelative)
        _transform_hyperlink(locale, p, p.href)
      else
        _external_link(locale, p)

    private def _transform_hyperlink(
      locale: Option[Locale],
      dox: Hyperlink,
      href: URI
    ): TreeTransformer.Directive[Dox] =
      Option(href.getPath) match {
        case Some(s) =>
          val base = pageNode.pathname
          val path = StringUtils.resolvePathSafe(base, s)
          enabler.getMetaData(path) match {
            case Some(s) => s.title match {
              case Some(title0) =>
                val title = _prepend(_link_mark, _inline_contents(locale, title0))
                val relpath0 = StringUtils.relativizePathSafe(base, path)
                val relpath = StringUtils.changeSuffix(relpath0, "html")
                val tooltip = s.getEffectiveTooltip // .map(_text(locale, _))
                val r = Hyperlink.createArticle(title, new URI(relpath), tooltip, pageNode.pathnameValue)
                _internal_link(locale, r)
              case None => _external_link(locale, dox)
            }
            case None => _external_link(locale, dox)
          }
        case None => _external_link(locale, dox)
      }

    private def _inline_contents(locale: Option[Locale], p: I18NFragment): InlineContents =
      locale match {
        case Some(s) => List(Text(p.distillString(s)))
        case None => List(p)
      }

    private def _prepend(p: String, ps: InlineContents): InlineContents =
      Text(p) :: ps

    private def _text(locale: Option[Locale], p: I18NFragment): String =
      locale match {
        case Some(s) => p.distillString(s)
        case None => p.distillStringDefault
      }

    private def _text(locale: Option[Locale], p: I18NString): String =
      locale match {
        case Some(s) => p.as(s)
        case None => p.en
      }

    private def _internal_link(
      locale: Option[Locale],
      p: Hyperlink
    ): TreeTransformer.Directive[Dox] = {
      val ref = _normalize_title(p)
      val l = locale match {
        case Some(s) => _internal_links.add(s, ref)
        case None => _internal_links.add(ref)
      }
      _internal_links = l
      directive_node(ref)
    }

    private def _external_link(
      locale: Option[Locale],
      p: Hyperlink
    ): TreeTransformer.Directive[Dox] = {
      val ref = _normalize_title(p)
      val l = locale match {
        case Some(s) => _external_links.add(s, ref)
        case None => _external_links.add(ref)
      }
      _external_links = l
      directive_node(ref)
    }

    private def _normalize_title(p: Hyperlink) = 
      p.title match {
        case Some(s) => p
        case None => p.withTitle(p.href.toString)
      }

    private def _transform_i18nfragment(p: I18NFragment): TreeTransformer.Directive[Dox] = {
      val xs = p.makeInlines
      val r = _transform_dox_list(xs)
      directive_node(r)
    }

    private def _transform_dox_list(ps: List[Dox]): List[Dox] =
      ps.map(_transform_dox)

    private def _transform_dox(p: Dox) = {
      val a = Dox.toTreeNode(p)
      val b: Tree[Dox] = apply(a)
      Dox.toDox(b)
    }
  }
  object LinkEmbedder {
    case class LinkHolder(links: Vector[Link] = Vector.empty) {
      def add(locale: Option[Locale], hyperlink: Hyperlink): LinkHolder = locale match {
        case Some(s) => add(s, hyperlink)
        case None => add(hyperlink)
      }

      def add(hyperlink: Hyperlink): LinkHolder =
        links.find(_.href == hyperlink.href) match {
          case Some(s) =>
            val a = s.add(hyperlink)
            val b = links.map(x =>
              if (x.href == hyperlink.href)
                s
              else
                x
            )
            copy(links = b)
          case None => copy(links = links :+ Link(hyperlink))
        }

      def add(locale: Locale, hyperlink: Hyperlink): LinkHolder =
        links.find(_.href == hyperlink.href) match {
          case Some(existing) =>
            val updated = existing.add(locale, hyperlink)
            val newLinks = links.map {
              case x if x.href == hyperlink.href => updated
              case x => x
            }
            copy(links = newLinks)
          case None =>
            val link = Link(locale, hyperlink)
            copy(links = links :+ link)
        }

      def toListContents: Vector[ListContent] = links.map(_.toListContent)
    }
    object LinkHolder {
      val empty = LinkHolder()
    }

    case class Link(
      href: URI,
      pathname: Option[PathName],
      slots: I18NHangar[Link.Slot]
    ) {
      def add(p: Hyperlink) = copy(slots = slots.add(Link.Slot(p)))
      def add(locale: Locale, p: Hyperlink) = copy(slots = slots.add(locale, Link.Slot(p)))

      def hyperlinks: I18NHangar[Hyperlink] = slots.mapValue(_.link)

      def toListContent: I18NFragment = {
//        slots.valueVector
        I18NFragment.create(slots.mapValueCollection(_.map(_.link)))
      }
    }
    object Link {
      case class Slot(link: Hyperlink)

      def apply(hyperlink: Hyperlink): Link = Link(
        hyperlink.href,
        hyperlink.source,
        I18NHangar.createCommons(Slot(hyperlink))
      )

      def apply(locale: Locale, hyperlink: Hyperlink): Link = Link(
        hyperlink.href,
        hyperlink.source,
        I18NHangar.create(locale, Slot(hyperlink))
      )
    }
  }

  case class DoxSiteToken(text: String) {
    import DoxSiteToken._

    val kind = classify(text)

    def isAvailable: Boolean = kind match {
      case TokenKind.Abbreviation => text.length >= 2
      case TokenKind.AllAlphabet => text.length >= 4
      case TokenKind.AllNonAlphabet => text.length >= 2
      case TokenKind.AlphabetMixed => text.length >= 4
      case TokenKind.ContainSymbol => false
      case TokenKind.Number => false
    }

    def merge(p: Token): DoxSiteToken = DoxSiteToken(text + p.getSurface)
  }
  object DoxSiteToken {
    sealed trait TokenKind
    object TokenKind {
      case object Abbreviation extends TokenKind
      case object AllAlphabet extends TokenKind
      case object AllNonAlphabet extends TokenKind
      case object AlphabetMixed extends TokenKind
      case object ContainSymbol extends TokenKind
      case object Number extends TokenKind
    }

    def create(p: Token): DoxSiteToken = DoxSiteToken(p.getSurface)

    def create(a: DoxSiteToken, b: String, c: Token): DoxSiteToken =
      DoxSiteToken(a.text + b + c.getSurface)

    def classify(p: String): TokenKind = {
      import org.goldenport.util.StringUtils._

      if (p.length > 1 && p.forall(x => isAsciiAlphabetUpperChar(x) || isAsciiNumberChar(x)))
        TokenKind.Abbreviation
      else if (isNumber(p) && !(p == "true" || p == "false"))
        TokenKind.Number
      else if (isAsciiAlphabetNumberString(p))
        TokenKind.AllAlphabet
      else if (isAsciiString(p) && !isAsciiAlphabetNumberString(p))
        TokenKind.ContainSymbol
      else if (p.exists(x => isAsciiChar(x)))
        TokenKind.AlphabetMixed
      else
        TokenKind.AllNonAlphabet
    }
  }

  case class TextLinkProcessor(
    createhref: Glossary.Definition => URI,
    dox: Vector[Dox],
    definitions: ListSet[Glossary.Definition] = ListSet.empty,
    expandedDefinitions: scala.collection.mutable.Set[Glossary.Definition] = scala.collection.mutable.Set.empty,
    expandDefinition: Boolean = true,
    boundaryConfig: TextLinkProcessor.GlossaryBoundaryConfig = TextLinkProcessor.GlossaryBoundaryConfig.default
  ) {
    import TextLinkProcessor._

    def +(candidate: Glossary.Definition) = {
      val tokens = candidate.candidates

      case class Z(holder: Holder = Holder.definitions(definitions)) {
        def r = TextLinkProcessor(
          createhref,
          holder.xs,
          holder.ds,
          expandedDefinitions = expandedDefinitions,
          expandDefinition = expandDefinition
        )

        def +(rhs: Dox) = {
          case class ZZ(zzholder: Holder) {
            def +(token: String) = {
              case class ZZZ(zzzholder: Holder) {
                def r = ZZ(zzzholder)

                def +(rhs: Dox) = rhs match {
                  case m: Text =>
                    val used = zzzholder.isUsed(candidate) // || zzzholder.isTokenUsed(token)
                    _split_and_link(candidate, m, token, used) match {
                      case Some(parts) =>
                        copy(zzzholder = zzzholder.add(parts, candidate))
                      case None => copy(zzzholder = zzzholder.add(m))
                    }
                  case m => copy(zzzholder = zzzholder.add(m))
                }
              }
              zzholder.xs.foldLeft(ZZZ(Holder(Vector.empty, zzholder.ds)))(_+_).r
            }
          }

          rhs match {
            case m: Text =>
              val start = Holder(Vector(m), holder.ds)
              val zz = tokens.foldLeft(ZZ(start))(_+_)
              copy(holder = holder.add(zz.zzholder))
            case m => copy(holder = holder.add(rhs))
          }
        }
      }
      dox.foldLeft(Z())(_+_).r
    }

    private def _split_and_link(
      definition: Glossary.Definition,
      t: Text,
      token: String,
      used: Boolean
    ): Option[Vector[Dox]] = {
      val s = t.contents
      if (token.isEmpty || s.isEmpty)
        return None
      val kind = GlossaryKind.detectGlossaryKind(s)
      val buf = Vector.newBuilder[Dox]
      var idx = 0
      var count = if (used) 1 else 0
      var hit = false
      var found = _next(s, token, idx, kind)
      var expanded = used
      while (found >= 0) {
        val pre = s.substring(idx, found)
        if (pre.nonEmpty)
          buf += Dox.text(pre)
        val canaux = if (count == 0)
          _can_aux(s, found + token.length)
        else
          false
        // Only expand if expandDefinition is true and this definition hasn't been expanded yet
        if (expandDefinition && !expandedDefinitions.contains(definition)) {
          // First occurrence outside table → expand
          buf += _make_glossary_link(definition, token, canaux = true, expand = true)
          expandedDefinitions += definition
          expanded = true
        } else {
          // Table or subsequent → link only
          buf += _make_glossary_link(definition, token, canaux, expand = false)
        }
        hit = true
        count = count + 1
        idx = found + token.length
        found = _next(s, token, idx, kind)
      }
      val tail = s.substring(idx)
      if (tail.nonEmpty)
        buf += Dox.text(tail)
      if (hit)
        Some(buf.result())
      else
        None
    }

    // Glossary-aware next-token finder
    private def _next(
      s: String,
      token: String,
      idx: Int,
      kind: GlossaryKind,
    ): Int = {

      // === Unicode category helpers ===

      @inline def isAsciiLetter(c: Char): Boolean =
        c <= '\u007F' && c.isLetter

      @inline def isHiragana(c: Char): Boolean =
        c >= '\u3040' && c <= '\u309F'

      @inline def isKatakana(c: Char): Boolean =
        (c >= '\u30A0' && c <= '\u30FF') ||
      (c >= '\u31F0' && c <= '\u31FF')

      @inline def isKanji(c: Char): Boolean = {
        val cp = c.toInt
        (cp >= 0x4E00  && cp <= 0x9FFF )  || // CJK Unified Ideographs
        (cp >= 0x3400  && cp <= 0x4DBF )  || // Extension A
        (cp >= 0xF900  && cp <= 0xFAFF )  || // Compatibility
        (cp >= 0x20000 && cp <= 0x2FA1F)     // Extension B–G (surrogate pair range)
      }

      @inline def dbg(msg: => String): Unit =
        if (boundaryConfig.debug) println("[GlossaryBoundary] " + msg)

      // === Boundary rule by glossary term kind ===
      //
      // Returns true if the character can be treated as a boundary.
      // Returns false if it indicates the token is in the middle of another word.
      //
      def isBoundaryFor(c: Char): Boolean = {
        // Special blocker symbols → treated as “not boundary”
        if (boundaryConfig.isSpecialBlocker(c))
          return false

        kind match {
          // English terms: English letters imply "not a boundary"
          case GlossaryKind.English =>
            !isAsciiLetter(c)

          // Katakana terms
          case GlossaryKind.Katakana =>
            // If Katakana compounding is not allowed and next char is Katakana → NG
            if (!boundaryConfig.katakanaCompoundOK && isKatakana(c))
              false
            else
              // Otherwise allowed except ASCII letters or Kanji
              !isAsciiLetter(c) && !isKanji(c)

          // Kanji terms
          case GlossaryKind.KanjiTerm =>
            // If Kanji compounding is not allowed and next char is Kanji → NG
            if (!boundaryConfig.kanjiCompoundOK && isKanji(c))
              false
            else
              // ASCII letters also imply continuation
              !isAsciiLetter(c)

          // Default: English/Kanji continuation is forbidden
          case GlossaryKind.Other =>
            !isAsciiLetter(c) && !isKanji(c)
        }
      }

      // === Search loop ===
      var pos = s.indexOf(token, idx)

      while (pos >= 0) {
        val beforeOK =
          if (pos == 0) true
          else isBoundaryFor(s.charAt(pos - 1))

        val afterPos = pos + token.length
        val afterOK =
          if (afterPos >= s.length) true
          else isBoundaryFor(s.charAt(afterPos))

        dbg(s"token='$token' pos=$pos beforeOK=$beforeOK afterOK=$afterOK")

        // If both sides look like boundaries → valid glossary term occurrence
        if (beforeOK && afterOK)
          return pos

        // Otherwise skip and search further
        pos = s.indexOf(token, pos + 1)
      }

      pos
    }
    // private def _next(s: String, token: String, idx: Int): Int = {
    //   val n = s.indexOf(token, idx)
    //   if (n > 0 && s.charAt(n - 1) == '.')
    //     _next(s, token, idx + token.length)
    //   else
    //     n
    // }

    private def _can_aux(p: String, i: Int): Boolean = {
      var k = i
      val n = p.length
      while (k < n && p.charAt(k).isWhitespace)
        k += 1
      val a = k == n
      val b = k < n && (p.charAt(k) match {
        case '(' => false
        case '（' => false
        case _ => true
      })
      a || b
    }

    private def _make_glossary_link(
      definition: Glossary.Definition,
      token: String,
      canaux: Boolean,
      expand: Boolean
    ) = makeGlossaryLink(createhref, definition, token, canaux, expand)
  }
  object TextLinkProcessor {
    final case class GlossaryBoundaryConfig(
      katakanaCompoundOK: Boolean = false, // default: disallow Katakana compounding
      kanjiCompoundOK: Boolean = false,    // default: disallow Kanji compounding,
      specialBlockerChars: Set[Char] = Set('・', '·'),
      debug: Boolean = false               // print debug logs
    ) {
      def isSpecialBlocker(c: Char): Boolean = specialBlockerChars.contains(c)
    }
    object GlossaryBoundaryConfig {
      val default = GlossaryBoundaryConfig()
    }

    // Glossary term classification
    sealed trait GlossaryKind
    object GlossaryKind {
      case object English extends GlossaryKind
      case object Katakana extends GlossaryKind
      case object KanjiTerm extends GlossaryKind
      case object Other extends GlossaryKind

      def detectGlossaryKind(token: String): GlossaryKind = {
        if (token.forall(c => c <= '\u007F' && (c.isLetter || c.isDigit)))
          GlossaryKind.English
        else if (token.forall { c =>
          (c >= '\u30A0' && c <= '\u30FF') || // Standard Katakana
          (c >= '\u31F0' && c <= '\u31FF')    // Katakana phonetic extensions
        })
          GlossaryKind.Katakana
        else if (token.exists(c =>
          (c >= '\u4E00' && c <= '\u9FFF') // CJK Ideographs
        ))
          GlossaryKind.KanjiTerm
        else
          GlossaryKind.Other
      }
    }

    case class Holder(
      xs: Vector[Dox],
      ds: ListSet[Glossary.Definition]
    ) {
      def isUsed(p: Glossary.Definition): Boolean = ds.contains(p)
      // def isTokenUsed(token: String): Boolean =
      //   ds.exists(_.candidates.exists(_ equalsIgnoreCase token))

      def add(p: Holder) = copy(xs = xs ++ p.xs, ds = ds ++ p.ds)
      def add(p: Dox) = copy(xs = xs :+ p)
      def add(ps: Seq[Dox], candidate: Glossary.Definition) =
        copy(xs = xs ++ ps, ds = ds + candidate)
    }
    object Holder {
      val empty = Holder(Vector.empty, ListSet.empty)

      def definitions(ps: ListSet[Glossary.Definition]) = empty.copy(ds = ps)
    }

    // case class Result(
    //   dox: Vector[Dox],
    //   definitions: Vector[Glossary.Definition] = Vector.empty
    // )

    def makeGlossaryLinkExpand(
      createhref: Glossary.Definition => URI,
      definition: Glossary.Definition
    ): Dox = makeGlossaryLink(
      createhref, definition, definition.term.name, true, true
    )

    def makeGlossaryLink(
      createhref: Glossary.Definition => URI,
      definition: Glossary.Definition,
      token: String,
      canaux: Boolean,
      expand: Boolean
    ): Dox = makeGlossaryLink(
      createhref, definition, I18NString(token), canaux, expand
    )

    def makeGlossaryLink(
      createhref: Glossary.Definition => URI,
      definition: Glossary.Definition,
      token: I18NString,
      canaux: Boolean,
      expand: Boolean
    ): Dox = {
      val href = createhref(definition)
      val titleoption = Option(definition.term.name)
      titleoption match {
        case Some(title) =>
          if (title.isSimple) {
            Hyperlink.createGlossary(_make_label(definition, token, canaux, expand), href, title.en)
          } else {
            I18NFragment.createDox(
              List(
                LocaleUtils.ja -> List(Hyperlink.createGlossary(_make_label_ja(definition, token, canaux, expand), href, title.ja)),
                LocaleUtils.en -> List(Hyperlink.createGlossary(_make_label_en(definition, token, canaux, expand), href, title.en))
              )
            )
          }
        case None => Hyperlink.createGlossary(_make_label(definition, token, canaux, expand), href)
      }
    }

    private def _make_label(
      definition: Glossary.Definition,
      token: I18NString,
      canaux: Boolean,
      expand: Boolean
    ): List[Inline] = {
      _make_label_en(definition, token, canaux, expand)
    }

    private def _make_label_en(
      definition: Glossary.Definition,
      token: I18NString,
      canaux: Boolean,
      expand: Boolean
    ): List[Inline] = {
      if (!expand) {
        Dox.toInlineContents(token.en)
      } else {
        val text = if (canaux) {
          val wr = definition.term.wordRelation(token.en)
          val xs = wr.en
          _create_label(token.en, xs)
        } else {
          token.en
        }
        Dox.toInlineContents(text)
      }
    }

    private def _make_label_ja(
      definition: Glossary.Definition,
      token: I18NString,
      canaux: Boolean,
      expand: Boolean
    ): List[Inline] = {
      if (!expand) {
        Dox.toInlineContents(token.ja)
      } else {
        val text = if (canaux) {
          val wr = definition.term.wordRelation(token.ja)
          val xs = wr.ja
          _create_label(token.ja, xs)
        } else {
          token.ja
        }
        Dox.toInlineContents(text)
      }
    }

    private def _create_label(token: String, ps: Seq[String]): String =
      if (ps.isEmpty)
        token
      else
        s"""$token (${ps.mkString(", ")})"""
  }

  private def _tokenize(p: String): Vector[Token] = {
    val in: InputStream = getClass()
      .getClassLoader()
      .getResourceAsStream("doxsite/kuromoji_dict.csv")
    val tokenizer: Tokenizer = new Tokenizer.Builder().
//      mode(TokenizerBase.Mode.NORMAL).
//      mode(TokenizerBase.Mode.SEARCH).
      userDictionary(in).
      build()
    tokenizer.tokenize(p).asScala.toVector
  }

  private def _to_tokens(ps: Seq[Token]): Vector[DoxSiteToken] = {
    case class Z(
      xs: Vector[DoxSiteToken] = Vector.empty,
      glue: Option[String] = None
    ) {
      val r = xs.filter(_.isAvailable).distinct

      def +(rhs: Token) = {
        if (_is_middle_dot(rhs)) {
          if (xs.isEmpty)
            this
          else
            glue match {
              case Some(s) => copy(glue = Some(s + rhs.getSurface))
              case None => copy(glue = Some(rhs.getSurface))
            }
        } else {
          glue match {
            case Some(s) => xs.lastOption match {
              case Some(l) => copy(xs = xs.init :+ DoxSiteToken.create(l, s, rhs), glue = None)
              case None => copy(xs = xs :+ DoxSiteToken.create(rhs), glue = None)
            }
            case None => 
              if (_is_noun(rhs)) {
                val x = if (_is_suffix(rhs))
                  xs.lastOption match {
                    case Some(s) => xs.init :+ s.merge(rhs)
                    case None => xs :+ DoxSiteToken.create(rhs)
                  }
                  else
                    xs :+ DoxSiteToken.create(rhs)
                copy(xs = x)
              } else {
                this
              }
          }
        }
      }
    }
    ps.foldLeft(Z())(_+_).r
  }

  private def _is_noun(t: Token): Boolean =
    t.getPartOfSpeechLevel1().startsWith("名詞")

  private def _is_suffix(t: Token): Boolean =
    t.getPartOfSpeechLevel2().startsWith("接尾")

  private def _is_middle_dot(t: Token): Boolean =  t.getSurface == "・"
}
