package org.smartdox.doxsite

import java.io._
import java.net.URI
import java.nio.file.Paths
import java.util.Locale
import org.goldenport.RAISE
import org.goldenport.tree._
import org.goldenport.i18n.I18NHangar
import org.goldenport.i18n.LocaleUtils
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
 * @version Oct. 22, 2025
 * @author  ASAMI, Tomoharu
 */
class LinkEnabler(
  val context: DoxSiteTransformer.Context,
  val site: Tree[Node]
) extends DoxSiteTransformer {
  import LinkEnabler._

  override protected def dox_Transformers(
    context: DoxSiteTransformer.Context,
    node: TreeNode[Node],
    p: Page
  ): List[HomoTreeTransformer[Dox]] =
    if (context.config.doxsiteConfig.fold(true)(_.isLinkEnable(p)))
      List(new LinkEmbeder(context, node, this))
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

  class LinkEmbeder(
    val context: DoxSiteTransformer.Context,
    pageNode: TreeNode[Node],
    enabler: LinkEnabler
  ) extends DoxInSiteTransformer {
//    private val _link_mark = "▸" // 軽量で自然: [▸ Glossary]
//    private val _link_mark = "⮕" // 見出し: [⮕ Error Concept]
    private val _link_mark = "📄" // Markdown/記事リンク: [📄ドメイン・モデル構成要素]

//    private val _link_mark = "📝" // 文書リンク: [📝定義済みデータ型]

//    private val _link_mark = "🔗" // 文中リンク例: [🔗エンティティ]
//    private val _link_mark = "🡒 " // [?] 文中: [🡒 Value Object]
//    private val _link_mark = "🔍" // 詳細記事: [🔍現象と観測記録]
//    private val _link_mark = "↳" // ネスト構造: [↳ サブモデル]

    private var _definitions: Set[Glossary.Definition] = Set.empty

    // Shared across all glossary terms within the same page
    private val expandedDefinitions = scala.collection.mutable.Set.empty[Glossary.Definition]

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

    def usedDefinitions = _definitions

    override protected def make_Node(
      node: TreeNode[Dox],
      content: Dox
    ): TreeTransformer.Directive[Dox] = {
      content match {
        case m: Text =>
          if (_is_unstable(m))
            _transform(m)
          else
            directive_node(m)
        case m: Dfn => directive_node(m)
        case m: Dt => directive_node(m)
        case m: Hyperlink => _transform_hyperlink(m)
        case m: Preserve => directive_node(m)
        case m if m.isStable => directive_node(m) // CAUTION
        case m => directive_container_content(m)
      }
    }

    private def _transform(m: Text) = _transform_simple(m)

    private def _is_inside_table: Boolean = {
      stack.exists { n =>
        n.getContent.exists {
          case _: Table => true
          case _ => false
        }
      }
    }

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

    private def _transform_hyperlink(p: Hyperlink): TreeTransformer.Directive[Dox] = {
      Option(p.href.getScheme) match {
        case Some(s) =>
          if (s == "file")
            _transform_hyperlink(p, p.href)
          else
            directive_node(p)
        case None => _transform_hyperlink(p, p.href)
      }
    }

    private def _transform_hyperlink(dox: Hyperlink, href: URI): TreeTransformer.Directive[Dox] =
      Option(href.getPath) match {
        case Some(s) =>
          val base = pageNode.pathname
          val path = StringUtils.resolvePathSafe(base, s)
          enabler.getMetaData(path) match {
            case Some(s) => s.title match {
              case Some(title0) =>
                val title = _link_mark +: title0
                val relpath0 = StringUtils.relativizePathSafe(base, path)
                val relpath = StringUtils.changeSuffix(relpath0, "html")
                val tooltip = s.getEffectiveTooltip
                val r = Hyperlink.createArticle(title, new URI(relpath), tooltip)
                directive_node(r)
              case None => directive_node(dox)
            }
            case None => directive_node(dox)
          }
        case None => directive_node(dox)
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
    definitions: Set[Glossary.Definition] = Set.empty,
    expandedDefinitions: scala.collection.mutable.Set[Glossary.Definition] = scala.collection.mutable.Set.empty,
    expandDefinition: Boolean = true
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
      val buf = Vector.newBuilder[Dox]
      var idx = 0
      var count = if (used) 1 else 0
      var hit = false
      var found = _next(s, token, idx)
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
        found = _next(s, token, idx)
      }
      val tail = s.substring(idx)
      if (tail.nonEmpty)
        buf += Dox.text(tail)
      if (hit)
        Some(buf.result())
      else
        None
    }

    private def _next(s: String, token: String, idx: Int): Int = {
      val n = s.indexOf(token, idx)
      if (n > 0 && s.charAt(n - 1) == '.')
        _next(s, token, idx + token.length)
      else
        n
    }

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
    ) = {
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
      token: String,
      canaux: Boolean,
      expand: Boolean
    ): List[Inline] = {
      _make_label_en(definition, token, canaux, expand)
    }

    private def _make_label_en(
      definition: Glossary.Definition,
      token: String,
      canaux: Boolean,
      expand: Boolean
    ): List[Inline] = {
      if (!expand) {
        List(Text(token))
      } else {
        val text = if (canaux) {
          val wr = definition.term.wordRelation(token)
          val xs = wr.en
          _create_label(token, xs)
        } else {
          token
        }
        List(Text(text))
      }
    }

    private def _make_label_ja(
      definition: Glossary.Definition,
      token: String,
      canaux: Boolean,
      expand: Boolean
    ): List[Inline] = {
      if (!expand) {
        List(Text(token))
      } else {
        val text = if (canaux) {
          val wr = definition.term.wordRelation(token)
          val xs = wr.ja
          _create_label(token, xs)
        } else {
          token
        }
        List(Text(text))
      }
    }

    private def _create_label(token: String, ps: Seq[String]): String =
      if (ps.isEmpty)
        token
      else
        s"""$token (${ps.mkString(", ")})"""

    // private def _create_label(
    //   definition: Glossary.Definition,
    //   token: String,
    //   p: Option[Vector[String]],
    //   canaux: Boolean
    // ): List[Inline] = {
    //   val b = definition.term.acronym.toVector ++ p.toVector.flatten
    //   _create_label(token, b, canaux)
    // }

    // private def _create_label(
    //   definition: Glossary.Definition,
    //   token: String,
    //   p: Vector[String],
    //   canaux: Boolean
    // ): List[Inline] = {
    //   val b = definition.term.acronym.toVector ++ p
    //   _create_label(token, b, canaux)
    // }

    // private def _create_label(
    //   definition: Glossary.Definition,
    //   token: String,
    //   p: I18NHangar[String],
    //   canaux: Boolean
    // ): List[Inline] = {
    //   val b = p.mapValueCollection(x => definition.term.acronym.toVector ++ x)
    //   _create_label(token, b, canaux)
    // }

    // private def _create_label(
    //   token: String,
    //   p: Option[Vector[String]],
    //   canaux: Boolean
    // ): List[Inline] =
    //   _create_label(token, p.toVector.flatten, canaux)

    // private def _create_label(
    //   token: String,
    //   ps: Vector[String],
    //   canaux: Boolean
    // ): List[Inline] =
    //   List(Text(_create_label_text(token, ps, canaux)))

    // private def _create_label(
    //   token: String,
    //   p: I18NHangar[String],
    //   canaux: Boolean
    // ): List[Inline] =
    //   p.unify match {
    //     case Left(l) => _create_label(token, l, canaux)
    //     case Right(r) => _create_label(token, r, canaux)
    //   }

    // private def _create_label(
    //   token: String,
    //   p: Map[Locale, Vector[String]],
    //   canaux: Boolean
    // ): List[Inline] =
    //   p.toList.map {
    //     case (k, v) => Span.create(k, List(Text(_create_label_text(token, v, canaux))))
    //   }

    // private def _create_label_text(
    //   token: String,
    //   ps: Vector[String],
    //   canaux: Boolean
    // ): String =
    //   if (ps.isEmpty || !canaux)
    //     token
    //   else
    //     s"""$token (${ps.mkString(", ")})"""
  }
  object TextLinkProcessor {
    case class Holder(
      xs: Vector[Dox],
      ds: Set[Glossary.Definition]
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
      val empty = Holder(Vector.empty, Set.empty)

      def definitions(ps: Set[Glossary.Definition]) = empty.copy(ds = ps)
    }

    // case class Result(
    //   dox: Vector[Dox],
    //   definitions: Vector[Glossary.Definition] = Vector.empty
    // )
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
