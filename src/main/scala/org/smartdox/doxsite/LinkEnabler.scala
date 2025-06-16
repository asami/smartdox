package org.smartdox.doxsite

import java.io._
import org.goldenport.RAISE
import org.goldenport.tree._
import org.smartdox._
import org.smartdox.transformer._
import org.smartdox.metadata._

/*
 * @since   Mar.  7, 2025
 *  version Mar.  9, 2025
 *  version Apr.  5, 2025
 *  version May. 21, 2025
 * @version Jun. 16, 2025
 * @author  ASAMI, Tomoharu
 */
class LinkEnabler(
  val context: DoxSiteTransformer.Context
) extends DoxSiteTransformer {
  import LinkEnabler._

  override protected def dox_Transformers(
    context: DoxSiteTransformer.Context,
    node: TreeNode[Node],
    p: Page
  ): List[HomoTreeTransformer[Dox]] =
    if (context.config.doxsiteConfig.fold(true)(_.isLinkEnable(p)))
      List(new LinkEmbeder(context, node))
    else
      Nil
}

object LinkEnabler {
  import scala.collection.JavaConverters._
  import com.atilika.kuromoji.TokenizerBase
  import com.atilika.kuromoji.ipadic.Token
  import com.atilika.kuromoji.ipadic.Tokenizer

  class LinkEmbeder(
    val context: DoxSiteTransformer.Context,
    pageNode: TreeNode[Node]
  ) extends DoxInSiteTransformer {
    override protected def make_Node(
      node: TreeNode[Dox],
      content: Dox
    ): TreeTransformer.Directive[Dox] = {
      content match {
        // case m: Paragraph =>
        //   val s = m.toPlainText
        //   val tokens = _to_tokens(_tokenize(s))
        //   println(s"text: $s")
        //   println(s"tokens: $tokens")
        //   TreeTransformer.Directive.Default
        case m: Text =>
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
                      val href = create_href(pageNode, definition.page, definition.id)
                      val alt = definition.description.toPlainText
                      Hyperlink.create(m, href, alt)
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
                Z(definition.terms.foldLeft(ZZ(definition, xs))(_+_).r)
            }
            candidates.foldLeft(Z(Vector(m)))(_+_).r
          }
        case m: Dfn => directive_node(m)
        case m => directive_container_content(m)
      }
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
    case class Z(xs: Vector[DoxSiteToken] = Vector.empty) {
      val r = xs.filter(_.isAvailable)

      def +(rhs: Token) = {
        if (rhs.getPartOfSpeechLevel1().startsWith("名詞")) {
          val x = if (rhs.getPartOfSpeechLevel2().startsWith("接尾"))
            xs.lastOption match {
              case Some(s) => xs.init :+ s.merge(rhs)
              case None => xs :+ DoxSiteToken.create(rhs)
            }
            else
              xs :+ DoxSiteToken.create(rhs)
          copy(xs = x)
//          copy(xs = xs :+ DoxSiteToken.create(rhs))
        } else {
          this
        }
      }
    }
    ps.foldLeft(Z())(_+_).r
  }
}
