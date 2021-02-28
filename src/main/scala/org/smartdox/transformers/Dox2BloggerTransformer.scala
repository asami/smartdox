package org.smartdox.transformers

import scalaz._, Scalaz._
import java.net.URI
import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.Parsers
import scala.collection.mutable.ArrayBuffer
import org.w3c.dom.{Node}
import org.goldenport.parser.ParseResult
import org.goldenport.xml.dom.DomUtils
import org.smartdox._
import Dox._
import org.smartdox.generator.Context
import org.smartdox.transformer._

/*
 * Derived from SmartDox2BloggerRealmGenerator.
 *
 * @since   Jan. 11, 2012
 *  version Sep.  8, 2012
 *  version Jan. 12, 2021
 * @version Feb.  4, 2021
 * @author  ASAMI, Tomoharu
 */
case class Dox2BloggerTransformer(
  context: Context,
  isPretty: Boolean = true
) extends HtmlTransformerBase {
  import Dox2DoxTransformer._

  val isDocument: Boolean = false

  def transform(in: Dox): ParseResult[String] = {
    import Dox2DomHtmlTransformer.Rule._
    val rule = Dox2DomHtmlTransformer.Rule(
      Tactics(NodeKindGuard(DProgram), ElementAction(HPre, "name" -> "code", "class" -> "java")),
      Tactics(NodeKindGuard(DConsole), ElementAction(HPre, "name" -> "code", "class" -> "console")),
      Tactics({
        case m: Figure => TextAction(s"*** embed manually: ${m.img.src} ***")
      })
    )
    for {
      x <- _transform(in)
      dom <- new Dox2DomHtmlTransformer(context, rule).transformAsIs(x)
      r <- ParseResult(to_html(dom))
    } yield r
  }

  private def _transform(p: Dox): ParseResult[Dox] = {
    val root = BodyDivRootStrategy
    val rule = Rule(root)
    Dox2DoxTransformer.transform(rule, p)
  }
}

/*
  override protected def find_Root(t:Tree[Dox]): Option[Tree[Dox]] = {
    find(t)(_.rootLabel.isInstanceOf[Body])
  }

  override protected def transform_Dox(t: Tree[Dox]): Tree[Dox] = {
    replace(t) {
      case (b: Body, cs) => (Div, cs)
      case (s: Section, cs) => {
        val hname = "h" + (s.level + 3)
        (Div, Stream.cons(Dox.html5(hname, s.title) |> Dox.tree, cs))
      }
      case (p: Program, cs) => (Pre(program_text(p), List("name" -> "code", "class" -> "java")), cs)
      case (c: Console, cs) => (Pre(c.contents, List("class" -> "console")), cs)
      case (f: Figure, cs) => (Div, Stream(Text("*** embed manually: " + f.img.src + " ***").leaf))
    } // ensuring { x => println("_transform = " + x.drawTree); true}
  }
 */
