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
 * @since   Jan. 11, 2021
 * @version Jan. 11, 2021
 * @author  ASAMI, Tomoharu
 */
case class Dox2BloggerTransformer(
  context: Context
) {
  def transform(in: Dox): ParseResult[String] = {
    for {
      x <- _transform(in)
      dom <- new Dox2DomHtmlTransformer(context).transformG(x)
      r <- ParseResult(_print(dom))
    } yield r
  }

  private def _transform(p: Dox): ParseResult[Dox] = {
    ???
  }

  private def _print(dom: Node): String = ???
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
