package org.smartdox.transformer

import scala.util.parsing.combinator.Parsers
import org.smartdox._
import scalaz._
import Scalaz._
import scala.collection.mutable.ArrayBuffer
import java.net.URI
import Dox._
import scala.util.parsing.input.Reader

/*
 * @since   Sep.  8, 2014
 * @version Sep.  9, 2014
 * @author  ASAMI, Tomoharu
 */
case class TechDoxTransformer() extends DoxTransformer {
  type Out = Dox

  def documentOut(d: Document) = {
    ???
  }

  def headOut(d: Head): Out = ???

  def bodyOut(d: Body): Out = ???

  def sectionOut(d: Section): Out = ???

  def divOut(d: Div): Out = ???

  def paragraphOut(d: Paragraph): Out = ???

  def textOut(d: Text): Out = ???

  def boldOut(d: Bold): Out = ???

  def italicOut(d: Italic): Out = ???

  def underlineOut(d: Underline): Out = ???

  def codeOut(d: Code): Out = ???

  def preOut(d: Pre): Out = ???

  def ulOut(d: Ul): Out = ???

  def olOut(d: Ol): Out = ???

  def liOut(d: Li): Out = ???

  def delOut(d: Del): Out = ???

  def hyperlinkOut(d: Hyperlink): Out = ???

  def referenceImgOut(d: ReferenceImg): Out = ???

  def tableOut(d: Table): Out = ???

  def spaceOut(d: Space): Out = ???

  def dlOut(d: Dl): Out = ???

  def dtOut(d: Dt): Out = ???

  def ddOut(d: Dd): Out = ???

  def fragmentOut(d: Fragment): Out = ???

  def figureOut(d: Figure): Out = ???

  def dotImgOut(d: DotImg): Out = ???

  def ditaaImgOut(d: DitaaImg): Out = ???
}
