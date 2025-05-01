package org.smartdox.transformer

import scalaz._, Scalaz._
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import java.net.URI
import org.goldenport.RAISE
import org.goldenport.parser.{ParseResult => GParseResult}
import org.smartdox._, Dox._

/*
 * @since   Jan. 11, 2012
 *  version Feb.  5, 2014
 *  version Jan.  5, 2015
 *  version Nov.  8, 2020
 *  version Jan. 17, 2021
 *  version Feb.  8, 2021
 * @version Apr.  3, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxTransformer extends Parsers {
  type Elem = Dox
  type Out

  def isDefaultCssIfRequired: Boolean = true // TODO

  def transformDocument(in: Dox): GParseResult[Out] = transformG(in)

  def transformAsIs(in: Dox): GParseResult[Out] = _result(_asis(in))

  private def _asis(in: Dox): ParseResult[Out] = _asis_parser(new DoxReader(in))

  private def _asis_parser: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Document => Success(documentOut(d), in.rest)
        case d: Div => Success(divOut(d), in.rest)
        case m => RAISE.notImplementedYetDefect
      }
    }
  }

  private def _result(p: ParseResult[Out]): GParseResult[Out] = p match {
    case s: Success[_] => GParseResult.success(s.result)
    case n: NoSuccess => GParseResult.error(n.msg)
  }

  def transform(in: Dox): ParseResult[Out] = {
    document(new DoxReader(in))
  } 

  def transformG(in: Dox): GParseResult[Out] = transform(in) match {
    case s: Success[_] => GParseResult.success(s.result)
    case n: NoSuccess => GParseResult.error(n.msg)
  } 

  def transformZ(in: Dox): Validation[NonEmptyList[String], Out] = transform(in) match {
    case s: Success[_] => s.get.success[String].toValidationNel
    case n: NoSuccess => n.msg.failure[Out].toValidationNel
  }

  def document: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Document => Success(documentOut(d), in.rest)
        case d =>
          val doc = _to_document(d)
          Success(documentOut(doc), in.rest)
      }
    }
  }

  private def _to_document(p: Dox): Document = {
    val css =
      if (isDefaultCssIfRequired)
        Some(org.smartdox.transformers.Dox2DomHtmlTransformer.defaultCss)
      else
        None
    val head = Head(css = css) // TODO (e.g. title)
    val body = Body(p)
    Document(head, body)
  }

  def documentOut(d: Document): Out

  def head: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Head => Success(headOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def headOut(d: Head): Out

  def body: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Body => Success(bodyOut(d, None), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def bodyOut(d: Body, title: Option[Out]): Out

  def section: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Section => Success(sectionOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def sectionOut(d: Section): Out

  def div: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Div => Success(divOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def divOut(d: Div): Out

  def paragraph: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Paragraph => Success(paragraphOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def paragraphOut(d: Paragraph): Out

  def text: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Text => Success(textOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def textOut(d: Text): Out

  def bold: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Bold => Success(boldOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def boldOut(d: Bold): Out

  def italic: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Italic => Success(italicOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def italicOut(d: Italic): Out

  def underline: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Underline => Success(underlineOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def underlineOut(d: Underline): Out

  def code: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Code => Success(codeOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def codeOut(d: Code): Out

  def pre: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Pre => Success(preOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def preOut(d: Pre): Out

  def ul: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Ul => Success(ulOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def ulOut(d: Ul): Out

  def ol: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Ol => Success(olOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def olOut(d: Ol): Out

  def li: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Li => Success(liOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def liOut(d: Li): Out

  def del: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Del => Success(delOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def delOut(d: Del): Out

  def hyperlink: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Hyperlink => Success(hyperlinkOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def hyperlinkOut(d: Hyperlink): Out

  def referenceImg: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: ReferenceImg => Success(referenceImgOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def referenceImgOut(d: ReferenceImg): Out

  def table: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Table => Success(tableOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def tableOut(d: Table): Out

  def space: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Space => Success(spaceOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def spaceOut(d: Space): Out

  def dl: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Dl => Success(dlOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def dlOut(d: Dl): Out

  def dt: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Dt => Success(dtOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def dtOut(d: Dt): Out

  def dd: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Dd => Success(ddOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def ddOut(d: Dd): Out

  def fragment: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Fragment => Success(fragmentOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def fragmentOut(d: Fragment): Out

  def figure: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: Figure => Success(figureOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def figureOut(d: Figure): Out

  def dotImg: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: DotImg => Success(dotImgOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def dotImgOut(d: DotImg): Out

  def ditaaImg: Parser[Out] = new Parser[Out] {
    def apply(in: Input) = {
      in.first match {
        case d: DitaaImg => Success(ditaaImgOut(d), in.rest)
        case d => Failure(d.showTerm, in) 
      }
    }
  }

  def ditaaImgOut(d: DitaaImg): Out
}
