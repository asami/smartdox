package org.smartdox

import scalaz._, Scalaz._
import java.net.URI

/*
 * derived from SNode.java since Sep. 17, 2006
 * derived from SDoc.scala since Sep.  1, 2008
 *
 * @since   Dec. 24, 2011
 *  version Apr. 24, 2012
 *  version Jun.  5, 2012
 *  version Jul. 22, 2012
 *  version Nov. 23, 2012
 *  version Dec. 24, 2012
 * @version Jan. 16, 2014
 * @author  ASAMI, Tomoharu
 */
trait Dox extends NotNull { // Use EmptyDox for null object.
  val elements: List[Dox] = Nil
  lazy val attributeMap = Map(showParams: _*)
  def attribute(name: String): Option[String] = attributeMap.get(name)

  def showTerm = getClass.getSimpleName().toLowerCase()
  def showParams: List[(String, String)] = Nil
  lazy val showParamsText = showParams.map {
    case (k, v) => """%s="%s"""".format(k, v) 
  } mkString(" ")
  def showOpenCloseText = {
    val params = showParamsText.isEmpty ? "" | " " + showParamsText
    "<" + showTerm + params + "/>"
  }
  def showOpenText = {
    val params = showParamsText.isEmpty ? "" | " " + showParamsText
    "<" + showTerm + params + ">"
  }
  def showCloseText = "</" + showTerm + ">"
  def showContentsElements = elements
  def isOpenClose = showContentsElements.isEmpty

  def toString(buf: StringBuilder, maxlength: Option[Int] = None) {
    if (maxlength.map(_ <= buf.length) | false) {
      if (!buf.endsWith("...")) {
        buf ++= "..." 
      }
    } else {
      if (isOpenClose) {
        show_Open_Close(buf)
      } else {
        show_Open(buf)
        show_Contents(buf)
        show_Close(buf)
      }
    }
  }

  override def toString() = {
    val buf = new StringBuilder
    toString(buf)
    buf.toString()
  }

  def toShowString() = {
    val buf = new StringBuilder
    toString(buf, Some(50))
    buf.toString()
  }

  protected def show_Open_Close(buf: StringBuilder) {
    buf.append(showOpenCloseText)
  }

  protected def show_Open(buf: StringBuilder) {
    buf.append(showOpenText)
  }

  protected def show_Contents(buf: StringBuilder) {
    showContentsElements.foreach(_.toString(buf))
  }

  protected def show_Close(buf: StringBuilder) {
    buf.append(showCloseText)
  }

  def toText(): String = {
    val buf = new StringBuilder
    to_Text(buf)
    buf.toString
  }

  protected def to_Text(buf: StringBuilder) {
    showContentsElements.foreach(_.to_Text(buf))
  }

  def toData(): String = {
    val buf = new StringBuilder
    to_data(buf)
//    println("Dox#toData(%s) = %s".format(this, buf))
    buf.toString
  }

  protected def to_data(buf: StringBuilder) {
    to_Data_Prologue(buf)
    to_Data(buf)
    to_Data_Epilogue(buf)
  }

  protected def to_Data_Prologue(buf: StringBuilder) {
//    println("Dox#to_Data_Prologue = " + this)
  }

  protected def to_Data(buf: StringBuilder) {
    showContentsElements.foreach(_.to_data(buf))
  }

  protected def to_Data_Epilogue(buf: StringBuilder) {
//    println("Dox#to_Data_Epilogue = " + this)
  }

  def tree: Tree[Dox] = Dox.tree(this)

  // invoke copyWV
  def copyV(cs: List[Dox]): ValidationNEL[String, Dox] = {
//    copyWV(cs).over
//    copyVW(cs).map(_.over)
    sys.error("not implemented yet")
  }

  // invoke copyV
  @deprecated("Use copyVW", "0.2.4")
  def copyWV(cs: List[Dox]): Writer[List[String], ValidationNEL[String, Dox]] = {
    writer(nil, copyV(cs))
  }

  // invoke copyV
  def copyVW(cs: List[Dox]): ValidationNEL[String, Writer[List[String], Dox]] = {
    copyV(cs).map(writer(nil, _))
  }

  protected final def to_failure[T, U](o: T)(implicit s: Show[T]): Failure[NonEmptyList[String], U] = {
    Failure(NonEmptyList(to_failure_message(o)(s)))
  }

  protected final def to_failure_message[T](o: T)(implicit s: Show[T]) = {
    showTerm + ": " + s.show(o)
  }

  protected final def to_empty(cs: List[Dox]): ValidationNEL[String, List[Dox]] = {
    if (cs.isEmpty) Success(Nil)
    else to_failure(cs)
  }

  private def _to_vs(cs: List[Dox]): List[ValidationNEL[String, Dox]] = {
    cs.map(Success(_))
  }

  protected final def to_inline(cs: List[Dox]): ValidationNEL[String, List[Inline]] = {
    cs.foldr(Success(Nil): ValidationNEL[String, List[Inline]]) {
      case (i: Inline, Success(a)) => Success(i :: a)
      case (i: Inline, e: Failure[_, _]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_li(cs: List[Dox]): ValidationNEL[String, List[Li]] = {
    cs.foldr(Success(Nil): ValidationNEL[String, List[Li]]) {
      case (d: Li, Success(a)) => Success(d :: a)
      case (d: Li, e: Failure[_, _]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_tr(cs: List[Dox]): ValidationNEL[String, List[TR]] = {
    cs.foldr(Success(Nil): ValidationNEL[String, List[TR]]) {
      case (d: TR, Success(a)) => Success(d :: a)
      case (d: TR, e: Failure[_, _]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_tfield(cs: List[Dox]): ValidationNEL[String, List[TField]] = {
    cs.foldr(Success(Nil): ValidationNEL[String, List[TField]]) {
      case (d: TField, Success(a)) => Success(d :: a)
      case (d: TField, e: Failure[_, _]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_dtdd(cs: List[Dox]): ValidationNEL[String, List[(Dt, Dd)]] = {    
    object DtDd {
      def unapply(xs: List[_]): Option[(Dt, Dd)] = {
        xs match {
          case List(dt: Dt, dd: Dd) => (dt, dd).some
          case _ => None
        }
      }
    }

    val xs = cs.sliding(2, 2).toList
    xs.foldr(Success(Nil): ValidationNEL[String, List[(Dt, Dd)]]) {
      case (DtDd(dt, dd), Success(a)) => Success((dt, dd) :: a)
      case (DtDd(_, _), e: Failure[_, _]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_figure(cs: List[Dox]): ValidationNEL[String, (Option[Img], Option[Figcaption])] = {    
    val img = cs.collectFirst { case x: Img => x }
    val caption = cs.collectFirst { case x: Figcaption => x }
    Success((img, caption))
  }

  protected final def to_list_content(cs: List[Dox]): ValidationNEL[String, List[ListContent]] = {
    cs.foldr(Success(Nil): ValidationNEL[String, List[ListContent]]) {
      case (d: ListContent, Success(a)) => Success(d :: a)
      case (d: ListContent, e: Failure[_, _]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

/* don't work so that type A is erased
  protected final def to_listA[A](cs: List[Dox]): ValidationNEL[String, List[A]] = {
    cs.foldr(Success(Nil): ValidationNEL[String, List[A]]) {
      case (d: A, Success(a)) => Success(d :: a)
      case (d: A, e: Failure[_, _]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_text(cs: List[Dox]): ValidationNEL[String, String] = {
    _to_vs(cs).foldr(Success(""): ValidationNEL[String, String]) {
      case (e, a) => (e |@| a)(_ + _)
    }
  }
*/

  protected final def to_plain_text(cs: List[Dox]): ValidationNEL[String, String] = {
    val (ss, es) = cs.partition(_.isInstanceOf[Text])
    if (es.nonEmpty) "Not text".failNel
    else ss.mkString.success
  }

  def find(p: Dox => Boolean): Option[Dox] = {
    if (p(this)) this.some
    else {
      for (e <- elements) {
        val r = e.find(p)
        if (r.isDefined) return r
      }
      None
    }
  }

  def collectFirst[T](pf: PartialFunction[Dox, T]): Option[T] = {
    if (pf.isDefinedAt(this)) pf(this).some
    else {
      for (e <- elements) {
        if (pf.isDefinedAt(e)) return pf(e).some

      }
      None
    }
  }

  def toVW: Dox.DoxVW = Dox.vw(this)
}

trait Block extends Dox {
}

trait Inline extends Dox with ListContent {
}

trait ListContent extends Dox {  
}

trait UseDox {
  implicit def toDox(string: String): Dox = {
    parser.DoxParser.parseOrgmodeZ(string) match {
      case Success(s) => s
      case Failure(ms) => Ul(ms.list.map(Li(_)))
    }
  }

  implicit def toFragment[T <: Dox](contents: List[T]): Fragment = {
    new Fragment(contents)
  }

  implicit def DoxShow: Show[Dox] = shows(_.toShowString)
}

object Dox extends UseDox {
  type DoxV = ValidationNEL[String, Dox]
  type DoxW = Writer[List[String], Dox]
  type DoxVW = ValidationNEL[String, Writer[List[String], Dox]]
  type DoxWV = Writer[List[String], DoxV]
  type TreeDoxV = ValidationNEL[String, Tree[Dox]]
  type TreeDoxW = Writer[List[String], Tree[Dox]]
  type TreeDoxVW = ValidationNEL[String, Writer[List[String], Tree[Dox]]]
  type TreeDoxWV = Writer[List[String], TreeDoxV]

  def tree(dox: Dox): Tree[Dox] = {
    Scalaz.node(dox, dox.elements.toStream.map(tree))
  }

  def untreeE(tree: Tree[Dox]): Dox = {
    untreeV(tree) match {
      case Success(d) => d
      case Failure(e) => throw new IllegalArgumentException(e.list.mkString(";"))
    }
  }

  def untreeO(tree: Tree[Dox]): Option[Dox] = {
    untreeV(tree).toOption
  }

  def untreeV(tree: Tree[Dox]): ValidationNEL[String, Dox] = {
//    println("untreeV: " + tree.drawTree)
    val children = tree.subForest.map(untreeV).toList
//    println("children -> errors: " + children + " , " + tree.subForest.toList.map(_.rootLabel))
    val errors = children.flatMap {
      case Success(d) => Nil
      case Failure(e) => e.list
    }
//    if (errors.nonEmpty) {
//      println("children -> errors: " + children + "," + errors + "/" + tree.subForest.toList)
//    }
    if (errors.nonEmpty) {
      Failure(errors.toNel.get)
    } else {
//      println("untreeV success = " + tree.drawTree)
      val cs = children.collect {
          case Success(d) => d
      }
//      println("untreeV success children = " + cs)
      val r = tree.rootLabel.copyV(cs)
//      println("untreeV success result = " + r.either.right.toString)
      r
    }
  }

  @deprecated("Use untreeVW", "0.2.4")
  def untreeWV(tree: Tree[Dox]): Writer[List[String], ValidationNEL[String, Dox]] = {
//    println("untreeV: " + tree.drawTree)
    val children = tree.subForest.map(untreeWV).toList
//    println("children -> errors: " + children + " , " + tree.subForest.toList.map(_.rootLabel))
    val errors = children.map(_.over).flatMap {
      case Success(d) => Nil
      case Failure(e) => e.list
    }
    val log = children.flatMap(_.written)
//    if (errors.nonEmpty) {
//      println("children -> errors: " + children + "," + errors + "/" + tree.subForest.toList)
//    }
    if (errors.nonEmpty) {
      writer(log, Failure(errors.toNel.get))
    } else {
//      println("untreeV success = " + tree.drawTree)
      val cs = children.map(_.over).collect {
          case Success(d) => d
      }
//      println("untreeV success children = " + cs)
      val r = tree.rootLabel.copyWV(cs)
//      println("untreeV success result = " + r.either.right.toString)
      writer(log ::: r.written, r.over)
    }    
  }

  def untreeVW(tree: Tree[Dox]): ValidationNEL[String, Writer[List[String], Dox]] = {
    val children = tree.subForest.map(untreeVW).toList
    val errors = children.flatMap {
      case Success(d) => Nil
      case Failure(e) => e.list
    }
    if (errors.nonEmpty) {
      errors.toNel.get.fail
    } else {
      val cs = children.collect {
          case Success(d) => d
      }
      val log = cs.flatMap(_.written)
      val r = tree.rootLabel.copyVW(cs.map(_.over))
//      println("untreeVW <= " + tree.drawTree)
//      r.foreach(x => println("untreeVM => " + x.over.toString))
      r.map(x => writer(log ::: x.written, x.over))
    }    
  }

  def untree(tree: Tree[Dox]) = untreeE(tree)

  val treeLens: Lens[Dox, Tree[Dox]] = {
    Lens(tree, (d, t) => untree(t))
  }

  val treeLensV: Lens[DoxV, TreeDoxV] = {
    Lens((d: DoxV) => d.map(tree),
        (d, t) => t.map(untree))
  }

  val treeLensVW: Lens[DoxVW, TreeDoxVW] = {
    def pushback(t: TreeDoxVW): DoxVW = {
      t.flatMap(x => untreeVW(x.over))
    }
    Lens((d: DoxVW) => d.map(_.map(tree)),
        (d, t) => pushback(t))
  }

  def tableLens(p: Table => Boolean = {(x: Table) => true}): Lens[Dox, Table] = {
    sys.error("not implemented yet")
  }

  def html5(name: String, children: List[Dox]) = {
    Html5(name, Nil, children)
  }

  def vw(d: Dox): DoxVW = {
    success(writer(nil, d))
  }

  // derived from UXML
  def escape(string: String) = {
    if (string.indexOf('<') == -1 &&
        string.indexOf('>') == -1 &&
        string.indexOf('&') == -1 &&
        string.indexOf('"') == -1 &&
        string.indexOf('\'') == -1) {
      string
    } else {
      val buf = new StringBuilder()
      val size = string.length();
      for (i <- 0 until size) {
        string.charAt(i) match {
          case '<'  => buf.append("&lt;")
          case '>'  => buf.append("&gt;")
          case '&'  => buf.append("&amp;")
          case '"'  => buf.append("&quot;")
          case '\'' => buf.append("&apos;")
          case c    => buf.append(c)
        }
      }
      buf.toString // ensuring {x => println("ESCAPE: " + string + " => " + x);true}
    }
  }
}

case class Document(head: Head, body: Body) extends Dox {
  override val elements = List(head, body)
  override def showTerm = "html"
  override def showOpenText = "<!DOCTYPE html><html>"
  override def showCloseText = "</html>"

  override def copyV(cs: List[Dox]) = {
    def s(h: Head, b: Body): ValidationNEL[String, Dox] = {
      Success(copy(h, b))
    }

    cs.length match {
      case 0 => s(head, body)
      case 1 => cs.head match {
        case h: Head => s(h, body)
        case b: Body => s(head, b)
        case d => to_failure(d)
      }
      case 2 => {
        val h: ValidationNEL[String, Head] = cs(0) match {
          case h: Head => Success(h)
          case d => to_failure(d)
        }
        val b: ValidationNEL[String, Body] = cs(1) match {
          case b: Body => Success(b)
          case d => to_failure(d)
        }
        (h |@| b) { case (h, b) => copy(h, b) }
      }
      case _ => to_failure(cs) 
    }
  }
}

case class Head(
    title: InlineContents = Nil,
    author: InlineContents = Nil,
    date: InlineContents = Nil) extends Dox {
  override def copyV(cs: List[Dox]) = {
    if (cs.isEmpty) Success(this)
    else to_failure(cs)
  }

  override def show_Open(buf: StringBuilder) {
    def showslot(name: String, contents: InlineContents) {
      if (contents.nonEmpty) {
        buf.append("<")
        buf.append(name)
        buf.append(">")
        contents.foreach(_.toString(buf))
        buf.append("</")
        buf.append(name)
        buf.append(">")
      }
    }
    buf.append(showOpenText)
    showslot("title", title)
    showslot("author", author)
    showslot("date", date)
  }

  override def isOpenClose = title.isEmpty && author.isEmpty && date.isEmpty
}

object Head {
  def builder() = new Builder

  class Builder {
    var title: InlineContents = Nil
    var author: InlineContents = Nil
    var date: InlineContents = Nil

    def build() = new Head(title, author, date)
  }
}

case class Body(contents: List[Dox]) extends Dox {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs))
  }
}

object Body {
  def apply(element: Dox) = new Body(List(element))
}

case class Section(title: List[Inline], contents: List[Dox], level: Int = 1) extends Dox {
  override val elements = contents
  override def show_Open(buf: StringBuilder) {
    val showh = "h" + (level + 1) 
    buf.append(showOpenText)
    buf.append("<")
    buf.append(showh)
    buf.append(">")
    title.foreach(_.toString(buf))
    buf.append("</")
    buf.append(showh)
    buf.append(">")
  }
  override def isOpenClose = false

  override def copyV(cs: List[Dox]) = {
    Success(copy(title, cs, level)) // XXX level
  }
}

case class Div(contents: List[Dox] = Nil) extends Block {
  override def showTerm = "div"
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs))
  }
}

object Div extends Div(Nil) {
  def apply(d: Dox) = new Div(List(d))
}

case class Paragraph(contents: List[Dox]) extends Block {
  override val elements = contents
  override def showTerm = "p"

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs))
  }
}

case class Text(contents: String) extends Inline {
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""
  override def show_Contents(buf: StringBuilder) {
    buf.append(Dox.escape(contents))
  }
  override def to_Text(buf: StringBuilder) {
    buf.append(contents)
  }
  override def to_Data(buf: StringBuilder) {
    buf.append(contents)
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

case class Bold(contents: List[Inline]) extends Inline {
  override val elements = contents
  override def showTerm = "b"

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

object Bold extends Bold(Nil) {
  def apply(element: Inline) = new Bold(List(element))
}

// 2011-12-26
case class Italic(contents: List[Inline]) extends Inline {
  override val elements = contents
  override def showTerm = "i"

  override def to_Data_Prologue(buf: StringBuilder) {
    buf.append("/")
  }

  override def to_Data_Epilogue(buf: StringBuilder) {
    buf.append("/")
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

object Italic extends Italic(Nil) {
  def apply(element: Inline) = new Italic(List(element))
}

case class Underline(contents: List[Inline]) extends Inline {
  override val elements = contents
  override def showTerm = "u"

  override def to_Data_Prologue(buf: StringBuilder) {
    buf.append("_")
  }

  override def to_Data_Epilogue(buf: StringBuilder) {
    buf.append("_")
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

object Underline extends Underline(Nil) {
  def apply(element: Inline) = new Underline(List(element))
}

case class Code(contents: List[Inline]) extends Inline {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

object Code extends Code(Nil) {
  def apply(element: Inline) = new Code(List(element))
}

case class Pre(contents: String, attributes: List[(String, String)] = Nil) extends Inline {
  override val elements = List(Text(contents))
  override def showParams = attributes

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs) >| copy(contents, attributes)
  }
}

case class Ul(contents: List[Li]) extends Block with ListContent {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_li(cs).map(copy)
  }
}

object Ul {
  def apply(element: Li) = new Ul(List(element))
}

case class Ol(contents: List[Li]) extends Block with ListContent {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_li(cs).map(copy)
  }
}

case class Li(contents: List[ListContent]) extends Block {
  override val elements = contents

  def :+(elem: ListContent): Li = {
    Li(contents :+ elem)
  }

  override def copyV(cs: List[Dox]) = {
    to_list_content(cs).map(copy)
  }
}

object Li {
  def apply(text: String) = new Li(List(Text(text)))
  def apply(element: ListContent) = new Li(List(element))
}

// 2011-12-30
case class Del(contents: List[Inline]) extends Inline {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

object Del extends Del(Nil) {
  def apply(element: Inline) = new Del(List(element))
}

case class Hyperlink(contents: List[Inline], href: URI) extends Inline {
  override val elements = contents
  override def showTerm = "a"
  override def showParams = List("href" -> href.toASCIIString())

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, href))
  }
}

case class ReferenceImg(src: URI) extends Img {
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

trait TableBlock extends Block {
  val caption: Option[Caption]
  val label: Option[String]
}

case class Table(head: Option[THead], body: TBody, foot: Option[TFoot], 
    caption: Option[Caption], label: Option[String]) extends TableBlock {
  override val elements = List(caption, head, body.some, foot).flatten
  override def showParams = label.toList.map(x => ("id", x))

  override def copyV(cs: List[Dox]) = {
    if (cs.isEmpty) to_failure(cs)
    else {
      val (c, cs1) = cs match {
        case (c: Caption) :: xs => (Some(c), xs)
        case _ => (None, cs)
      }
      val (h, cs2) = cs1 match {
        case (h: THead) :: xs => (Some(h), xs)
        case _ => (None, cs1)
      }
      val (b, cs3) = cs2 match {
        case (b: TBody) :: xs => (Some(b), xs)
        case _ => (None, cs2)
      }
      val (f, cs4) = cs3 match {
        case (f: TFoot) :: xs => (Some(f), xs)
        case _ => (None, cs3)
      }
      if (b.isEmpty || cs4.nonEmpty) {
        to_failure(cs)
      } else {
        Success(copy(h, b.get, f, c, label))
      }
    }
  }

  def width: Int = {
    List(head, body.some, foot).flatten.map(_.width).max
  }

  def height: Int = {
    List(head, body.some, foot).flatten.map(_.height).sum
  }
}

trait TableCompartment extends Block {
  val records: List[TRecord]
  override val elements = records

  def width: Int = records.map(_.length).max
  def height: Int = records.length

  def getText(x: Int, y: Int): String = {
    if (records.length > y) {
      val r = records(y)
      if (r.fields.length > x) {
        val f = r.fields(x)
        return f.toText
      }
    }
    return ""
  }

  def getField(x: Int, y: Int): Option[TField] = {
    if (records.length > y) {
      val r = records(y)
      if (r.fields.length > x) {
        return r.fields(x).some
      }
    }
    return None
  }

  def getContent(x: Int, y: Int): Option[List[Inline]] = {
    getField(x, y).map(_.contents)
  }

  def getData(x: Int, y: Int): String = {
    getField(x, y).map(_.toData) | ""
  }
}

trait TRecord extends Block {
  val fields: List[TField]
  def length: Int
}

case class THead(records: List[TRecord]) extends TableCompartment {
  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy)
  }
}

case class TBody(records: List[TRecord]) extends TableCompartment {
  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy)
  }
}

case class TFoot(records: List[TRecord]) extends TableCompartment {
  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy)
  }
}

case class TR(fields: List[TField]) extends TRecord {
  override val elements = fields

  override def copyV(cs: List[Dox]) = {
    to_tfield(cs).map(copy)
  }
  def length = fields.length
}

trait TField extends Block {
  val contents: List[Inline]
  override val elements = contents
}

case class TD(contents: List[Inline]) extends TField {
  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

case class TH(contents: List[Inline]) extends TField {  
  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

case class TTable(uri: String, params: List[String],
                  caption: Option[Caption] = None,
                  label: Option[String] = None
                ) extends TableBlock with TRecord { // with TField { // 2012-07-04
//  override val contents = Nil
  val contents = Nil
  override val elements = List(caption).flatten
  override def showParams = ("uri", uri) :: params.flatMap {
    _.trim.split(":").toList match {
      case Nil => Nil
      case "" :: Nil => Nil
      case x :: Nil => List(x.trim -> "true")
      case x :: xs => List(x.trim -> xs.mkString(":").trim)
    } 
  }

  val fields = Nil
  def length = 0

  override def copyV(cs: List[Dox]) = {
//    println("TTable copyV = " + cs)
    Success(this) // XXX
  }
}

case class Space() extends Inline {
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""
  override def show_Contents(buf: StringBuilder) {
    buf.append(" ")
  }
  override def to_Text(buf: StringBuilder) {
    buf.append(" ")
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

case class Dl(contents: List[(Dt, Dd)]) extends Block {
  override val elements: List[Dox] = contents flatMap {
    case (dt, dd) => List(dt, dd)
  }

  override def copyV(cs: List[Dox]) = {
    to_dtdd(cs).map(copy)
  }
}

case class Dt(contents: String) extends Block {
  override val elements = List(Text(contents))

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(_ => this)
  }
}

case class Dd(contents: List[Inline]) extends Block {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

case class Fragment(contents: List[Dox]) extends Dox with Block with Inline with ListContent {
  override val elements = contents
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs))
  }
}

case class Caption(contents: List[Inline]) extends Block {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

// 2011-12-31
case class Figure(img: Img, caption: Figcaption, label: Option[String] = None) extends Block {
  override val elements = List(img, caption)
  override def showParams = List("id" -> label).flatMap(_.sequence)

  override def copyV(cs: List[Dox]) = {
    to_figure(cs).map { case (i, c) =>
      copy(i | img, c | caption, label)
    }
  }
}

case class Figcaption(contents: List[Inline]) extends Block {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

case class EmptyLine() extends Block {  
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2011-01-01
case class Newline() extends Inline {
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""
  override def show_Contents(buf: StringBuilder) {
    buf.append("\n")
  }
  override def to_Text(buf: StringBuilder) {
    buf.append("\n")
  }
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

trait Img extends Inline {
  val src: URI
  override val elements = Nil
  override def showTerm = "img"
  override def showParams = List("src" -> src.toASCIIString())  
}

trait EmbeddedImg extends Img {
  val contents: String
  val params: List[String]
}

case class DotImg(src: URI, contents: String, params: List[String] = Nil) extends EmbeddedImg {
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

case class DitaaImg(src: URI, contents: String, params: List[String] = Nil) extends EmbeddedImg {
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2011-01-16
case class Html5(name: String, attributes: List[(String, String)], contents: List[Dox]) extends Block {
  override val elements = contents
  override def showTerm = name

  override def copyV(cs: List[Dox]) = {
    Success(copy(name, attributes, cs))
  }
}

// 2011-01-17
case class Program(contents: String, attributes: List[(String, String)] = Nil) extends Block {
  override val elements = List(new Text(contents))
  override def showTerm = "pre"
  override def showParams = attributes

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs).map(_ => this)
  }
}

case class Console(contents: String, attributes: List[(String, String)] = Nil) extends Block {
  override val elements = List(new Text(contents))
  override def showTerm = "console"
  override def showParams = attributes

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs).map(_ => this)
  }
}

// 2011-01-18
case class SmartDoc(name: String, attributes: List[(String, String)], contents: List[Dox]) extends Block {
  override val elements = contents
  override def showTerm = name
  override def showParams = attributes

  override def copyV(cs: List[Dox]) = {
    Success(copy(name, attributes, cs))
  }
}

// 2011-01-20
case class SmCsvImg(src: URI, contents: String, params: List[String] = Nil) extends EmbeddedImg {
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2012-02-15
object EmptyDox extends Dox {
  override def toString(buf: StringBuilder, maxlength: Option[Int] = None) {
  }
}

// 2012-04-24
case class Tt(contents: List[Inline]) extends Inline {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

object Tt extends Tt(Nil) {
  def apply(element: Inline) = new Tt(List(element))
}

// 2012-06-05
case class Span(contents: List[Inline]) extends Inline {
  override val elements = contents
  override def showTerm = "span"

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy)
  }
}

object Span extends Span(Nil) {
  def apply(element: Inline) = new Span(List(element))
}

// 2012-11-23
case class IncludeDoc(filename: String) extends Block {
  override def showParams = List(("filename", filename))

  override def copyV(cs: List[Dox]) = {
//    println("IncludeDoc#copyV: " + cs)
    Div(cs).success
  }
}
