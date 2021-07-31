package org.smartdox

import scala.language.implicitConversions
import scalaz._, Scalaz._, WriterT._, Show._, Validation._
import java.net.URI
import scala.xml.{Node => XNode, _}
import org.goldenport.RAISE
import org.goldenport.collection.VectorMap
import org.goldenport.parser._
import org.goldenport.io.MimeType
import org.goldenport.bag.ChunkBag
import org.goldenport.extension.IDocument
import org.goldenport.extension.IRecord
import org.goldenport.tree.{Tree => GTree}
import org.goldenport.util.AnyUtils

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
 *  version Jan. 29, 2014
 *  version Feb.  5, 2014
 *  version Sep.  9, 2014
 *  version Jan.  5, 2015
 *  version Nov. 18, 2018
 *  version Dec. 31, 2018
 *  version Jan. 12, 2019
 *  version Feb.  6, 2019
 *  version Apr. 18, 2019
 *  version Aug.  8, 2019
 *  version Jun.  7, 2020
 *  version Jul. 26, 2020
 *  version Sep. 21, 2020
 *  version Oct. 18, 2020
 *  version Nov. 29, 2020
 *  version Dec. 27, 2020
 *  version Jan. 12, 2021
 *  version Feb. 15, 2021
 *  version Mar. 14, 2021
 *  version Apr.  3, 2021
 *  version May. 19, 2021
 *  version Jun. 20, 2021
 * @version Jul. 12, 2021
 * @author  ASAMI, Tomoharu
 */
trait Dox extends IDocument with NotNull { // Use EmptyDox for null object.
  def location: Option[ParseLocation]
  def isEmpty: Boolean = elements.isEmpty
  val elements: List[Dox] = Nil
  def sections: List[Section] = sectionsShallow
  lazy val sectionsShallow = elements collect { case m: Section => m }
  def tables: List[Table] = tablesShallow
  lazy val tablesShallow = elements collect { case m: Table => m }
  lazy val attributeMap = VectorMap(showParams)
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

  def takeHtmlTag: String = getHtmlTag getOrElse RAISE.noReachDefect(s"${this}")
  def getHtmlTag: Option[String] = Some(showTerm)

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

  // for Hocon
  def toPlainText(): String = {
    val buf = new StringBuilder
    to_Plain_Text(buf)
    buf.toString
  }

  protected def to_Plain_Text(buf: StringBuilder) {
    showContentsElements.foreach(_.to_Plain_Text(buf))
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

  def toTree: GTree[Dox] = Dox.toTree(this)

  def tree: Tree[Dox] = Dox.tree(this)

  def copyV(cs: List[Dox]): ValidationNel[String, Dox] =
    if (cs.isEmpty)
      Success(this)
    else
      copy_V(cs: List[Dox])

  protected def copy_V(cs: List[Dox]): ValidationNel[String, Dox] =
    RAISE.noReachDefect(s"copy_V: not implemented yet($this): $cs")

  // invoke copyV
  @deprecated("Use copyVW", "0.2.4")
  def copyWV(cs: List[Dox]): Writer[List[String], ValidationNel[String, Dox]] = {
    writer((Nil, copyV(cs)))
  }

  // invoke copyV
  def copyVW(cs: List[Dox]): ValidationNel[String, Writer[List[String], Dox]] = {
    copyV(cs).map(writer(Nil, _))
  }

  protected final def to_failure[T, U](o: T)(implicit s: Show[T]): Failure[NonEmptyList[String]] = {
    Failure(NonEmptyList(to_failure_message(o)(s)))
  }

  protected final def to_failure_message[T](o: T)(implicit s: Show[T]) = {
    showTerm + ": " + s.show(o)
  }

  protected final def to_empty(cs: List[Dox]): ValidationNel[String, List[Dox]] = {
    if (cs.isEmpty) Success(Nil)
    else to_failure(cs)
  }

  private def _to_vs(cs: List[Dox]): List[ValidationNel[String, Dox]] = {
    cs.map(Success(_))
  }

  protected final def normalize_fragment(cs: List[Dox]): List[Dox] =
    cs.flatMap {
      case m: Fragment => normalize_fragment(m.contents)
      case m => List(m)
    }

  protected final def to_inline(cs: List[Dox]): ValidationNel[String, List[Inline]] = {
    cs.foldRight(Success(Nil): ValidationNel[String, List[Inline]]) {
      case (i: Inline, Success(a)) => Success(i :: a)
      case (i: Inline, e: Failure[_]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_li(cs: List[Dox]): ValidationNel[String, List[Li]] = {
    cs.foldRight(Success(Nil): ValidationNel[String, List[Li]]) {
      case (d: Li, Success(a)) => Success(d :: a)
      case (d: Li, e: Failure[_]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_tr(cs: List[Dox]): ValidationNel[String, List[TR]] = {
    cs.foldRight(Success(Nil): ValidationNel[String, List[TR]]) {
      case (d: TR, Success(a)) => Success(d :: a)
      case (d: TR, e: Failure[_]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_tfield(cs: List[Dox]): ValidationNel[String, List[TField]] = {
    cs.foldRight(Success(Nil): ValidationNel[String, List[TField]]) {
      case (d: TField, Success(a)) => Success(d :: a)
      case (d: TField, e: Failure[_]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_dtdd(cs: List[Dox]): ValidationNel[String, List[(Dt, Dd)]] = {    
    object DtDd {
      def unapply(xs: List[_]): Option[(Dt, Dd)] = {
        xs match {
          case List(dt: Dt, dd: Dd) => (dt, dd).some
          case _ => None
        }
      }
    }

    val xs = cs.sliding(2, 2).toList
    xs.foldRight(Success(Nil): ValidationNel[String, List[(Dt, Dd)]]) {
      case (DtDd(dt, dd), Success(a)) => Success((dt, dd) :: a)
      case (DtDd(_, _), e: Failure[_]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_figure(cs: List[Dox]): ValidationNel[String, (Option[Img], Option[Figcaption])] = {    
    val img = cs.collectFirst { case x: Img => x }
    val caption = cs.collectFirst { case x: Figcaption => x }
    Success((img, caption))
  }

  protected final def to_list_content(cs: List[Dox]): ValidationNel[String, List[ListContent]] = {
    cs.foldRight(Success(Nil): ValidationNel[String, List[ListContent]]) {
      case (d: ListContent, Success(a)) => Success(d :: a)
      case (d: ListContent, e: Failure[_]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

/* don't work so that type A is erased
  protected final def to_listA[A](cs: List[Dox]): ValidationNel[String, List[A]] = {
    cs.foldRight(Success(Nil): ValidationNel[String, List[A]]) {
      case (d: A, Success(a)) => Success(d :: a)
      case (d: A, e: Failure[_]) => e
      case (d, Success(a)) => to_failure(d)
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
    }
  }

  protected final def to_text(cs: List[Dox]): ValidationNel[String, String] = {
    _to_vs(cs).foldRight(Success(""): ValidationNel[String, String]) {
      case (e, a) => (e |@| a)(_ + _)
    }
  }
*/

  protected final def to_plain_text(cs: List[Dox]): ValidationNel[String, String] = {
    val (ss, es) = cs.partition(_.isInstanceOf[Text])
    if (es.nonEmpty) "Not text".failureNel
    else ss.mkString.success
  }

  protected def get_location(p: Option[ParseLocation], ps: Seq[Dox]): Option[ParseLocation] =
    p orElse get_location(ps)

  protected def get_location(ps: Seq[Dox]): Option[ParseLocation] =
    ps.toStream.flatMap(_.location).headOption

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

trait Block extends Dox with ListContent {
}

trait Inline extends Dox with ListContent {
}

trait ListContent extends Dox {  
}

trait UseDox {
  // implicit def toDox(string: String): Dox = {
  //   parser.DoxParser.parseOrgmodeZ(string) match {
  //     case Success(s) => s
  //     case Failure(ms) => Ul(ms.list.map(Li(_)))
  //   }
  // }

  implicit def toFragment[T <: Dox](contents: List[T]): Fragment = {
    new Fragment(contents)
  }

  implicit def DoxShow: Show[Dox] = shows(_.toShowString)
}

trait DoxFactory extends Doxes {
  def label: String

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Dox

  def applyOption(label: String, attrs: VectorMap[String, String], body: Seq[Dox]): Option[Dox] =
    if (label == this.label)
      Some(apply(attrs, body))
    else
      None

  protected final def ensure_inline(ps: Seq[Dox]): List[Inline] =
    ps.collect {
      case m: Inline => m
      case m => RAISE.noReachDefect
    }.toList

  protected final def ensure_li(ps: Seq[Dox]): List[Li] =
    ps.collect {
      case m: Li => m
      case m => RAISE.noReachDefect
    }.toList

  protected final def ensure_list_content(ps: Seq[Dox]): List[ListContent] =
    ps.collect {
      case m: ListContent => m
      case m => RAISE.noReachDefect
    }.toList

  protected final def ensure_dtdd(ps: Seq[Dox]): List[(Dt, Dd)] =
    RAISE.notImplementedYetDefect
}

object Dox extends UseDox {
  type DoxV = ValidationNel[String, Dox]
  type DoxW = Writer[List[String], Dox]
  type DoxVW = ValidationNel[String, Writer[List[String], Dox]]
  type DoxWV = Writer[List[String], DoxV]
  type TreeDoxV = ValidationNel[String, Tree[Dox]]
  type TreeDoxW = Writer[List[String], Tree[Dox]]
  type TreeDoxVW = ValidationNel[String, Writer[List[String], Tree[Dox]]]
  type TreeDoxWV = Writer[List[String], TreeDoxV]

  val empty = Fragment.empty

  val tags: Vector[DoxFactory] = Vector(
    Document,
    Head,
    Body,
    Div,
    Bold,
    Italic,
    Underline,
    Code,
    Pre,
    Ul,
    Ol,
    Li,
    Del,
    Hyperlink,
    ReferenceImg,
    Dl,
    Dt,
    Dd,
    Fragment,
    EmptyDox,
    Tt,
    Span
  )

  def toDox(ps: Seq[Dox]): Dox = ps.filter {
    case m: Div if m.contents.isEmpty => false
    case m: Span if m.contents.isEmpty => false
    case _ => true
  }.toList match {
    case Nil => Dox.empty
    case x :: Nil => x
    case xs => Fragment(xs)
  }

  def toTree(p: Dox): GTree[Dox] = {
    val t = GTree.create[Dox](p)
    RAISE.notImplementedYetDefect
  }

  def toDox(p: GTree[Dox]): Dox = {
    RAISE.notImplementedYetDefect
  }

  def tree(dox: Dox): Tree[Dox] = {
    // println(s"Dox#tree in: $dox/${dox.elements}")
    val r = Tree.node(dox, dox.elements.toStream.map(tree))
    // println(s"Dox#tree out: $dox => ${r.drawTree}")
    r
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

  def untreeV(tree: Tree[Dox]): ValidationNel[String, Dox] = {
    // println(s"untreeV in: ${tree.drawTree}")
    // println(s"untreeV in XXX: ${_show(tree.subForest)}")
    // println(s"untreeV in YYY: ${_show_object(tree.subForest)}")
    val children: List[ValidationNel[String, Dox]] = tree.subForest.map(untreeV).toList
    // println(s"untreeV in after: ${tree.drawTree}")
    // println(s"""untreeV children XXX: ${children}""")
    // println(s"""untreeV children: ${children.map(_show).mkString("\n")}""")
    // println("children -> errors: " + children + " , " + tree.subForest.toList.map(_.rootLabel))
    val errors = children.flatMap {
      case Success(d) => Nil
      case Failure(e) => e.list
    }
   // if (errors.nonEmpty) {
   //   println("children -> errors: " + children + "," + errors + "/" + tree.subForest.toList)
   // }
    if (errors.nonEmpty) {
      Failure(errors.toNel.get)
    } else {
     // println("untreeV success = " + tree.drawTree)
      val cs = children.collect {
          case Success(d) => d
      }
      // println(s"untreeV success parent = ${tree.rootLabel}")
      // println("untreeV success children = " + cs)
      val r = tree.rootLabel.copyV(cs)
      // println("untreeV success result = " + _show(r))
      r
    }
  }

  private def _show(ps: Stream[Tree[Dox]]) = ps.map(_.drawTree).mkString("\n")

  private def _show_object(ps: Stream[Tree[Dox]]) = ps.toVector.map(_.rootLabel.getClass.getSimpleName).mkString(",")

  private def _show(p: ValidationNel[String, Dox]) = p match {
    case Success(d) => s"${d.getClass.getSimpleName}: ${d.show}" // d.toString
    case Failure(e) => e.toString
  }

  @deprecated("Use untreeVW", "0.2.4")
  def untreeWV(tree: Tree[Dox]): Writer[List[String], ValidationNel[String, Dox]] = {
//    println("untreeV: " + tree.drawTree)
    val children = tree.subForest.map(untreeWV).toList
//    println("children -> errors: " + children + " , " + tree.subForest.toList.map(_.rootLabel))
    val errors = children.map(_.value).flatMap {
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
      val cs = children.map(_.value).collect {
          case Success(d) => d
      }
//      println("untreeV success children = " + cs)
      val r = tree.rootLabel.copyWV(cs)
//      println("untreeV success result = " + r.either.right.toString)
      writer(log ::: r.written, r.value)
    }    
  }

  def untreeVW(tree: Tree[Dox]): ValidationNel[String, Writer[List[String], Dox]] = {
    val children = tree.subForest.map(untreeVW).toList
    val errors = children.flatMap {
      case Success(d) => Nil
      case Failure(e) => e.list
    }
    if (errors.nonEmpty) {
      errors.toNel.get.failure
    } else {
      val cs = children.collect {
          case Success(d) => d
      }
      val log = cs.flatMap(_.written)
      val r = tree.rootLabel.copyVW(cs.map(_.value))
//      println("untreeVW <= " + tree.drawTree)
//      r.foreach(x => println("untreeVM => " + x.value.toString))
      r.map(x => writer(log ::: x.written, x.value))
    }    
  }

  def untree(tree: Tree[Dox]) = untreeE(tree)

/*
  val treeLens: Lens[Dox, Tree[Dox]] = {
    Lens(tree, (d, t) => untree(t))
  }

  val treeLensV: Lens[DoxV, TreeDoxV] = {
    Lens((d: DoxV) => d.map(tree),
        (d, t) => t.map(untree))
  }

  val treeLensVW: Lens[DoxVW, TreeDoxVW] = {
    def pushback(t: TreeDoxVW): DoxVW = {
      t.flatMap(x => untreeVW(x.value))
    }
    Lens((d: DoxVW) => d.map(_.map(tree)),
        (d, t) => pushback(t))
  }

  def tableLens(p: Table => Boolean = {(x: Table) => true}): Lens[Dox, Table] = {
    sys.error("not implemented yet")
  }
*/

  def html5(name: String, children: List[Dox]) = {
    Html5(name, Nil, children)
  }

  def vw(d: Dox): DoxVW = {
    success(writer(Nil, d))
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

  def create(name: String, attrs: Seq[(String, String)], body: Seq[Dox]): Dox =
    create(name, VectorMap(attrs), body)

  def create(name: String, attrs: VectorMap[String, String], body: Seq[Dox]): Dox =
    tags.toStream.flatMap(_.applyOption(name, attrs, body)).headOption.
      getOrElse {
        RAISE.notImplementedYetDefect(s"$name")
      }

  def text(p: String): Text = {
    require (p != null, "Text should not be null.")
    Text(p)
  }

  def list(ps: Seq[String]): List[Inline] = ps.map(text).toList

  def list(p: String, ps: String*): List[Inline] = list(p +: ps)

  def vector(ps: Seq[String]): Vector[Inline] = ps.map(text).toVector

  def vector(p: String, ps: String*): Vector[Inline] = vector(p +: ps)

  def toText(ps: Seq[Dox]): String = ps.map(_.toText).mkString

  def findTable(p: Dox): Option[Table] = p match {
    case m: Table => Some(m)
    case _ => p.elements.toStream.flatMap(findTable).headOption
  }

  def addContent(p: Dox, c: Dox): Dox = p match {
    case m: Fragment => m.append(c)
    case m: Paragraph => c match {
      case mm: Inline => m.append(mm)
      case mm => Fragment(m, mm)
    }
    case m => Fragment(m, c)
  }
}

case class Document(
  head: Head,
  body: Body,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Dox {
  override val elements = List(head, body)
  override def showTerm = "html"
  override def showOpenText = "<!DOCTYPE html><html>"
  override def showCloseText = "</html>"

  override def copyV(cs: List[Dox]) = {
    def s(h: Head, b: Body): ValidationNel[String, Dox] = {
      Success(copy(h, b))
    }

    cs.length match {
      case 0 => s(head, body)
      case 1 => cs.head match {
        case h: Head => s(h, body)
        case b: Body => s(head, b)
        case d => _copy_v(cs)
      }
      case 2 =>
        (cs(0), cs(1)) match {
          case (h: Head, b: Body) => s(h, b)
          case _ => _copy_v(cs)
        }
        // val h: ValidationNel[String, Head] = cs(0) match {
        //   case h: Head => Success(h)
        //   case d => to_failure(cs)
        // }
        // val b: ValidationNel[String, Body] = cs(1) match {
        //   case b: Body => Success(b)
        //   case d => to_failure(cs)
        // }
        // (h |@| b) { case (h, b) => copy(h, b) }
      case _ => _copy_v(cs)
    }
  }

  private def _copy_v(cs: List[Dox]): ValidationNel[String, Dox] = for {
    x <- body.copyV(cs)
  } yield copy(head, x)
}
object Document extends DoxFactory {
  val label = "document"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Document =
    RAISE.unsupportedOperationFault

  // def unapply(p: (String, VectorMap[String, String], Seq[Dox])): Option[Document] =
  //   if (p._1 == name)
  //     Some(apply(p._1, p._2, p._3))
  //   else
  //     None
}

case class Head(
  title: InlineContents = Nil,
  author: InlineContents = Nil,
  date: InlineContents = Nil,
  css: Option[String] = None,
  csslink: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Dox {
  override def isEmpty = (
    title.isEmpty && author.isEmpty && date.isEmpty && css.isEmpty && csslink.isEmpty &&
      attributes.isEmpty && location.isEmpty
  )

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
    css foreach { x =>
      buf.append("<style type=\"text/css\"><!--\n")
      buf.append(x)
      buf.append("\n--></style>")
    }
    csslink foreach { x =>
      buf.append("<link rel=\"stylesheet\" type=\"text/css\" href=\"")
      buf.append(x)
      buf.append("\">")
    }
  }

  override def isOpenClose = title.isEmpty && author.isEmpty && date.isEmpty

  def toOption: Option[Head] =
    if (isEmpty)
      None
    else
      Some(this)

  def merge(p: Head): Head = Head(
    title ++ p.title,
    author ++ p.author,
    date ++ p.date,
    css |+| p.css,
    csslink |+| p.csslink,
    attributes ++ p.attributes,
    location orElse p.location
  )
}

object Head extends DoxFactory {
  val label = "head"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Head =
    RAISE.unsupportedOperationFault

  def builder() = new Builder

  class Builder {
    var title: InlineContents = Nil
    var author: InlineContents = Nil
    var date: InlineContents = Nil

    def build() = new Head(title, author, date)
  }
}

case class Body(
  contents: List[Dox],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Dox {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs, location = get_location(location, contents)))
  }
}

object Body extends DoxFactory {
  val label = "body"

  val empty = Body(Nil)

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Body =
    RAISE.unsupportedOperationFault

  def apply(element: Dox) = new Body(List(element))
}

case class Section(
  title: List[Inline],
  contents: List[Dox],
  level: Int = 1,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Dox {
  lazy val titleName = Dox.toText(title)
  lazy val keyForModel: String = titleName.trim.toLowerCase
  lazy val nameForModel: String = titleName.trim
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
    Success(copy(title, cs, level, location = get_location(location, cs))) // XXX level
  }

  def tableList: List[Table] = contents.collect {
    case m: Table => m
  }
}
object Section {
  def apply(title: String, p: Dox, ps: Dox*): Section =
    Section(List(Dox.text(title)), p +: ps.toList)

  def apply(title: String, ps: Seq[Dox]): Section =
    Section(List(Dox.text(title)), ps.toList)
}

case class Div(
  contents: List[Dox] = Nil,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override def showTerm = "div"
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs, location = get_location(location, cs)))
  }
}

object Div extends Div(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "div"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Div =
    Div(body.toList)

  def apply(d: Dox) = new Div(List(d))
}

case class Paragraph(
  contents: List[Dox],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents
  override def showTerm = "p"

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs, location = get_location(location, cs)))
  }

  override protected def to_Plain_Text(buf: StringBuilder) {
    to_Text(buf)
    buf.append("\n")
  }

  def append(p: Dox): Paragraph = copy(contents = contents :+ p)
}

case class Text(
  contents: String,
  location: Option[ParseLocation] = None
) extends Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty
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

  override def getHtmlTag = None

  override def copyV(cs: List[Dox]) = Success(this)

  def append(p: String): Text = copy(contents = contents ++ p)
}

case class Bold(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "b"

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(contents = x, location = get_location(location, cs)))
  }
}

object Bold extends Bold(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "b"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Bold =
    Bold(body.toList)

  def apply(element: Inline) = new Bold(List(element))
}

// 2011-12-26
case class Italic(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "i"

  override def to_Data_Prologue(buf: StringBuilder) {
    buf.append("/")
  }

  override def to_Data_Epilogue(buf: StringBuilder) {
    buf.append("/")
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(contents = x, location = get_location(location, cs)))
  }
}

object Italic extends Italic(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "i"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Italic =
    Italic(body.toList)

  def apply(element: Inline) = new Italic(List(element))
}

case class Underline(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "u"

  override def to_Data_Prologue(buf: StringBuilder) {
    buf.append("_")
  }

  override def to_Data_Epilogue(buf: StringBuilder) {
    buf.append("_")
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(x, location = get_location(location, cs)))
  }
}

object Underline extends Underline(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "u"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Underline =
    Underline(body.toList)

  def apply(element: Inline) = new Underline(List(element))
}

case class Code(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object Code extends Code(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "code"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Code =
    Code(body.toList)

  def apply(element: Inline) = new Code(List(element))
}

case class Pre(
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = List(Text(contents))
  override def showParams = attributes.list

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs) >| copy(contents, attributes, get_location(location, cs))
  }
}
object Pre extends DoxFactory {
  val label = "pre"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Pre =
    apply(to_text(body), attrs)
}

case class Ul(
  contents: List[Li],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_li(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object Ul extends Ul(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "ul"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Ul =
    Ul(ensure_li(body))

  val empty = Ul(Nil)

  def apply(element: Li) = new Ul(List(element))
  def apply(lis: Seq[Li]) = new Ul(lis.toList)
}

case class Ol(
  contents: List[Li],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_li(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object Ol extends Ol(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "ol"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Ol =
    Ol(ensure_li(body))

  val empty = Ol(Nil)
}

case class Li(
  contents: List[ListContent],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Dox {
  override val elements = contents

  def :+(elem: ListContent): Li = {
    Li(contents :+ elem)
  }

  override def copyV(cs: List[Dox]) = {
    to_list_content(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object Li extends DoxFactory {
  val label = "li"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Li =
    Li(ensure_list_content(body))

  val empty = Li(Nil)
  def apply(text: String) = new Li(List(Text(text)))
  def apply(element: ListContent) = new Li(List(element))
}

// 2011-12-30
case class Del(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object Del extends Del(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "del"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Del =
    Del(ensure_inline(body))

  def apply(element: Inline) = new Del(List(element))
}

case class Hyperlink(
  contents: List[Inline],
  href: URI,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "a"
  override def showParams = List("href" -> href.toASCIIString())

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, href, location = get_location(location, cs)))
  }
}
object Hyperlink extends DoxFactory {
  val label = "a"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Hyperlink =
    Hyperlink(ensure_inline(body), attrs.applyIgnoreCase("href"))

  def apply(c: Seq[Inline], href: String): Hyperlink =
    Hyperlink(c.toList, new URI(href))

  def apply(c: Seq[Inline], href: String, location: Option[ParseLocation]): Hyperlink =
    Hyperlink(c.toList, new URI(href), VectorMap.empty, location)
}

case class ReferenceImg(
  src: URI,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Img {
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}
object ReferenceImg extends DoxFactory {
  val label = "img"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): ReferenceImg =
    ReferenceImg(attrs.applyIgnoreCase("src"))

  def apply(p: String): ReferenceImg = ReferenceImg(new URI(p))
}

trait TableBlock extends Block {
  val caption: Option[Caption]
  val label: Option[String]
}

case class Table(
  head: Option[THead],
  body: TBody,
  foot: Option[TFoot],
  caption: Option[Caption],
  label: Option[String],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TableBlock {
  def getCaptionName: Option[String] = caption.map(_.toText)
  override val elements = List(caption, head, body.some, foot).flatten
  override def showParams = label.toList.map(x => ("id", x))

  override def copyV(cs: List[Dox]) =
    if (cs.isEmpty) {
      Success(this)
    } else {
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
        Success(copy(h, b.get, f, c, label, location = get_location(location, cs)))
      }
    }

  def width: Int = {
    List(head, body.some, foot).flatten.map(_.width).max
  }

  def height: Int = {
    List(head, body.some, foot).flatten.map(_.height).sum
  }

  def getKey: Option[String] = caption.map(_.toText.toLowerCase)

  override def isEmpty: Boolean = body.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def withCaption(p: String): Table = {
    val c = Caption(p)
    copy(caption = Some(c))
  }

  def toOption: Option[Table] = if (body.isEmpty) None else Some(this)

  def toVectorMapStringVector: Vector[VectorMap[String, String]] =
    head.map(_to_vector_map_string_vector(_)).getOrElse(_to_vector_map_string_vector())

  private def _to_vector_map_string_vector(): Vector[VectorMap[String, String]] = {
    val w = body.records.map(_.length).max
    val a = (1 to w).map(_.toString)
    _to_vector_map_string_vector(a)
  }

  private def _to_vector_map_string_vector(h: THead): Vector[VectorMap[String, String]] =
    _to_vector_map_string_vector(h.columns)

  private def _to_vector_map_string_vector(hs: Seq[String]): Vector[VectorMap[String, String]] =
    body.records.toVector.map(_to_vector_map_string(hs, _))

  private def _to_vector_map_string(hs: Seq[String], p: TRecord): VectorMap[String, String] =
    hs.zip(p.fields).toVector.foldMap {
      case (column, field) => VectorMap(column -> field.text)
    }
}
object Table {
  val empty = Table(None, TBody.empty, None, None, None)

  def apply(h: THead, b: TBody): Table = Table(Some(h), b, None, None, None)

  def create(h: Seq[String], data: Seq[IRecord]): Table = {
    val head = THead.create(h)
    val body = TBody.create(head, data)
    Table(head, body)
  }

  class Builder() {
    private var _header: Vector[Inline] = Vector.empty
    private var _data: Vector[Vector[Dox]] = Vector.empty
    private var _caption: Option[Inline] = None
    private var _id: Option[String] = None

    def withCaption(p: String) = {
      _caption = Some(Dox.text(p))
      this
    }

    def withHeader(ps: Seq[Inline]) = {
      _header = ps.toVector
      this
    }

    def withHeaderString(p: Seq[String]) = {
      _header = Dox.vector(p)
      this
    }

    def append(p: String, ps: String*): Builder = appendString(p +: ps)

    def append(p: Dox, ps: Dox*): Builder = append(p +: ps)

    def appendString(ps: Seq[String]): Builder = append(Dox.vector(ps))

    def append(ps: Seq[Dox]): Builder = {
      _data = _data :+ ps.toVector
      this
    }

    def apply(): Table = {
      val h = TR(_header.map(x => TH(x)).toList)
      val b = for (r <- _data.toList) yield {
        TR(for (f <- r.toList) yield {
          TD(f)
        })
      }
      val c = _caption.map(x => Caption(x))
      val thead = THead(List(h))
      val tbody = TBody(b)
      Table(thead.some, tbody, None, c, _id)
    }
  }
  object Builder {
    def captionHeaderString(caption: String, header: Seq[String]): Builder =
      headerString(header).withCaption(caption)

    def headerString(p: String, ps: String*): Builder = headerString(p +: ps)

    def headerString(header: Seq[String]): Builder = {
      new Builder().withHeaderString(header)
    }

    def header(header: Seq[Inline]): Builder = {
      new Builder().withHeader(header)
    }
  }
}

trait TableCompartment extends Block {
  val records: List[TRecord]
  override val elements = records

  def width: Int = records.map(_.length) match {
    case Nil => 0
    case xs => xs.max
  }
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

  def getContent(x: Int, y: Int): Option[List[Dox]] = {
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

case class THead(
  records: List[TRecord],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TableCompartment {
  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy(_, location = get_location(location, cs)))
  }

  def columns: List[String] = records.headOption.map(_.fields.map(_.toText)).orZero
}
object THead {
  def create(names: Seq[String]): THead = {
    val ths = names.map(TH.apply).toList
    val tr = TR(ths)
    THead(List(tr))
  }

  def data(name: String, names: String*): THead = create(name +: names)
}

case class TBody(
  records: List[TRecord],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TableCompartment {
  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy(_, location = get_location(location, cs)))
  }

}
object TBody {
  val empty = TBody(Nil)

  def create(h: THead, data: Seq[IRecord]): TBody = create(h.columns, data)

  def create(keys: Seq[String], data: Seq[IRecord]): TBody = {
    val trs = for (row <- data) yield {
      val tds = for (k <- row.keySymbols) yield {
        row.get(k).map(x => TD(AnyUtils.toString(x))).getOrElse(TD.empty)
      }
      TR(tds)
    }
    TBody(trs.toList)
  }
}

case class TFoot(
  records: List[TRecord],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None) extends TableCompartment {
  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy(_, location = get_location(location, cs)))
  }
}

case class TR(
  fields: List[TField],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TRecord {
  override val elements = fields

  override def copyV(cs: List[Dox]) = {
    to_tfield(cs).map(copy(_, location = get_location(location, cs)))
  }
  def length = fields.length
}

trait TField extends Block {
  val contents: List[Dox]
  lazy val text: String = Dox.toText(contents)
  override val elements = contents
}

case class TD(
  contents: List[Dox],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TField {
  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object TD {
  val empty = TD(Nil)

  def apply(p: Dox): TD = TD(List(p))
  def apply(p: String): TD = TD(List(Dox.text(p)))
}

case class TH(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TField {
  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object TH {
  def apply(p: Inline): TH = TH(List(p))
  def apply(p: String): TH = TH(List(Dox.text(p)))
}

case class TTable(
  uri: String,
  params: List[String],
  caption: Option[Caption] = None,
  label: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
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
    Success(this) // currently do nothing
  }
}

case class Space(
  location: Option[ParseLocation] = None
) extends Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty
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

case class Dl(
  contents: List[(Dt, Dd)],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements: List[Dox] = contents flatMap {
    case (dt, dd) => List(dt, dd)
  }

  override def copyV(cs: List[Dox]) = {
    to_dtdd(cs).map(copy(_))
  }
}
object Dl extends Dl(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dl"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Dl = Dl(ensure_dtdd(body))
}

case class Dt(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def copyV(cs: List[Dox]) = to_inline(cs).map(copy(_))
}
object Dt extends Dt(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dt"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Dt = Dt(ensure_inline(body))

  def apply(p: String): Dt = Dt(to_text(p))

  def unapply(x: XNode): Option[Dt] = {
    RAISE.notImplementedYetDefect
  }
}

case class Dd(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_))
  }
}
object Dd extends Dd(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dd"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Dd =
    Dd(ensure_inline(body))

  def unapply(x: XNode): Option[Dd] = {
    RAISE.notImplementedYetDefect
  }
}

case class Fragment(
  contents: List[Dox],
  location: Option[ParseLocation] = None
) extends Dox with Block with Inline with ListContent {
  def attributes: VectorMap[String, String] = VectorMap.empty
  override val elements = contents
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""

  override protected def show_Contents(buf: StringBuilder) {
    // println(s"Fragment#show_Contents: ${showContentsElements}")
    showContentsElements.foreach(_.toString(buf))
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(contents ::: normalize_fragment(cs)))
  }

  def append(p: String): Fragment = {
    contents.lastOption.map {
      case m: Text => copy(contents = contents.init :+ m.append(p))
      case m => append(Text(p))
    }.getOrElse(append(Text(p)))
  }

  def append(p: Dox): Fragment = copy(contents = contents :+ p)
}
object Fragment extends DoxFactory {
  val label = "fragment"

  val empty = new Fragment(Nil)

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Fragment =
    _create(body)

  def apply(p: Dox, ps: Dox*): Fragment = _create((p +: ps))

  def apply(p: String, ps: String*): Fragment = apply(Text((p +: ps).mkString))

  def apply(ps: Seq[Dox]): Fragment = _create(ps)

  private def _create(ps: Seq[Dox]): Fragment = {
    require (ps.forall(_ != null), "contents should not be null")
    ps.flatMap(_normalize).toList match {
      case Nil => empty
      case xs => new Fragment(xs)
    }
  }

  private def _normalize(p: Dox): List[Dox] = p match {
    case m: Fragment => m.contents.flatMap(_normalize)
    case m => List(m)
  }
}

case class Caption(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_))
  }
}
object Caption {
  def apply(p: Inline): Caption = Caption(List(p))
  def apply(p: String): Caption = Caption(List(Dox.text(p)))
}

// 2011-12-31
case class Figure(
  img: Img,
  caption: Figcaption,
  label: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = List(img, caption)
  override def showParams = List("id" -> label).flatMap(_.sequence)

  override def copyV(cs: List[Dox]) = {
    to_figure(cs).map { case (i, c) =>
      copy(i | img, c | caption, label)
    }
  }
}
object Figure {
  def apply(img: Img, name: String): Figure = Figure(img, Figcaption(name))
}

case class Figcaption(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_))
  }
}
object Figcaption {
  def apply(name: String): Figcaption = Figcaption(List(Text(name)))
}

case class EmptyLine(
  location: Option[ParseLocation] = None
) extends Block {
  def attributes: VectorMap[String, String] = VectorMap.empty

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2011-01-01
case class Newline(
  location: Option[ParseLocation] = None
) extends Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty

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

case class DotImg(
  src: URI,
  contents: String,
  params: List[String] = Nil,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends EmbeddedImg {
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

case class DitaaImg(
  src: URI,
  contents: String,
  params: List[String] = Nil,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends EmbeddedImg {
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2011-01-16
case class Html5(
  name: String,
  attributes: List[(String, String)],
  contents: List[Dox],
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents
  override def showTerm = name
  override def showParams = attributes

  override def copyV(cs: List[Dox]) = {
    Success(copy(name, attributes, cs))
  }
}

// 2011-01-17
case class Program(
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = List(new Text(contents))
  override def showTerm = "pre"
  override def showParams = attributes.list ++ List("class" -> "program")

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs).map(_ => this)
  }
}
object Program {
  def apply(p: String, attr: (String, String), attrs: (String, String)*): Program =
    Program(p, VectorMap(attr +: attrs))

  def apply(p: Seq[String], attr: (String, String), attrs: (String, String)*): Program =
    Program(p.mkString("\n"), VectorMap(attr +: attrs))
}

case class Console(
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = List(new Text(contents))
  override def showTerm = "pre"
  override def showParams = attributes.list ++ List("class" -> "console")

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs).map(_ => this)
  }
}

// 2011-01-18
case class SmartDoc(
  name: String,
  attributes: VectorMap[String, String],
  contents: List[Dox],
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents
  override def showTerm = name
  override def showParams = attributes.list

  override def copyV(cs: List[Dox]) = {
    Success(copy(name, attributes, cs))
  }
}

// 2011-01-20
case class SmCsvImg(
  src: URI,
  contents: String,
  params: List[String] = Nil,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends EmbeddedImg {
  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2012-02-15
object EmptyDox extends Dox with DoxFactory with Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty
  def location: Option[ParseLocation] = None

  val label = "empty"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Dox = EmptyDox

  override def toString(buf: StringBuilder, maxlength: Option[Int] = None) {
  }
}

// 2012-04-24
case class Tt(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, contents)))
  }
}

object Tt extends Tt(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "tt"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Tt =
    Tt(ensure_inline(body))

  def apply(element: Inline) = new Tt(List(element))
}

// 2012-06-05
case class Span(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "span"

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, contents)))
  }
}

object Span extends Span(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "span"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox]): Span =
    Span(ensure_inline(body))

  def apply(element: Inline) = new Span(List(element))
}

// 2012-11-23
case class IncludeDoc(filename: String) extends Block {
  def attributes: VectorMap[String, String] = VectorMap.empty
  def location: Option[ParseLocation] = None

  override def showParams = List(("filename", filename))

  override def copyV(cs: List[Dox]) = {
//    println("IncludeDoc#copyV: " + cs)
    Div(cs).success
  }
}

// 2020-09-21
case class BinaryImg(
  name: String,
  mime: MimeType,
  chunk: ChunkBag,
  location: Option[ParseLocation] = None
) extends Img {
  val src: URI = {
    import org.apache.commons.codec.binary.Base64
    val binary = Base64.encodeBase64String(chunk.toByteArray)
//    val a = s"""data:${name}/${suffix};base64,${binary}"""
    val a = s"""data:${mime.name};base64,${binary}"""
    new URI(a)
  }
}
object BinaryImg {
//   def apply(name: String, mime: MimeType, chunk: ChunkBag): BinaryImg = {
// //    val suffix = MimeType.getSuffix(mime).getOrElse(RAISE.invalidArgumentFault(s"Not image: ${mime}"))
//     BinaryImg(name, mime, chunk)
//   }
}

// 2020-09-22
case class UnresolvedLink(
  contents: List[Inline],
  data: Any,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "a"
  override def showParams = List("href" -> AnyUtils.toString(data))

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, data, attributes, location = get_location(location, cs)))
  }
}
object UnresolvedLink {
  def apply(label: String, data: Any): UnresolvedLink =
    UnresolvedLink(List(Dox.text(label)), data)
}
