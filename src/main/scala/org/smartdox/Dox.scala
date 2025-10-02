package org.smartdox

import scala.language.implicitConversions
import scalaz.{Value => _, _}, Scalaz._, WriterT._, Show._, Validation._
import java.net.URI
import java.net.URL
import java.util.Locale
import scala.xml.{Node => XNode, _}
import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.context.Showable
import org.goldenport.context.Conclusion
import org.goldenport.context.Consequence
import org.goldenport.context.DateTimeContext
import org.goldenport.collection.VectorMap
import org.goldenport.collection.NonEmptyVector
import org.goldenport.parser._
import org.goldenport.io.MimeType
import org.goldenport.bag.ChunkBag
import org.goldenport.extension.IDocument
import org.goldenport.extension.IRecord
import org.goldenport.tree.{Tree => GTree}
import org.goldenport.tree.TreeNode
import org.goldenport.tree.HomoTreeTransformer
import org.goldenport.xsv.{Lxsv, LxsvSequence}
import org.goldenport.hocon.HoconUtils
import org.goldenport.value._
import org.goldenport.values.LocalDateOrDateTime
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NContainer
import org.goldenport.i18n.I18NHangar
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.LocaleUtils
import org.goldenport.xml.XmlUtils
import org.goldenport.util.StringUtils
import org.goldenport.util.AnyUtils
import org.goldenport.util.ListUtils
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.metadata.DoxCacheControl
import org.smartdox.generator.Context
import org.smartdox.converter.DoxTreeVisitor
import org.smartdox.parser.Dox2Parser
import org.smartdox.parser.DoxLinesParser.BlockMacro
import org.smartdox.parser.PureParser
import org.smartdox.util.DoxUtils

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
 *  version Oct. 15, 2018
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
 *  version Jul. 12, 2021
 *  version Jan. 30, 2022
 *  version Sep. 20, 2023
 *  version Jun.  6, 2024
 *  version Jul.  7, 2024
 *  version Sep.  5, 2024
 *  version Oct. 31, 2024
 *  version Nov. 24, 2024
 *  version Dec. 22, 2024
 *  version Jan.  1, 2025
 *  version Mar. 31, 2025
 *  version Apr. 30, 2025
 *  version May.  2, 2025
 *  version Jun. 26, 2025
 *  version Jul. 29, 2025
 *  version Aug. 31, 2025
 *  version Sep. 29, 2025
 * @version Oct.  2, 2025
 * @author  ASAMI, Tomoharu
 */
trait Dox extends IDocument {
  def location: Option[ParseLocation]
  def isEmpty: Boolean = elements.isEmpty
  def isVisialBlock: Boolean
  def elements: List[Dox] = Nil
  def children: List[Dox] = elements // for print

  def attributes: VectorMap[String, String]
  def attributeMap: VectorMap[String, String] = effectiveAttributes
  def attribute(name: String): Option[String] = attributeMap.get(name)

  def getId: Option[Dox.Id] = attributeMap.get("id").map(Dox.Id)
  def getLanguage: Option[Locale] = attributeMap.get("lang").
    map(x => Locale.forLanguageTag(x))

  def showTerm = getClass.getSimpleName().toLowerCase()
  def showParams: List[(String, String)] = Nil

  lazy val effectiveAttributes: VectorMap[String, String] =
    attributes ++ VectorMap(showParams)

  lazy val strategy: Set[String] = effectiveAttributes.get("strategy").
    map(StringUtils.eagerCommaForm).
    getOrElse(Nil).toSet

  def isStable: Boolean = getStable getOrElse false

  def getStable: Option[Boolean] = this match {
    case _: Preserve => Some(true)
    case _ =>
      effectiveAttributes.get("strategy_stable").
        map(_.equalsIgnoreCase("true")) orElse {
          if (strategy.contains("stable"))
            Some(true)
          else if (strategy.contains("unstable"))
            Some(false)
          else
            None
        }
  }

  lazy val showParamsText = effectiveAttributes.map {
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

  def printDox(buffer: StringBuilder): Unit = {
    printOpen(buffer)
    printContents(buffer)
    printClose(buffer)
  }

  def printOpen(buffer: StringBuilder): Unit =
    if (isOpenClose)
      print_Open_Close(buffer)
    else
      print_Open(buffer)

  def printContents(buffer: StringBuilder): Unit =
    print_Contents(buffer)

  def printClose(buffer: StringBuilder): Unit =
    if (!isOpenClose)
      print_Close(buffer)

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

  override final def equals(o: Any) = o match {
    case m: Dox => 
      val r = equals_Whole(m)
//    println(s"Eqauls[${this.getClass.getName}]: $this/$o => $r")
      r
    case _ => false
  }

  // override def equals(o: Any) = o match {
  //   case m: Dox => dox_Equals(m)
  //   case _ => false
  // }

  protected def equals_Whole(o: Dox): Boolean =
    equals_Value(o) && location.equals(o.location)

  protected def equals_Value(o: Dox): Boolean

  def print = toShowString

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

  protected def print_Open_Close(buf: StringBuilder) {
    XmlUtils.printOpenCloseTag(buf, showTerm, attributeMap)
  }

  protected def print_Open(buf: StringBuilder) {
    XmlUtils.printOpenTag(buf, showTerm, attributeMap)
  }

  protected def print_Contents(buf: StringBuilder): Unit =
    for (x <- children) {
      x.printDox(buf)
    }

  protected def print_Close(buf: StringBuilder) {
    show_Close(buf)
  }

  def toText(): String = {
    val buf = new StringBuilder
    to_Text(buf)
    buf.toString
  }

  protected def to_Text(buf: StringBuilder) {
    showContentsElements.foreach(_.to_Text(buf))
  }

  // escape XML literal
  def toPlainText(): String = {
    val buf = new StringBuilder
    to_Plain_Text(buf)
    buf.toString
  }

  protected def to_Plain_Text(buf: StringBuilder) {
    showContentsElements.foreach(_.to_Plain_Text(buf))
  }

  // for Hocon
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

  def traverse(p: DoxTreeVisitor): Unit = toTree.traverse(p)

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
    Dox.normalizeFragment(cs)

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

  def sections: List[Section] = sectionsShallow
  lazy val sectionsShallow = elements collect { case m: Section => m }
  def tables: List[Table] = tablesShallow
  lazy val tablesShallow = elements collect { case m: Table => m }
  def dls: List[Dl] = dlShallow
  lazy val dlShallow = elements collect { case m: Dl => m }
  def uls: List[Ul] = ulShallow
  lazy val ulShallow = elements collect { case m: Ul => m }
  def ols: List[Ol] = olShallow
  lazy val olShallow = elements collect { case m: Ol => m }
  def getTextIfOnly: Option[Text] =
    elements match {
      case Nil => None
      case x :: Nil => x.getTextIfOnly
      case _ => None
    }
  def getStringIfOnlyText: Option[String] = getTextIfOnly.map(_.contents)

  protected final def print_open_tag(buf: StringBuilder, name: String): Unit =
    XmlUtils.printOpenTag(buf, name)

  protected final def print_open_tag(
    buf: StringBuilder,
    name: String,
    attributes: Map[String, String]
  ): Unit = XmlUtils.printOpenTag(buf, name, attributes)

  protected final def print_close_tag(buf: StringBuilder, name: String): Unit =
    XmlUtils.printCloseTag(buf, name)

  def toContent: Dox = Dox.toDox(elements)
}

trait Block extends Dox with ListContent {
  def isVisialBlock = true
}

trait Inline extends Dox with ListContent {
  def isVisialBlock = false
}

trait ListContent extends Dox {  
}

trait Preserve { Dox =>
}

trait Directive extends Dox {
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

  def apply(
    attrs: VectorMap[String, String],
    body: Seq[Dox]
  )(implicit ctx: DateTimeContext): Dox

  def applyOption(
    label: String,
    attrs: VectorMap[String, String],
    body: Seq[Dox]
  )(implicit ctx: DateTimeContext): Option[Dox] =
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

  case class Id(id: String) extends AnyVal

  case class Callouts(slots: Vector[Callouts.Callout] = Vector.empty) {
  }
  object Callouts {
    val empty = Callouts()

    case class Callout(num: Int, content: I18NFragment)

    class Builder() {
      var slots: Vector[Callout] = Vector.empty

      def add(num: Int, desc: String): Builder = {
        val dox = Dox2Parser.parseI18NFragment(desc)
        slots = slots :+ Callout(num, dox)
        this
      }

      def build(): Callouts = Callouts(slots)
    }
  }

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
    Span,
    Dfn,
    Abbr
  )

  def toDox(ps: Seq[Dox]): Dox =
    _activate(ps) match {
      case Nil => Dox.empty
      case x :: Nil => x
      case xs => Fragment(xs)
    }

  private def _activate(ps: Seq[Dox]): List[Dox] = ps.filter {
    case m: Div if m.contents.isEmpty => false
    case m: Span if m.contents.isEmpty => false
    case m: Text if m.contents.isEmpty => false
    case _ => true
  }.toList

  def toDox(p: NonEmptyVector[Dox]): Dox = toDox(p.vector)

  def toDox(p: I18NString): Inline =
    if (p.isSimple)
      Text(p.en)
    else
      I18NFragment.create(p)

  def toDox(p: GTree[Dox]): Dox = untree(p)

  def toInlineContents(ps: Seq[Dox]): List[Inline] =
    _activate(ps).flatMap {
      case m: Fragment => toInlineContents(m)
      case m: Block => toInlineContents(m)
      case m: Inline => List(m)
      case m => RAISE.illegalStateFault(s"No inline: $m")
    }

  def toInlineContents(p: Dox): List[Inline] = p match {
    case m: Fragment => toInlineContents(m.contents)
    case m: Inline => List(m)
    case m: Block => toInlineContents(p.elements)
    case m => RAISE.illegalStateFault(s"No inline: $m")
  }

  def toTree(p: Dox): GTree[Dox] = {
    val root: TreeNode[Dox] = toTreeNode(p)
    GTree.create[Dox](root)
  }

  def toTreeNode(p: Dox): TreeNode[Dox] = {
    val xs = p.elements.map(toTreeNode)
    TreeNode.createContentNode(p, xs)
  }

  def untree(p: GTree[Dox]): Dox = _untree(p.root)

  private def _untree(p: TreeNode[Dox]): Dox = {
    val xs = _untree_children(p)
    p.getContent.fold(Div(xs): Dox) { x =>
      x.copyV(xs) match {
        case Success(s) => s
        case Failure(e) => Conclusion.noReachDefect(s"""Dox#_untree: ${e.list.toList.mkString(";")}""").RAISE
      }
    }
  }

  private def _untree_children(p: TreeNode[Dox]): List[Dox] = {
    val xs = p.children
    val r = xs.map(_untree)
    normalizeFragment(r)
  }

  def transform(p: Dox, tx: HomoTreeTransformer[Dox]): Dox = {
    val a = Dox.toTree(p)
    val b = a.transform(tx)
    Dox.toDox(b)
  }

  def transformDocument(p: Document, tx: HomoTreeTransformer[Dox]): Document = {
    val r = transform(p, tx)
    toDocument(r)
  }

  def transformInlineContents(p: InlineContents, tx: HomoTreeTransformer[Dox]): InlineContents = {
    val r = transform(p, tx)
    toInlineContents(r)
  }

  def toDocument(p: Dox): Document = p match {
    case m: Document => m
    case m: Head => Document.create(m)
    case m: Body => Document.create(m)
    case m => Document.create(m)
  }

  // def untreeV(tree: GTree[Dox]): ValidationNel[String, Dox] = {
  //   val children: List[ValidationNel[String, Dox]] = tree.children.map(untreeV)
  //   // println(s"untreeV in after: ${tree.drawTree}")
  //   // println(s"""untreeV children XXX: ${children}""")
  //   // println(s"""untreeV children: ${children.map(_show).mkString("\n")}""")
  //   // println("children -> errors: " + children + " , " + tree.subForest.toList.map(_.rootLabel))
  //   val errors = children.flatMap {
  //     case Success(d) => Nil
  //     case Failure(e) => e.list
  //   }
  //  // if (errors.nonEmpty) {
  //  //   println("children -> errors: " + children + "," + errors + "/" + tree.subForest.toList)
  //  // }
  //   if (errors.nonEmpty) {
  //     Failure(errors.toNel.get)
  //   } else {
  //    // println("untreeV success = " + tree.drawTree)
  //     val cs = children.collect {
  //         case Success(d) => d
  //     }
  //     // println(s"untreeV success parent = ${tree.rootLabel}")
  //     // println("untreeV success children = " + cs)
  //     val r = tree.rootLabel.copyV(cs)
  //     // println("untreeV success result = " + _show(r))
  //     r
  //   }
  // }

  def tree(dox: Dox): Tree[Dox] = {
    // println(s"Dox#tree in: $dox/${dox.elements}")
    val r = Tree.Node(dox, dox.elements.toStream.map(tree))
    // println(s"Dox#tree out: $dox => ${r.drawTree}")
    r
  }

  def untreeE(tree: Tree[Dox]): Dox = {
    untreeV(tree) match {
      case Success(d) => d
      case Failure(e) => throw new IllegalArgumentException(e.list.toList.mkString(";"))
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
      case Failure(e) => e.list.toList
    }
   // if (errors.nonEmpty) {
   //   println("children -> errors: " + children + "," + errors + "/" + tree.subForest.toList)
   // }
    if (errors.nonEmpty) {
      Failure(errors.toNel.get)
    } else {
     // println("untreeV success = " + tree.drawTree)
      val cs0 = children.collect {
          case Success(d) => d
      }
      // println(s"untreeV success parent = ${tree.rootLabel}")
      // println("untreeV success children = " + cs)
      val cs = normalizeFragment(cs0)
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
      case Failure(e) => e.list.toList
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
      case Failure(e) => e.list.toList
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
    Html5(name, VectorMap.empty, children)
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

  def create(name: String, attrs: Seq[(String, String)], body: Seq[Dox])(implicit ctx: DateTimeContext): Dox =
    create(name, VectorMap(attrs), body)

  def create(name: String, attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dox =
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

  def getText(ps: Seq[Dox]): Option[String] =
    if (ps.isEmpty) {
      None
    } else {
      val s = toText(ps)
      if (s.isEmpty)
        None
      else
        Some(s)
    }

  def toPlainText(ps: Seq[Dox]): String = ps.map(_.toPlainText).mkString

  def makeSection(p: Dox): Section = findSection(p).get

  def findSection(p: Dox): Option[Section] = p match {
    case m: Section => Some(m)
    case _ => p.elements.toStream.flatMap(findSection).headOption
  }

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

  def normalizeFragment(cs: Seq[Dox]): List[Dox] =
    cs.flatMap {
      case m: Fragment => normalizeFragment(m.contents)
      case m => List(m)
    }.toList

  def distillInline(p: Dox): List[Inline] = p match {
    case m: Inline => List(m)
    case m: Paragraph => m.contents.flatMap(x => distillInline(x))
    case m => RAISE.noReachDefect(s"Not inline: $m")
  }

  def distillTitleStringDefault(p: Dox): Option[String] = p match {
    case m: Document => m.head.distillTitleStringDefault
    case m: Head => m.distillTitleStringDefault
    case _ => None
  }

  def distillTitleString(p: Dox)(implicit ctx: I18NContext): Option[String] =
    p match {
      case m: Document => m.head.distillTitleString
      case m: Head => m.distillTitleString
      case _ => None
    }

  def getTitleI18NString(p: Dox): Option[I18NString] =
    p match {
      case m: Document => m.head.getTitleI18NString
      case m: Head => m.getTitleI18NString
      case _ => None
    }

  def trimSingleLine(p: InlineContents): InlineContents =
    toInlineContents(p.map(trimSingleLine))

  def trimSingleLine(p: Inline): Inline = p match {
    case m: I18NFragment => m.trimSingleLine
    case m: Text => Text(DoxUtils.trimSingleLine(m.contents))
    case m => m
  }

  def toDescriptionAsI18NFragment(p: Block): I18NFragment =
    I18NFragment.buildDescription(p)

  def toValueOrValuesAsI18NFragment(p: Block): I18NFragment =
    I18NFragment.buildValueOrValues(p)

  def toValueOrValues(p: Block): Value = {
    val ulopt = p.elements.collectFirst { case m: Ul => m }
    ulopt match {
      case Some(ul) => Ul.toValues(ul)
      case None => toValue(p)
    }
  }

  def toValue(p: Block): Value.Single = Value.Single(p.toText)

  private def toValuesAsInlineContents(p: Block): List[InlineContents] = {
    val ulopt = p.elements.collectFirst { case m: Ul => m }
    ulopt match {
      case Some(ul) => Ul.toValuesAsInlineContents(ul)
      case None => List(toValueAsInlineContents(p))
    }
  }

  private def toValueAsInlineContents(p: Block): InlineContents = {
    val xs = p.elements
    val a = xs.toStream.map {
      case m: Block => toValueAsInlineContents(m)
      case m: Inline => List(m)
    }.headOption
    a getOrElse List(Text(""))
  }

  def getHead(p: Dox): Option[Head] = p match {
    case m: Document => Some(m.head)
    case m: Head => Some(m)
    case _ => None
  }

  def getMetadata(p: Dox): Option[DocumentMetaData] =
    getHead(p).map(_.metadata)

  def compareWithoutDoxCacheControl(expected: Dox, actual: Dox): Boolean = {
    expected match {
      case m: Document => actual match {
        case mm: Document =>
          compareWithoutDoxCacheControl(m.head, mm.head) &&
          compareWithoutDoxCacheControl(m.body, mm.body)
        case _ => false
      }
      case m: Head => actual match {
        case mm: Head => m.equalsWithoutDoxCacheControl(mm)
        case _ => false
      }
      case m => m.equals(actual)
    }
  }

  def printI18NFragment(buf: StringBuilder, name: String, dox: Option[I18NFragment]): Unit =
    dox.foreach(printI18NFragment(buf, name, _))

  def printI18NFragment(buf: StringBuilder, name: String, dox: I18NFragment): Unit =
    printDox(buf, name, dox)

  def printDox(buf: StringBuilder, name: String, dox: Option[Dox]): Unit =
    dox.foreach(printDox(buf, name, _))

  def printDox(buf: StringBuilder, name: String, dox: Dox): Unit = {
    XmlUtils.printOpenTag(buf, name)
    dox.printDox(buf)
    XmlUtils.printCloseTag(buf, name)
  }
}

case class Document(
  head: Head,
  body: Body,
  foot: Option[Foot] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Dox {
  def isVisialBlock: Boolean = false
  override val elements = List(head, body) ::: foot.toList
  override def showTerm = "html"
  override def showOpenText = "<!DOCTYPE html><html>"
  override def showCloseText = "</html>"

  override protected def print_Open(buf: StringBuilder): Unit = {
    print_open_tag(buf, "document")
  }

  override protected def print_Close(buf: StringBuilder): Unit = {
    print_close_tag(buf, "document")
  }

  override def equals_Value(o: Dox) = o match {
    case m: Document => head == m.head && body == m.body && attributes == m.attributes
    case _ => false
  }

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

//  def isMarkCache: Boolean = head.isMarkCache
  def markCache: Document = copy(head = head.markCache)
}
object Document extends DoxFactory {
  val label = "document"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Document =
    RAISE.unsupportedOperationFault

  // def unapply(p: (String, VectorMap[String, String], Seq[Dox])): Option[Document] =
  //   if (p._1 == name)
  //     Some(apply(p._1, p._2, p._3))
  //   else
  //     None

  def create(p: Head): Document = Document(p, Body.empty)

  def create(p: Body): Document = Document(Head.empty, p)

  def create(p: Dox): Document = create(Body(p))
}

case class Head(
  css: Option[String] = None,
  csslink: Option[String] = None,
  cacheControl: Head.CacheControl = Head.CacheControl.empty,
  seo: Head.Seo = Head.Seo.empty,
  metadata: DocumentMetaData = DocumentMetaData.empty,
  attributes: VectorMap[String, String] = VectorMap.empty,
  doxCacheControl: Option[DoxCacheControl] = None,
  location: Option[ParseLocation] = None
) extends Dox {
  def isVisialBlock: Boolean = false
  override def isEmpty = (
    css.isEmpty && csslink.isEmpty &&
      cacheControl.isEmpty && seo.isEmpty &&
      metadata.isEmpty &&
      attributes.isEmpty && location.isEmpty
  )

  override def equals_Value(o: Dox) = o match {
    case m: Head =>
      css == m.css && csslink == m.csslink &&
      cacheControl == m.cacheControl && seo == m.seo &&
      metadata == m.metadata &&
      attributes == m.attributes
    case _ => false
  }

  def equalsWithoutDoxCacheControl(p: Head): Boolean = {
    css.equals(p.css) &&
    csslink.equals(p.csslink) &&
    cacheControl.equals(p.cacheControl) &&
    seo.equals(p.seo) &&
    metadata.equals(p.metadata) &&
    attributes.equals(p.attributes)
  }

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
    showslot("title", titleDefault)
    // showslot("author", author)
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

  override protected def print_Open(buf: StringBuilder): Unit = {
    print_open_tag(buf, "head", attributes)
    metadata.printFlat(buf)
    doxCacheControl.map(_.print(buf))
  }

  override protected def print_Close(buf: StringBuilder): Unit = {
    print_close_tag(buf, "head")
  }

  def title: Option[I18NFragment] = metadata.title
  def titleDefault: InlineContents = metadata.getTitleInclineContentsDefault getOrElse Nil
  def date: InlineContents = metadata.publishedAt.toList.map(x => Text(x.print))
  def author: InlineContents = seo.author getOrElse Nil
  def description: Option[I18NFragment] = metadata.description

  def getTitleI18NString: Option[I18NString] = title.map(_.toI18NString)

  def distillTitleStringDefault: Option[String] = titleDefault match {
    case Nil => None
    case xs => Some(Dox.toText(xs))
  }

  def distillTitleString(implicit ctx: I18NContext): Option[String] =
    title.map(_.distillString(ctx.locale))

  override def isOpenClose = titleDefault.isEmpty && author.isEmpty && date.isEmpty

  def toOption: Option[Head] =
    if (isEmpty)
      None
    else
      Some(this)

  def withDocumentMetaData(p: DocumentMetaData) = copy(metadata = p)

  def withTitle(ps: InlineContents) = copy(metadata = metadata.withTitle(ps))

  def withSummary(ps: InlineContents) = copy(metadata = metadata.withSummary(ps))

  private def withSummary(p: String) = copy(metadata = metadata.withSummary(p))

  def merge(p: Head): Head = Head(
    css |+| p.css,
    csslink |+| p.csslink,
    cacheControl + p.cacheControl,
    seo + p.seo,
    metadata + p.metadata, // properties.withFallback(p.properties),
    attributes ++ p.attributes,
    doxCacheControl,
    location orElse p.location
  )

  def merge(p: DocumentMetaData): Head = copy(metadata = metadata + p)

//  def isMarkCache: Boolean = doxCacheControl.fold(false)(_.isMarked)

  def markCache: Head = {
    val dcc = doxCacheControl.map(_.mark) getOrElse DoxCacheControl.marked()
    copy(doxCacheControl = Some(dcc))
  }
}

object Head extends DoxFactory {
  val empty = Head()

  case class CacheControl(
  ) {
    def isEmpty = true

    def +(p: CacheControl) = this // TODO
  }
  object CacheControl {
    val empty = CacheControl()
  }

  case class Seo(
    basic: BasicSeo = BasicSeo.empty,
    openGraphProtocol: OpenGraphProtocol = OpenGraphProtocol.empty,
    twitterCard: TwitterCard = TwitterCard.empty
  ) {
    def isEmpty = basic.isEmpty && openGraphProtocol.isEmpty && twitterCard.isEmpty

    def toOption: Option[Seo] = if (isEmpty) None else Some(this)

    def +(rhs: Seo) = copy(
      basic = basic + rhs.basic,
      openGraphProtocol = openGraphProtocol + rhs.openGraphProtocol,
      twitterCard = twitterCard + rhs.twitterCard
    )

    def author = basic.author
  }
  object Seo {
    val empty = Seo()

    def author(p: InlineContents) = empty.copy(basic = BasicSeo.author(p))

    def parseFlat(elem: XNode): Consequence[Option[Seo]] = {
      for {
        bseo <- BasicSeo.parseFlat(elem)
      } yield {
        Seo(bseo).toOption
      }
    }
  }

  case class BasicSeo(
    description: Option[String] = None,
    robots: Option[String] = None,
    canonical: Option[URL] = None,
    author: Option[InlineContents] = None,
    keywords: Option[String] = None
  ) {
    def isEmpty = true // TODO

    def +(p: BasicSeo) = this // TODO
  }
  object BasicSeo {
    val empty = BasicSeo()

    def author(p: InlineContents) = if (p.isEmpty) empty else empty.copy(author = Some(p))

    def parseFlat(elem: XNode): Consequence[BasicSeo] = {
      for {
        authorx <- XmlUtils.getElementC(elem, "author")
        author <- authorx.traverse(PureParser.buildChildrenInlinesC)
      } yield {
        BasicSeo(author = author)
      }
    }
  }

  case class OpenGraphProtocol(
    title: Option[String] = None,
    description: Option[String] = None,
    image: Option[URL] = None,
    url: Option[URL] = None,
    pageType: Option[String] = None
  ) {
    def isEmpty = true // TODO

    def +(p: OpenGraphProtocol) = this // TODO

    def enablePageType = copy(pageType = Some("website"))
  }
  object OpenGraphProtocol {
    val empty = OpenGraphProtocol()
  }

  case class TwitterCard(
    card: Option[String] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    image: Option[URL] = None,
    site: Option[String] = None
  ) {
    def isEmpty = true // TODO

    def +(p: TwitterCard) = this // TODO
  }
  object TwitterCard {
    val empty = TwitterCard()
  }

  val label = "head"

  def apply(
    attrs: VectorMap[String, String],
    body: Seq[Dox]
  )(implicit ctx: DateTimeContext): Head = {
    case class Z(properties: Hocon = HoconUtils.empty) {
      def r = Head(metadata = DocumentMetaData.create(properties))

      def +(rhs: Dox) = rhs match {
        case m: Text =>
          HoconUtils.parseConfig(m.contents).fold(
            f => copy(properties = HoconUtils.addProperty(
              properties,
              "error",
              f.message
            )),
            s => copy(properties = properties.withFallback(s))
          )
        case m => this // TODO
      }
    }
    body.foldLeft(Z())(_+_).r
  }

  def apply(
    title: Inline,
    subtitle: String, // TODO
    css: Option[String],
    csslink: Option[String]
  ): Head = {
    val metadata = DocumentMetaData.create(title)
    new Head(metadata = metadata, css = css, csslink = csslink)
  }

  def create(
    md: Option[DocumentMetaData],
    seo: Option[Seo],
    dcc: Option[DoxCacheControl]
  ): Head = {
    new Head(
      seo = seo getOrElse Seo.empty,
      metadata = md getOrElse DocumentMetaData.empty,
      doxCacheControl = dcc
    )
  }

  def create(
    title: InlineContents,
    author: InlineContents,
    date: InlineContents
  )(implicit dctx: DateTimeContext): Head = {
    val metadata = DocumentMetaData.create(title, date)
    new Head(seo = Seo.author(author), metadata = metadata)
  }

  def title(title: Inline): Head = Head(metadata = DocumentMetaData.create(title))

  def builder(implicit dctx: DateTimeContext) = new Builder()

  class Builder(implicit dctx: DateTimeContext)  {
    var title: InlineContents = Nil
    var author: InlineContents = Nil
    var date: InlineContents = Nil

    def build() = create(title, author, date)
  }
}

case class Body(
  contents: List[Dox],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Dox {
  def isVisialBlock: Boolean = false
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Body => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs, location = get_location(location, contents)))
  }
}

object Body extends DoxFactory {
  val label = "body"

  val empty = Body(Nil)

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Body =
    RAISE.unsupportedOperationFault

  def apply(node: Dox) = node match {
    case m: Fragment => new Body(m.contents)
    case m => new Body(List(m))
  }
}

// 2025-09-06
case class Foot(
  contents: List[Dox] = Nil,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override def showTerm = "foot"
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Foot => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs, location = get_location(location, cs)))
  }
}

case class Section(
  title: List[Inline],
  contents: List[Dox],
  level: Int = 1,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  lazy val titleName = Dox.toText(title)
  lazy val keyForModel: String = titleName.trim.toLowerCase
  lazy val nameForModel: String = titleName.trim

  def titleI18NFragment: I18NFragment = I18NFragment.create(title)

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

  override def print_Open(buf: StringBuilder): Unit = {
    val showh = "title"
    buf.append(showOpenText)
    buf.append("<")
    buf.append(showh)
    buf.append(">")
    title.foreach(_.printDox(buf))
    buf.append("</")
    buf.append(showh)
    buf.append(">")
  }

  override def equals_Value(o: Dox) = o match {
    case m: Section => title == m.title && contents == m.contents && level == m.level && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(title, cs, level, location = get_location(location, cs))) // XXX level
  }

  def withTitle(p: InlineContents): Section = copy(title = p)

  def tableList: List[Table] = contents.collect {
    case m: Table => m
  }

  def distillDescription: Description = {
    val name = titleName
    val xs = contents.takeWhile(_.isInstanceOf[Section] == false)
    val dox = xs match {
      case Nil => EmptyDox
      case x :: Nil => x
      case xs => Fragment(xs)
    }
    Description.name(name, dox)
  }
}
object Section {
  def apply(title: String, p: Dox, ps: Dox*): Section =
    Section(List(Dox.text(title)), p +: ps.toList)

  def apply(title: String, ps: Seq[Dox]): Section =
    Section(List(Dox.text(title)), ps.toList)

  def apply(title: I18NFragment, ps: Seq[Dox]): Section =
    Section(List(title), ps.toList)

  def create(title: I18NString, p: Option[Dox]): Section =
    Section(List(I18NFragment.create(title)), p.toList)

  def create(title: I18NString, p: I18NFragment): Section =
    Section(List(I18NFragment.create(title)), List(p))

  // def toKeyValues(p: Section): (String, List[InlineContents]) =
  //   (p.keyForModel, Dox.toValuesAsInlineContents(p))

  def toKeyValueOrValues(p: Section): (String, Value) =
    (p.keyForModel, Value.buildValueOrValuesI18N(p))

  def toKeyDescription(p: Section): (String, I18NFragment) =
    (p.keyForModel, Dox.toDescriptionAsI18NFragment(p))
}

case class Div(
  contents: List[Dox] = Nil,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override def showTerm = "div"
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Div => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(cs, location = get_location(location, cs)))
  }
}

object Div extends Div(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "div"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: org.goldenport.context.DateTimeContext): Div =
    Div(body.toList, attrs)

  def apply(d: Dox) = new Div(List(d))

  def create(locale: Locale, body: Dox): Div =
    Div(List(body), VectorMap("lang" -> locale.toString))

  def build(elem: XNode): Div = {
    val cs = PureParser.buildChildren(elem)
    val attrs = PureParser.getAttributes(elem)
    Div(cs, attrs)
  }
}

case class Paragraph(
  contents: List[Dox],
  attributes: VectorMap[String, String] = VectorMap.empty,
  logicalLine: Option[LogicalLine] = None,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents
  override def showTerm = "p"

  override protected def equals_Value(o: Dox) = o match {
    case m: Paragraph =>
      val r = contents.equals(m.contents)
      // println(s"E: ${r}")
      r
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    val r = copy(cs, location = get_location(location, cs))
    // println(s"Paragraph#copyV: $cs => $r")
    Success(r)
  }

  override protected def to_Plain_Text(buf: StringBuilder) {
    to_Text(buf)
    buf.append("\n")
  }

  override protected def to_Data(buf: StringBuilder) {
    logicalLine match {
      case Some(s) => for (x <- s.physicalLines) {
        buf.append(x)
        buf.append("\n")
      }
      case None => to_Plain_Text(buf)
    }
  }

  override def getStringIfOnlyText: Option[String] = getTextIfOnly.map(_.contents)

  def append(p: Dox): Paragraph = copy(contents = contents :+ p)
}
object Paragraph {
  def apply(p: List[Dox], ll: LogicalLine): Paragraph =
    Paragraph(p, logicalLine = Some(ll))

  def apply(p: Dox, ll: LogicalLine): Paragraph = apply(List(p), ll)

  def text(p: String): Paragraph = Paragraph(List(Text(p)))

  def build(elem: XNode): Paragraph = {
    val cs = PureParser.buildChildren(elem)
    val attrs = PureParser.getAttributes(elem)
    Paragraph(cs, attrs)
  }
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
  override def to_Plain_Text(buf: StringBuilder) {
    buf.append(XmlUtils.escape(contents))
  }
  override def to_Data(buf: StringBuilder) {
    buf.append(contents)
  }

  override def print_Open(buf: StringBuilder) = {}

  override def print_Contents(buf: StringBuilder): Unit = {
    buf.append(contents)
  }

  override def print_Close(buf: StringBuilder) = {}

  override def getHtmlTag = None

  override protected def equals_Value(o: Dox) = o match {
    case m: Text =>
      val r = contents.equals(m.contents)
      // println(s"Text: ${r}")
      r
    case _ => false
  }

  override def copyV(cs: List[Dox]) = Success(this)

  override def getTextIfOnly = Some(this)

  def append(p: String): Text = copy(contents = contents ++ p)

  def xmlString: String = XmlUtils.escape(contents)

  def isBlank: Boolean = Strings.blankp(contents)
}

case class Bold(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "b"

  override def equals_Value(o: Dox) = o match {
    case m: Bold => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(contents = x, location = get_location(location, cs)))
  }
}

object Bold extends Bold(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "b"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Bold =
    Bold(ensure_inline(body), attrs)

  def apply(element: Inline) = new Bold(List(element))

  def build(elem: XNode): Bold = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Bold(cs, attrs)
  }
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

  override def equals_Value(o: Dox) = o match {
    case m: Italic => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(contents = x, location = get_location(location, cs)))
  }
}

object Italic extends Italic(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "i"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Italic =
    Italic(ensure_inline(body), attrs)

  def apply(element: Inline) = new Italic(List(element))

  def build(elem: XNode): Italic = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Italic(cs, attrs)
  }
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

  override def equals_Value(o: Dox) = o match {
    case m: Underline => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(x => copy(x, location = get_location(location, cs)))
  }
}

object Underline extends Underline(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "u"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Underline =
    Underline(body.toList)

  def apply(element: Inline) = new Underline(List(element))

  def build(elem: XNode): Underline = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Underline(cs, attrs)
  }
}

case class Code(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  kind: Option[Code.Kind] = None,
  location: Option[ParseLocation] = None
) extends Inline with Preserve {
  override val elements = contents

  override def showParams = ListUtils.buildTupleList("class" -> kind.map(_.name))

  override def equals_Value(o: Dox) = o match {
    case m: Code => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object Code extends Code(Nil, VectorMap.empty, None, None) with DoxFactory {
  val label = "code"

  sealed trait Kind extends NamedValueInstance {
  }
  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(Console)

    case object Console extends Kind {
      val name = "console"
    }
  }

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Code =
    Code(body.toList)

  def apply(content: Inline) = new Code(List(content))

  def apply(content: Inline, kind: Option[Kind]) = new Code(List(content), kind = kind)

  def build(elem: XNode): Code = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Code(cs, attrs)
  }
}

case class Pre(
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline with Preserve {
  override val elements = List(Text(contents))
//  override def showParams = attributes.list

  override def equals_Value(o: Dox) = o match {
    case m: Pre => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs) >| copy(contents, attributes, get_location(location, cs))
  }
}
object Pre extends DoxFactory {
  val label = "pre"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Pre =
    apply(to_text(body), attrs)
}

case class Ul(
  contents: List[Li],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Ul => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_li(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object Ul extends Ul(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "ul"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Ul =
    Ul(ensure_li(body))

  val empty = Ul(Nil)

  def apply(element: Li) = new Ul(List(element))
  def apply(lis: Seq[Li]) = new Ul(lis.toList)

  def toValuesAsInlineContents(ul: Ul): List[InlineContents] =
    ul.contents.map(Dox.toInlineContents)

  def toValues(ul: Ul): Value.Multiple =
    Value.Multiple(ul.contents.map(_.toText).toVector)
}

case class Ol(
  contents: List[Li],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Ol => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_li(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object Ol extends Ol(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "ol"

  val empty = Ol(Nil)

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Ol =
    Ol(ensure_li(body))
  def apply(lis: Seq[Li]) = new Ol(lis.toList)
}

case class Li(
  contents: List[ListContent],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  def :+(elem: ListContent): Li = {
    Li(contents :+ elem)
  }

  override def equals_Value(o: Dox) = o match {
    case m: Li => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_list_content(cs).map(copy(_, location = get_location(location, cs)))
  }
}

object Li extends DoxFactory {
  val label = "li"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Li =
    Li(ensure_list_content(body))

  val empty = Li(Nil)
  def apply(text: String) = new Li(List(Text(text)))
  def apply(element: ListContent) = new Li(List(element))
  def apply(ps: Seq[ListContent]) = new Li(ps.toList)
}

// 2011-12-30
case class Del(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Del => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, cs)))
  }
}
object Del extends Del(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "del"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Del =
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

  override def equals_Value(o: Dox) = o match {
    case m: Hyperlink => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, href, location = get_location(location, cs)))
  }

  def getTitle: Option[String] = attributes.get("title")
  def getHtmlClass: Option[String] = attributes.get("class")
}
object Hyperlink extends DoxFactory {
  val label = "a"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Hyperlink =
    Hyperlink(ensure_inline(body), attrs.applyIgnoreCase("href"))

  def apply(c: Seq[Inline], href: String): Hyperlink =
    Hyperlink(c.toList, new URI(href))

  def apply(c: Seq[Inline], href: String, location: Option[ParseLocation]): Hyperlink =
    Hyperlink(c.toList, new URI(href), VectorMap.empty, location)

  def create(body: String, href: URI, alt: String): Hyperlink =
    Hyperlink(List(Text(body)), href, VectorMap("title" -> alt))

  def create(url: String): Hyperlink =
    Hyperlink(List(Text(url)), new URI(url))

  def createCategory(body: Inline, href: URI): Hyperlink =
    Hyperlink(List(body), href, VectorMap("class" -> "category"))

  def createArticle(body: I18NString, href: URI): Hyperlink =
    Hyperlink(List(Dox.toDox(body)), href, VectorMap("class" -> "article"))

  def createGlossary(body: String, href: URI, title: String): Hyperlink =
    Hyperlink(List(Text(body)), href, VectorMap("title" -> title, "class" -> "glossary"))

  def createGlossary(body: List[Inline], href: URI, title: String): Hyperlink =
    Hyperlink(body, href, VectorMap("title" -> title, "class" -> "glossary"))

  def createGlossary(body: String, href: URI): Hyperlink =
    Hyperlink(List(Text(body)), href, VectorMap("class" -> "glossary"))

  def createGlossary(body: List[Inline], href: URI): Hyperlink =
    Hyperlink(body, href, VectorMap("class" -> "glossary"))
}

case class ReferenceImg(
  src: URI,
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Img {
  // def attributesUnified = {
  //   val a = VectorMap("src" -> src.toString)
  //   val b = VectorMap.create("alt" -> alt)
  //   a ++ b ++ attributes
  // }
  override def showParams = ListUtils.buildTupleList(
    List("src" -> src.toString),
    List("atl" -> alt)
  )

  override def equals_Value(o: Dox) = o match {
    case m: ReferenceImg => src == m.src && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }

  def withSrc(p: String): ReferenceImg = copy(src = new URI(p))
}
object ReferenceImg extends DoxFactory {
  val label = "img"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): ReferenceImg =
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
  side: Option[TSide],
  colgroup: Option[Colgroup],
  caption: Option[Caption],
  label: Option[String],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TableBlock {
  def getCaptionName: Option[String] = caption.map(_.toText)
  override val elements = List(caption, colgroup, head, side, body.some, foot).flatten
  override def showParams = label.toList.map(x => ("id", x))

  override def equals_Value(o: Dox) = o match {
    case m: Table => head == m.head && body == m.body && foot == m.foot && side == m.side && colgroup == m.colgroup && caption == m.caption && label == m.label && attributes == m.attributes
    case _ => false
  }

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
      val (s, cs5) = cs4 match {
        case (s: TSide) :: xs => (Some(s), xs)
        case _ => (None, cs4)
      }
      val (cg, cs6) = cs5 match {
        case (cg: Colgroup) :: xs => (Some(cg), xs)
        case _ => (None, cs5)
      }
      if (b.isEmpty || cs6.nonEmpty) {
        to_failure(cs)
      } else {
        Success(copy(h, b.get, f, s, cg, c, label, location = get_location(location, cs)))
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
  val empty = Table(None, TBody.empty, None, None, None, None, None)

  sealed trait Align extends Showable
  object Align {
    case object Left extends Align {
      def print = "left"
    }
    case object Center extends Align {
      def print = "center"
    }
    case object Right extends Align {
      def print = "right"
    }
  }

  def apply(h: THead, b: TBody): Table = Table(Some(h), b, None, None, None, None, None)

  def create(h: Seq[String], data: Seq[IRecord]): Table = {
    val head = THead.create(h)
    val body = TBody.create(head, data)
    Table(head, body)
  }

  // def create(h: Seq[String], data: VectorMap[String, String]): Table = {
  //   val head = THead.create(h)
  //   val body = TBody.create(head, data)
  //   Table(head, body)
  // }

  def create(p: LxsvSequence): Table = {
    case class Z(
      header: Vector[String] = Vector.empty,
      data: Vector[VectorMap[String, String]] = Vector.empty
    ) {
      def r = create(header, data.map(IRecord.create))

      def +(rhs: Lxsv) = {
        val h = rhs.keyNames.foldLeft(header)((z, x) =>
          if (z.contains(x))
            z
          else
            z :+ x
        )
        val d = data :+ rhs.toStringStringVectorMap
        Z(h, d)
      }
    }
    p.vector.foldLeft(Z())(_+_).r
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

    def withHeaderString(p: Seq[String]): Builder = {
      _header = Dox.vector(p)
      this
    }

    def withHeaderString(p: String, ps: String*): Builder = withHeaderString(p +: ps)

    def append(p: String, ps: String*): Builder = appendString(p +: ps)

    def append(p: Dox, ps: Dox*): Builder = append(p +: ps)

    def append(p: I18NString, ps: I18NString*): Builder =
      append((p +: ps).map(I18NFragment.create))

    def appendString(ps: Seq[String]): Builder = append(Dox.vector(ps))

    def append(ps: Seq[Dox]): Builder = {
      _data = _data :+ ps.toVector
      this
    }

    def apply(): Table = {
      val thead: Option[THead] =
        if (_header.isEmpty) {
          None
        } else {
          val h = TR(_header.map(x => TH(x)).toList)
          Some(THead(List(h)))
        }
      val b = for (r <- _data.toList) yield {
        TR(for (f <- r.toList) yield {
          TD(f)
        })
      }
      val c = _caption.map(x => Caption(x))
      val tbody = TBody(b)
      Table(thead, tbody, None, None, None, c, _id)
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
  override def equals_Value(o: Dox) = o match {
    case m: THead => records == m.records && attributes == m.attributes
    case _ => false
  }

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
  override def equals_Value(o: Dox) = o match {
    case m: TBody => records == m.records && attributes == m.attributes
    case _ => false
  }

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

  // def create(keys: Seq[String], data: VectorMap[String, String]): TBody = {
  //   val trs = for (row <- data.vector) yield {
  //     val tds = for (k <- row.keySymbols) yield {
  //       row.get(k).map(x => TD(AnyUtils.toString(x))).getOrElse(TD.empty)
  //     }
  //     TR(tds)
  //   }
  //   TBody(trs.toList)
  // }
}

case class TFoot(
  records: List[TRecord],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None) extends TableCompartment {
  override def equals_Value(o: Dox) = o match {
    case m: TFoot => records == m.records && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_tr(cs).map(copy(_, location = get_location(location, cs)))
  }
}

case class TSide(
  top: Option[TH],
  columns: List[TH],
  bottom: Option[TH],
  location: Option[ParseLocation] = None
) extends Block {
  override val attributes = VectorMap.empty
  override val elements = List()

  override def equals_Value(o: Dox) = o match {
    case m: TSide => top == m.top && columns == m.columns && bottom == m.bottom
    case _ => false
  }
}

case class Colgroup(
  cols: List[Col],
  location: Option[ParseLocation] = None
) extends Block {
  override val attributes = VectorMap.empty
  override val elements = cols

  override def equals_Value(o: Dox) = o match {
    case m: Colgroup => cols == m.cols
    case _ => false
  }
}

case class Col(
  span: Option[Int] = None,
  align: Option[Table.Align] = None,
  location: Option[ParseLocation] = None
) extends Block {
  override val attributes = VectorMap.empty

  override def showParams = ListUtils.buildTupleList(
    "span" -> span.map(AnyUtils.toString),
    "align" -> align.map(_.print)
  )

  override def equals_Value(o: Dox) = o match {
    case m: Col => span == m.span && align == m.align
    case _ => false
  }
}
object Col {
  def apply(p: Table.Align): Col = Col(align = Some(p))
}

case class TR(
  fields: List[TField],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends TRecord {
  override val elements = fields

  override def equals_Value(o: Dox) = o match {
    case m: TR => fields == m.fields && attributes == m.attributes
    case _ => false
  }

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
  override def equals_Value(o: Dox) = o match {
    case m: TD => contents == m.contents && attributes == m.attributes
    case _ => false
  }

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
  override def equals_Value(o: Dox) = o match {
    case m: TH => contents == m.contents && attributes == m.attributes
    case _ => false
  }

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

  override def equals_Value(o: Dox) = o match {
    case m: TTable => uri == m.uri && params == m.params && caption == m.caption && label == m.label && attributes == m.attributes
    case _ => false
  }

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

  override def equals_Value(o: Dox) = o match {
    case m: Space => true
    case _ => false
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

  override protected def equals_Value(o: Dox) = o match {
    case m: Dl =>
      val r = contents == m.contents && attributes == m.attributes
      // println(s"Dl: ${r}")
      r
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_dtdd(cs).map(copy(_))
  }
}
object Dl extends Dl(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dl"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dl = Dl(ensure_dtdd(body))

  def create(p: Seq[(String, String)]): Dl = Dl(p.toList.map {
    case (t, d) => (Dt(t), Dd(d))
  })
}

case class Dt(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override protected def equals_Value(o: Dox) = o match {
    case m: Dt =>
      val r = contents.equals(m.contents) && location == m.location
      // println(s"Dt: ${r}")
      r
    case _ => false
  }

  override def copyV(cs: List[Dox]) = to_inline(cs).map(copy(_))
}
object Dt extends Dt(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dt"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dt = Dt(ensure_inline(body))

  def apply(p: String): Dt = Dt(Dox.text(p))

  def apply(p: Inline): Dt = Dt(List(p))

  def unapply(x: XNode): Option[Dt] = x.label.toLowerCase match {
    case "dt" => Some(PureParser.buildDt(x))
    case _ => None
  }
}

case class Dd(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Dd => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_))
  }
}
object Dd extends Dd(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dd"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dd =
    Dd(ensure_inline(body))

  def unapply(x: XNode): Option[Dd] = x.label.toLowerCase match {
    case "dd" => Some(PureParser.buildDd(x))
    case _ => None
  }
}

case class Fragment(
  contents: List[Dox],
  location: Option[ParseLocation] = None
) extends Dox with Block with Inline with ListContent {
  override def isVisialBlock: Boolean = false
  def attributes: VectorMap[String, String] = VectorMap.empty
  override val elements = contents
  override def isOpenClose = false
  override def showOpenText = ""
  override def showCloseText = ""

  override protected def show_Contents(buf: StringBuilder) {
    // println(s"Fragment#show_Contents: ${showContentsElements}")
    showContentsElements.foreach(_.toString(buf))
  }

  override def equals_Value(o: Dox) = o match {
    case m: Fragment => contents == m.contents
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(normalize_fragment(cs)))
//    Success(copy(contents ::: normalize_fragment(cs)))
  }

  def toInlines: List[Inline] = Dox.toInlineContents(contents)

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

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Fragment =
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

case class I18NFragment(
  contents: I18NContainer[List[Dox]],
  location: Option[ParseLocation] = None
) extends Dox with Block with Inline with ListContent {
  override def isVisialBlock: Boolean = false
  def attributes: VectorMap[String, String] = VectorMap.empty

  override def equals_Value(o: Dox) = o match {
    case m: I18NFragment => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def showTerm = "i18n"

  override def isOpenClose = false

  override def printDox(buf: StringBuilder): Unit = 
    contents.getIfNoLocale match {
      case Some(s) => _print_contents(buf, s)
      case None => super.printDox(buf)
    }

  override def print_Contents(buf: StringBuilder): Unit =
    for ((locale, xs) <- contents.localeVector) {
      buf.append("<")
      buf.append(locale)
      buf.append(">")
      _print_contents(buf, xs)
      buf.append("</")
      buf.append(locale)
      buf.append(">")
    }

  private def _print_contents(buf: StringBuilder, xs: Seq[Dox]): Unit =
    for (x <- xs) {
      x.printDox(buf)
    }

  def isSimple: Boolean = contents.getIfNoLocale.isDefined

  def distill(locale: Option[Locale]): List[Dox] =
    locale.fold(contents.default)(distill)

  def distill(locale: Locale): List[Dox] = contents.apply(locale)

  def distillInline(locale: Locale): List[Inline] =
    distill(locale).flatMap(Dox.distillInline)

  def distillI18NFragment(locale: Option[Locale]): I18NFragment =
    locale.fold(distillI18NFragmentDefault)(distillI18NFragment)

  def distillI18NFragment(locale: Locale): I18NFragment =
    I18NFragment(I18NContainer.make(distill(locale)))

  def distillI18NFragmentDefault: I18NFragment =
    I18NFragment(I18NContainer.make(contents.default))

  def distillString(locale: Locale): String = Dox.toPlainText(distillInline(locale))

  def distillStringDefault: String = Dox.toPlainText(contents.default)

  def distillInlineContentsDefault: InlineContents = contents.default.map {
    case m: Inline => m
  }

  def toI18NString: I18NString = I18NString(
    Dox.toPlainText(contents.c),
    Dox.toPlainText(contents.en),
    Dox.toPlainText(contents.ja),
    contents.map.mapValues(Dox.toPlainText)
  )

  def makeInlines: List[Inline] =
    contents.getIfNoLocale match {
      case Some(s) => Dox.toInlineContents(s)
      case None => _make_inlines
    }

  private def _make_inlines = {
    case class Z(ls: Map[Locale, List[Inline]] = Map.empty) {
      def r = {
        ls.toList map {
          case (l, xs) => Span.create(l, xs)
        }
      }

      def +(rhs: (Locale, List[Dox])) = {
        val v = Dox.toInlineContents(rhs._2)
        copy(ls + (rhs._1 -> v))
      }
    }
    contents.localeVector.foldLeft(Z())(_+_).r
  }

  def trimSingleLine: I18NFragment = {
    val lv = contents.localeVector
    val r = lv.map(_trim_single_line)
    copy(contents = I18NContainer.create(r))
  }

  private def _trim_single_line(p: (Locale, List[Dox])): (Locale, List[Dox]) = {
    val (k, v) = p
    val r = _find_text(v) match {
      case Some(s) => List(Text(DoxUtils.trimSingleLine(s.contents)))
      case None => Nil
    }
    k -> r
  }

  private def _find_text(ps: List[Dox]): Option[Text] = {
    def _go_(x: Dox): Option[Text] = x match {
      case m: Text => if (m.isBlank) None else Some(m)
      case m => x.children.toStream.flatMap(_go_).headOption
    }
    ps.toStream.flatMap(_go_).headOption
  }

  def toVectorMapStringVector: VectorMap[Locale, Vector[String]] = {
    val a: Vector[(Locale, List[Dox])] = contents.localeVector
    val b = a.map {
      case (l, vs) => l -> vs.flatMap(toStringVector).toVector
    }
    VectorMap(b)
  }

  def toVectorMapString: VectorMap[Locale, String] =
    toVectorMapStringVector.mapValues(_.mkString)

  def toStringVector(p: Dox): Vector[String] = p match {
    case m: Value.Multiple => m.vs
    case m: Value.Single => Vector(m.v)
    case m => Vector(m.toText)
  }
}
object I18NFragment {
  def create(ps: Seq[Dox]): I18NFragment = ps.toList match {
    case Nil => _create_distill(Nil)
    case x :: Nil => x match {
      case m: I18NFragment => m
      case _ => _create_distill(ps)
    }
    case xs => _create_distill(xs)
  }

  def create(p: Dox): I18NFragment = p match {
    case m: I18NFragment => m
    case m: Fragment => create(m.contents)
    case m => create(List(m))
  }

  case class Z(
    xs: Vector[Dox] = Vector.empty,
    ls: Map[Locale, Vector[Dox]] = Map.empty
  ) {
    def r = if (ls.isEmpty)
      I18NFragment(I18NContainer.make(xs.toList))
    else
      I18NFragment(I18NContainer.createSeq(ls))

    def +(rhs: Dox) = rhs.getLanguage match {
      case Some(l) =>
        ls.get(l) match {
          case Some(v) =>
            copy(ls = ls + (l -> (v :+ rhs)))
          case None =>
            copy(ls = ls + (l -> (xs :+ rhs)))
        }
      case None =>
        val a = _make_locales(rhs)
        a match {
          case Some(lrs) =>
            lrs.foldLeft(ZZ(Z.this))(_+_).r
          case None =>
            copy(
              xs = xs :+ rhs,
              ls = ls.keys.foldLeft(ls)((z, x) =>
                ls |+| Map(x -> Vector(rhs)))
            )
        }
    }
  }

  case class ZZ(z: Z) {
    def r = z

    def +(rhs: (Locale, Vector[Dox])) = {
      val (locale, rs) = rhs
      z.ls.get(locale) match {
        case Some(v) => copy(z = z.copy(ls = z.ls + (locale -> (v ++ rs))))
        case None => copy(z = z.copy(ls = z.ls + (locale -> (z.xs ++ rs))))
      }
    }
  }

  private def _create_distill(ps: Seq[Dox]): I18NFragment = {
    ps.foldLeft(Z())(_+_).r
  }

  private def _make_locales(p: Dox): Option[Vector[(Locale, Vector[Dox])]] = {
    val a = _create_distill(p.elements)
    if (a.isSimple) {
      None
    } else {
      val b: Vector[(Locale, List[Dox])] = a.contents.localeVector
      val c = b.map {
        case (k, vs) => k -> Vector(p.copyV(vs).toOption.get)
      }
      Some(c)
    }
  }

  def createList(ps: List[List[Dox]]): I18NFragment = {
    val a = ps.map(create)
    val b: List[Vector[(Locale, List[Dox])]] = a.map(_.contents.localeVector)
    val locales: List[Locale] = b.flatMap(_.toList).map(_._1).distinct
    case class Z(xs: Vector[(Locale, List[Dox])] = Vector.empty) {
      def r = I18NFragment.createDox(xs)

      def +(rhs: Locale) = {
        val x: List[Dox] = b.flatMap(_.filter(_._1 == rhs).flatMap(_._2))
        copy(xs = xs :+ (rhs -> x))
      }
    }
    locales.foldLeft(Z())(_+_).r
  }

  def create(p: String): I18NFragment = create(List(Text(p)))

  def create(p: I18NString): I18NFragment = {
    val a = p.localeVector.map {
      case (k, v) => k -> List(Text(v))
    }
    I18NFragment(I18NContainer.create(a))
  }

  def createString(p: Map[Locale, String]): I18NFragment = {
    val a = p.mapValues(x => List(Text(x)))
    I18NFragment(I18NContainer.createSeq(a))
  }

  def createDox(p: Seq[(Locale, Seq[Dox])]) =
    I18NFragment(I18NContainer.createSeq(p))

  def enja(en: String, ja: String) = I18NFragment(
    I18NContainer.enja(List(Text(en)), List(Text(ja)))
  )

  def getC(name: String, p: XNode): Consequence[Option[I18NFragment]] =
    for {
      a <- XmlUtils.getI18NContainerC(p, name)
      r <- a.traverse(x => _make_i18nfragment(x))
    } yield r

  private def _make_i18nfragment(p: I18NContainer[List[XNode]]): Consequence[I18NFragment] =
  Consequence {
    val a = p.mapValue(xs => xs.map(PureParser.build))
    I18NFragment(a)
  }

  def buildDescription(p: Block): I18NFragment = {
    val commons = p.elements.takeWhile {
      case m: Section => false
      case _ => true
    }
    val ss = p.sections
    ss match {
      case Nil => create(p.elements)
      case xs =>
        val a = xs.map(x => (Locale.forLanguageTag(x.nameForModel), commons ++ x.contents))
        createDox(a)
    }
  }

  def buildValueOrValues(p: Block): I18NFragment = {
    val ss = p.sections
    ss match {
      case Nil => create(p.elements)
      case xs =>
        val a = xs.map(x => (Locale.forLanguageTag(x.nameForModel), List(Dox.toValueOrValues(x))))
        createDox(a)
    }
  }
}

case class Caption(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Caption => contents == m.contents && attributes == m.attributes
    case _ => false
  }

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

  override def equals_Value(o: Dox) = o match {
    case m: Figure => img == m.img && caption == m.caption && label == m.label && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_figure(cs).map { case (i, c) =>
      copy(i | img, c | caption, label)
    }
  }
}
object Figure {
  def apply(img: Img, name: String): Figure = Figure(img, Figcaption(name))

  def apply(img: Img, caption: InlineContents): Figure =
    Figure(img, Figcaption(caption))
}

case class Figcaption(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Figcaption => contents == m.contents && attributes == m.attributes
    case _ => false
  }

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

  override def equals_Value(o: Dox) = o match {
    case m: EmptyLine => true
    case _ => false
  }

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
  override def equals_Value(o: Dox) = o match {
    case m: Newline => true
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

trait Img extends Inline {
  val src: URI
  val alt: Option[String]
  def attributes: VectorMap[String, String]
  override val elements = Nil
  override def showTerm = "img"
  // override def showParams = List(
  //   "src" -> src.toASCIIString(),
  //   "width" -> "640" // TODO
  // )
  override def showParams = {
    val a = List(
      "src" -> src.toASCIIString()
    )
    val b = ListUtils.buildTupleList("alt" -> alt)
    val c = attributes.toList
    a ++ b ++ c
  }
}

trait EmbeddedImg extends Img {
  val contents: String
  val params: List[String]
}

case class DotImg(
  src: URI,
  contents: String,
  params: List[String] = Nil,
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends EmbeddedImg {
  override def equals_Value(o: Dox) = o match {
    case m: DotImg => src == m.src && contents == m.contents && params == m.params && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

case class DitaaImg(
  src: URI,
  contents: String,
  params: List[String] = Nil,
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends EmbeddedImg {
  override def equals_Value(o: Dox) = o match {
    case m: DitaaImg => src == m.src && contents == m.contents && params == m.params && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2011-01-16
case class Html5(
  name: String,
  attributes: VectorMap[String, String],
  contents: List[Dox],
  location: Option[ParseLocation] = None
) extends Block {
  override val elements = contents
  override def showTerm = name
  override def showParams = attributes.toList

  override def equals_Value(o: Dox) = o match {
    case m: Html5 => name == m.name && attributes == m.attributes && contents == m.contents
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(name, attributes, cs))
  }
}

// 2011-01-17
case class Program private(
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None,
  callouts: Dox.Callouts = Dox.Callouts.empty
) extends Block with Preserve {
  override val elements = List(new Text(contents))
  override def showTerm = "pre"
  override def showParams = attributes.list ++ List("class" -> "program")

  def kind: Option[String] = attributes.get("kind")
  def caption: Option[String] = attributes.get("caption")

  override protected def print_Open(buf: StringBuilder): Unit = {
    print_open_tag(buf, "program", attributes)
  }

  override protected def print_Close(buf: StringBuilder): Unit = {
    print_close_tag(buf, "program")
  }

  override def equals_Value(o: Dox) = o match {
    case m: Program => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_plain_text(cs).map(_ => this)
  }
}
object Program {
  def create(p: String): Program =
    create(p, VectorMap.empty[String, String], None)

  def create(p: String, attrs: Map[String, String]): Program =
    create(p, VectorMap(attrs), None)

  def create(p: String, attrs: Seq[(String, String)]): Program =
    create(p, VectorMap(attrs), None)

  def create(p: String, attrs: Map[String, String], location: Option[ParseLocation]): Program = {
    val (s, cs) = _extract_callouts(_normalize(p))
    Program(s, VectorMap(attrs), location, cs)
  }

  def create(p: String, attr: (String, String), attrs: (String, String)*): Program =
    create(p, VectorMap(attr +: attrs))

  def create(p: Seq[String], attr: (String, String), attrs: (String, String)*): Program =
    create(p.mkString("\n"), VectorMap(attr +: attrs))


  def create(p: String, kind: Option[String], caption: Option[String]): Program =
    create(p, ListUtils.buildTupleList("kind" -> kind, "caption" -> caption))

  private def _normalize(p: String): String =
    if (p.endsWith("\n") || p.endsWith("\r"))
      p
    else
      p + "\n"

  private def _extract_callouts(p: String): (String, Dox.Callouts) = {
    val Explicit = """<(\d+)>\s*(.*)""".r
    val Auto = """<>\s*(.*)""".r
    val callouts = new Dox.Callouts.Builder()
    val taken = scala.collection.mutable.Set[Int]()
    val lines = p.linesIterator.toVector
    val result = new StringBuilder
    var auto = 1

    for (line <- lines) {
      val commentIndex = line.indexOf("//")
      if (commentIndex >= 0) {
        val code = line.substring(0, commentIndex)
        val comment = line.substring(commentIndex + 2).trim
        comment match {
          case Explicit(n, desc) =>
            val num = n.toInt
            taken += num
            callouts.add(num, desc.trim)
            result.append(code + s"<$num>")
          case Auto(desc) =>
            while (taken.contains(auto)) auto += 1
            val num = auto
            taken += num
            callouts.add(num, desc.trim)
            result.append(code + s"<$num>")
          case _ =>
            result.append(line)
        }
      } else {
        result.append(line)
      }
      result.append("\n")
    }

    (_normalize(result.toString), callouts.build())
  }
}

case class Console(
  contents: String,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Block with Preserve {
  override val elements = List(new Text(contents))
  override def showTerm = "pre"
  override def showParams = attributes.list ++ List("class" -> "console")

  override def equals_Value(o: Dox) = o match {
    case m: Console => contents == m.contents && attributes == m.attributes
    case _ => false
  }

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

  override def equals_Value(o: Dox) = o match {
    case m: SmartDoc => name == m.name && attributes == m.attributes && contents == m.contents
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    Success(copy(name, attributes, cs))
  }
}

// 2011-01-20
case class SmCsvImg(
  src: URI,
  contents: String,
  params: List[String] = Nil,
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends EmbeddedImg {
  override def equals_Value(o: Dox) = o match {
    case m: SmCsvImg => src == m.src && contents == m.contents && params == m.params && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_empty(cs).map(_ => this)
  }
}

// 2012-02-15
object EmptyDox extends Dox with DoxFactory with Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty
  def location: Option[ParseLocation] = None

  val label = "empty"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dox = EmptyDox

  override def toString(buf: StringBuilder, maxlength: Option[Int] = None) {
  }

  override def equals_Value(o: Dox) = o eq this
}

// 2012-04-24
case class Tt(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents

  override def equals_Value(o: Dox) = o match {
    case m: Tt => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, contents)))
  }
}

object Tt extends Tt(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "tt"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Tt =
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

  override def children = contents

  override def equals_Value(o: Dox) = o match {
    case m: Span => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map { x =>
      val r = copy(x, location = get_location(location, contents))
      // println(s"Span#copyV: $r")
      r
    }
  }
}

object Span extends Span(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "span"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Span =
    Span(ensure_inline(body), attrs)

  def apply(element: Inline) = new Span(List(element))

  def create(locale: Locale, p: String): Span = Span(
    List(Text(p)),
    VectorMap("lang" -> locale.toLanguageTag)
  )

  def create(locale: Locale, p: List[Inline]): Span = Span(
    p,
    VectorMap("lang" -> locale.toLanguageTag)
  )

  def createEn(p: String) = create(LocaleUtils.en, p)
  def createJa(p: String) = create(LocaleUtils.ja, p)

  def build(elem: XNode): Span = {
    val cs = PureParser.buildInline(elem)
    val attrs = PureParser.getAttributes(elem)
    Span(cs, attrs)
  }
}

// 2025-03-03
case class Dfn(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "dfn"
  override def showParams = ListUtils.buildTupleList("id" -> attributes.get("id"))

  override def equals_Value(o: Dox) = o match {
    case m: Dfn => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, contents)))
  }
}

object Dfn extends Dfn(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "dfn"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dfn =
    Dfn(ensure_inline(body))

  def apply(element: Inline) = new Dfn(List(element))
}

// 2025-03-03
case class Abbr(
  contents: List[Inline],
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Inline {
  override val elements = contents
  override def showTerm = "abbr"

  override def equals_Value(o: Dox) = o match {
    case m: Abbr => contents == m.contents && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, location = get_location(location, contents)))
  }
}

object Abbr extends Abbr(Nil, VectorMap.empty, None) with DoxFactory {
  val label = "abbr"

  def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Abbr =
    Abbr(ensure_inline(body))

  def apply(element: Inline) = new Abbr(List(element))
}

// 2012-11-23
case class IncludeDoc(filename: String) extends Block {
  def attributes: VectorMap[String, String] = VectorMap.empty
  def location: Option[ParseLocation] = None

  override def showParams = List(("filename", filename))

  override def equals_Value(o: Dox) = o match {
    case m: IncludeDoc => filename == m.filename
    case _ => false
  }

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
  alt: Option[String] = None,
  attributes: VectorMap[String, String] = VectorMap.empty,
  location: Option[ParseLocation] = None
) extends Img {
  val src: URI = {
    import org.apache.commons.codec.binary.Base64
    val binary = Base64.encodeBase64String(chunk.toByteArray)
//    val a = s"""data:${name}/${suffix};base64,${binary}"""
    val a = s"""data:${mime.name};base64,${binary}"""
    new URI(a)
  }

  override def equals_Value(o: Dox) = o match {
    case m: BinaryImg => name == m.name && mime == m.mime && chunk == m.chunk && attributes == m.attributes
    case _ => false
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

  override def equals_Value(o: Dox) = o match {
    case m: UnresolvedLink => contents == m.contents && data == m.data && attributes == m.attributes
    case _ => false
  }

  override def copyV(cs: List[Dox]) = {
    to_inline(cs).map(copy(_, data, attributes, location = get_location(location, cs)))
  }
}
object UnresolvedLink {
  def apply(label: String, data: Any): UnresolvedLink =
    UnresolvedLink(List(Dox.text(label)), data)
}

// 2025-07-17
case class Include(
  directive: BlockMacro.Include,
  location: Option[ParseLocation] = None
) extends Directive {
  def attributes: VectorMap[String, String] = VectorMap.empty
  override def isVisialBlock: Boolean = false
  override def equals_Value(o: Dox) = o == this

  def target = directive.target
  def parameters = directive.parameters
}
object Include {
  def create(p: String): Include = Include(BlockMacro.Include(p))
  def create(p: URI): Include = Include(BlockMacro.Include.create(p))
}

// 2025-07-17
case class Error(
  conclusion: Conclusion,
  location: Option[ParseLocation] = None
) extends Inline {
  def attributes: VectorMap[String, String] = VectorMap.empty
  override def equals_Value(o: Dox) = o == this
}

// 2025-09-01
sealed trait Value extends Inline {
  def toI18NHangar: I18NHangar[String]
  def values: Vector[String]
//  def toInlineContentsList: List[InlineContents]
}
object Value {
  case class Single(v: String) extends Value {
    def attributes: VectorMap[String, String] = VectorMap.empty
    def location: Option[ParseLocation] = None
    override def equals_Value(o: Dox) = o == this

//    def toInlineContentsList: List[InlineContents] = List(List(this))

    override protected def to_Text(buf: StringBuilder) {
      buf.append(v)
    }

    override protected def to_Plain_Text(buf: StringBuilder) {
      buf.append(v)
    }

    override def to_Data(buf: StringBuilder) {
      buf.append(v)
    }

    def toI18NHangar: I18NHangar[String] = I18NHangar.createCommons(v)
    def values: Vector[String] = Vector(v)
  }

  case class Multiple(vs: Vector[String]) extends Value {
    def attributes: VectorMap[String, String] = VectorMap.empty
    def location: Option[ParseLocation] = None
    override def equals_Value(o: Dox) = o == this

//    def toInlineContentsList: List[InlineContents] = vs.map(x => List(Single(x))).toList

    private def _values = vs.mkString(", ")

    override protected def to_Text(buf: StringBuilder) {
      buf.append(_values)
    }

    override protected def to_Plain_Text(buf: StringBuilder) {
      buf.append(_values)
    }

    override def to_Data(buf: StringBuilder) {
      buf.append(_values)
    }

    def toI18NHangar: I18NHangar[String] = I18NHangar.createCommons(vs)
    def values: Vector[String] = vs
  }

  case class I18N(hangar: I18NHangar[String]) extends Value {
    def attributes: VectorMap[String, String] = VectorMap.empty
    def location: Option[ParseLocation] = None
    override def equals_Value(o: Dox) = o == this

    def distill(locale: Locale): Option[Value] = {
      hangar.get(locale).map(Value.apply)
    }

    def toI18NHangar: I18NHangar[String] = hangar
    def values: Vector[String] = hangar.valueVector
  }

  def apply(p: String): Value = Single(p)

  def apply(ps: Seq[String]): Value = Multiple(ps.toVector)

  def create(ps: Seq[Dox]): Value = {
    case class Z(
      map: Map[Locale, Vector[String]] = Map.empty,
      common: Vector[String] = Vector.empty
    ) {
      def r =
        if (map.isEmpty)
          Single(common.mkString)
        else if (common.isEmpty)
          I18N(I18NHangar.createOne(map.mapValues(_.mkString)))
        else
          I18N(I18NHangar.createOne(map.mapValues(_.mkString), common.mkString))

      def +(rhs: Dox) = rhs match {
        case m: I18NFragment => copy(map |+| m.toVectorMapStringVector)
        case m: Value.I18N => copy(
          map |+| m.hangar.map,
          common ++ m.hangar.commons
        )
        case m => m.getLanguage match {
          case Some(l) => copy(map |+| Map(l -> Vector(m.toPlainText)))
          case None => copy(common = common :+ m.toPlainText)
        }
      }
    }
    ps.foldLeft(Z())(_+_).r
  }

  def createMulti(ps: Seq[Seq[Dox]]): Value = {
    val a = ps.map(create)
    case class Z(builder: I18NHangar.Builder[String] = I18NHangar.Builder()) {
      def r = I18N(builder.build())

      def +(rhs: Value) = rhs match {
        case m: I18NFragment => copy(builder.add(m.toVectorMapString))
        case m: Value.I18N => copy(builder.add(m.hangar))
        case m => m.getLanguage match {
          case Some(l) => copy(builder.add(Map(l -> m.toPlainText)))
          case None => copy(builder.add(m.toPlainText))
        }
      }
    }
    a.foldLeft(Z())(_+_).r
  }

  def buildValueOrValuesI18N(p: Block): Value = {
    val ss = p.sections
    ss match {
      case Nil => Dox.toValueOrValues(p.elements)
      case xs =>
        // TODO common
        val a = xs.map(x => (Locale.forLanguageTag(x.nameForModel), Dox.toValueOrValues(x).values))
        val b = I18NHangar.create(a.toMap)
        I18N(b)
    }
  }
}

// 2025-09-09
// case class Verbatim(
//   contents: List[Inline],
//   attributes: VectorMap[String, String] = VectorMap.empty,
//   location: Option[ParseLocation] = None
// ) extends Inline {
//   override val elements = contents

//   override def equals_Value(o: Dox) = o match {
//     case m: Verbatim => contents == m.contents && attributes == m.attributes
//     case _ => false
//   }

//   override def copyV(cs: List[Dox]) = {
//     to_inline(cs).map(copy(_, location = get_location(location, cs)))
//   }
// }

// object Verbatim extends Verbatim(Nil, VectorMap.empty, None) with DoxFactory {
//   val label = "verbatim"

//   def apply(attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Verbatim =
//     Verbatim(body.toList)

//   def apply(element: Inline) = new Verbatim(List(element))

//   def build(elem: XNode): Verbatim = {
//     val cs = PureParser.buildInline(elem)
//     val attrs = PureParser.getAttributes(elem)
//     Verbatim(cs, attrs)
//   }
// }
