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
import org.goldenport.values.PathName
import org.goldenport.values.LocalDateOrDateTime
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NContainer
import org.goldenport.i18n.I18NHangar
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.LocaleUtils
import org.goldenport.xml.XmlUtils
import org.goldenport.record.v3.{Table => RTable}
import org.goldenport.record.v3.ITable
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
import org.smartdox.structure.StructureObject.KeyContent
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
 *  version Oct. 28, 2025
 *  version Nov. 22, 2025
 *  version Dec. 11, 2025
 *  version Apr. 16, 2026
 *  version Jun.  3, 2026
 * @version Aug. 19, 2026
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
  def getClassName: Option[String] = attributeMap.get("class")

  def isAccept(p: Locale): Boolean = getLanguage.fold(true)(LocaleUtils.isAccept(p, _))

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

  protected final def to_inline_force(cs: List[Dox]): ValidationNel[String, List[Inline]] =
    cs.foldRight(Success(Nil): ValidationNel[String, List[Inline]]) {
      case (i: Inline, Success(a)) => Success(i :: a)
      case (i: Inline, e: Failure[_]) => e
      case (d, Success(a)) => to_inline_force(d.elements) match {
        case Success(aa) => Success(aa :: a)
        case e: Failure[_] => e
      }
      case (d, Failure(e)) => Failure(to_failure_message(d) <:: e)
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
    if (es.nonEmpty)
      "Not text".failureNel
    else
      ss.mkString.success
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
    Paragraph,
    Bold,
    Strong,
    Em,
    Italic,
    Underline,
    Code,
    InlineMacro,
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
    Term,
    NoTerm,
    Abbr
  )

  val html5InlineNames = Set("mark")

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

  def toInlineContents(p: String): InlineContents = List(Text(p))

  def toInlineContents(p: I18NString): InlineContents = List(toDox(p))

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

  def toParagraphs(p: Dox): List[Paragraph] = p match {
    case m: Paragraph => List(m)
    case m => List(Paragraph(List(p)))
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

  def create(name: String, attrs: Seq[(String, String)], p: Dox)(implicit ctx: DateTimeContext): Dox =
    p match {
      case m: Fragment => create(name, attrs, m.contents)
      case m => create(name, attrs, List(m))
    }

  def create(name: String, attrs: Seq[(String, String)], body: Seq[Dox])(implicit ctx: DateTimeContext): Dox =
    create(name, VectorMap(attrs), body)

  def create(name: String, attrs: VectorMap[String, String], body: Seq[Dox])(implicit ctx: DateTimeContext): Dox =
    tags.toStream.flatMap(_.applyOption(name, attrs, body)).headOption.
      getOrElse {
        if (_is_html5_inline(name))
          Html5Inline(name, attrs, body.toList)
        else if (_is_html5(name))
          Html5(name, attrs, body.toList)
        else
          RAISE.notImplementedYetDefect(s"$name")
      }

  // Parser-only source provenance for values created through Dox.create.
  // Keep this here, rather than in individual parser states, so every supported
  // tag gets identical treatment without changing the public factory API.
  private[smartdox] def attachLocation(dox: Dox, location: Option[ParseLocation]): Dox =
    location.map { p =>
      dox match {
        case m: Document => m.copy(location = Some(p))
        case m: Head => m.copy(location = Some(p))
        case m: Body => m.copy(location = Some(p))
        case m: Div => m.copy(location = Some(p))
        case m: Paragraph => m.copy(location = Some(p))
        case m: Bold => m.copy(location = Some(p))
        case m: Strong => m.copy(location = Some(p))
        case m: Em => m.copy(location = Some(p))
        case m: Italic => m.copy(location = Some(p))
        case m: Underline => m.copy(location = Some(p))
        case m: Code => m.copy(location = Some(p))
        case m: InlineMacro => m.copy(location = Some(p))
        case m: Pre => m.copy(location = Some(p))
        case m: Ul => m.copy(location = Some(p))
        case m: Ol => m.copy(location = Some(p))
        case m: Li => m.copy(location = Some(p))
        case m: Del => m.copy(location = Some(p))
        case m: Hyperlink => m.copy(location = Some(p))
        case m: ReferenceImg => m.copy(location = Some(p))
        case m: Dl => m.copy(location = Some(p))
        case m: Dt => m.copy(location = Some(p))
        case m: Dd => m.copy(location = Some(p))
        case m: Fragment => m.copy(location = Some(p))
        case m: Tt => m.copy(location = Some(p))
        case m: Span => m.copy(location = Some(p))
        case m: Dfn => m.copy(location = Some(p))
        case m: Term => m.copy(location = Some(p))
        case m: NoTerm => m.copy(location = Some(p))
        case m: Abbr => m.copy(location = Some(p))
        case m: Html5 => m.copy(location = Some(p))
        case m: Html5Inline => m.copy(location = Some(p))
        case m => m
      }
    }.getOrElse(dox)

  private def _is_html5_inline(name: String): Boolean = html5InlineNames(name)

  private def _is_html5(name: String) = true // TODO

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

  private def _to_values_as_inline_contents(p: Block): List[InlineContents] = {
    val ulopt = p.elements.collectFirst { case m: Ul => m }
    ulopt match {
      case Some(ul) => Ul.toValuesAsInlineContents(ul)
      case None => List(_to_value_as_inline_contents(p))
    }
  }

  private def _to_value_as_inline_contents(p: Block): InlineContents = {
    val xs = p.elements
    val a = xs.toStream.map {
      case m: Block => _to_value_as_inline_contents(m)
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

  def parseInlineContentsInclusion(p: String): List[Inline] =
    I18NFragment.parseInclusion(p).toInlines

  def parseInlineContentsInclusion(p: Inline): List[Inline] = p match {
    case m: Text => parseInlineContentsInclusion(m.contents)
    case m => List(m)
  }

  def parseInlineContentsInclusion(ps: Seq[Inline]): List[Inline] = ps.toList match {
    case Nil => Nil
    case x :: Nil => parseInlineContentsInclusion(x)
    case xs => xs
  }

  def getLocale(p: TreeNode[Dox]): Option[Locale] =
    p.getContent.flatMap(getLocale)

  def getLocale(p: Dox): Option[Locale] = p.getLanguage

  def getLocaleInContext(p: TreeNode[Dox]): Option[Locale] = {
    getLocale(p) orElse {
      if (p.parent.isRoot)
        None
      else
        getLocaleInContext(p.parent)
    }
  }
}
