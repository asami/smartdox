package org.smartdox

/*
 * @since   Aug. 19, 2026
 * @version Aug. 29, 2026
 * @author  ASAMI, Tomoharu
 */

import scalaz.{Value => _, _}, Scalaz._, WriterT._, Show._, Validation._
import java.util.Locale
import scala.xml.{Node => XNode, _}
import org.goldenport.context.Consequence
import org.goldenport.collection.VectorMap
import org.goldenport.parser._
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NContainer
import org.goldenport.i18n.I18NHangar
import org.goldenport.i18n.LocaleUtils
import org.goldenport.xml.XmlUtils
import org.smartdox.parser.Dox2Parser
import org.smartdox.parser.PureParser
import org.smartdox.util.DoxUtils


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

  override def show_Contents(buf: StringBuilder) = print_Contents(buf)

  override def printDox(buf: StringBuilder): Unit =
    if (isSimple)
      _print_contents(buf, _neutral_source)
    else
      super.printDox(buf)

  override def print_Contents(buf: StringBuilder): Unit = {
    _print_contents(buf, _neutral_source)
    for ((locale, xs) <- _localized_source_locale_vector) {
      val language = locale.toLanguageTag
      buf.append("<")
      buf.append(language)
      buf.append(">")
      _print_contents(buf, xs)
      buf.append("</")
      buf.append(language)
      buf.append(">")
    }
  }

  private def _print_contents(buf: StringBuilder, xs: Seq[Dox]): Unit =
    for (x <- xs) {
      x.printDox(buf)
    }

  def isSimple: Boolean = !_has_exact_localized_source

  def hasExactSource(locale: Locale): Boolean =
    _exact_source(locale).exists(_.nonEmpty)

  private[smartdox] def _exact_source_contents(locale: Locale): List[Dox] =
    _exact_source(locale).getOrElse(Nil)

  def distillExact(locale: Locale): List[Dox] =
    if (locale == LocaleUtils.C)
      _neutral_source
    else
      _neutral_source ++ _exact_source_contents(locale)

  def distill(locale: Option[Locale]): List[Dox] =
    locale.fold(_legacy_contents.default)(distill)

  def distill(locale: Locale): List[Dox] = _legacy_contents.apply(locale)

  def distillInline(locale: Locale): List[Inline] =
    distill(locale).flatMap(Dox.distillInline)

  def distillI18NFragment(locale: Option[Locale]): I18NFragment =
    locale.fold(distillI18NFragmentDefault)(distillI18NFragment)

  def distillI18NFragment(locale: Locale): I18NFragment =
    I18NFragment(I18NContainer.make(distill(locale)))

  def distillI18NFragmentDefault: I18NFragment =
    I18NFragment(I18NContainer.make(distill(None)))

  def distillString(locale: Locale): String = Dox.toPlainText(distillInline(locale))

  def distillStringDefault: String = Dox.toPlainText(distill(None))

  def distillInlineContentsDefault: InlineContents = distill(None).map {
    case m: Inline => m
  }

  def toI18NString: I18NString = {
    val a = _legacy_contents
    I18NString(
      Dox.toPlainText(a.c),
      Dox.toPlainText(a.en),
      Dox.toPlainText(a.ja),
      a.map.mapValues(Dox.toPlainText)
    )
  }

  def makeInlines: List[Inline] =
    if (isSimple)
      Dox.toInlineContents(_neutral_source)
    else
      _make_inlines

  private def _make_inlines = {
    _source_locale_vector.flatMap {
      case (locale, xs) if locale == LocaleUtils.C => Dox.toInlineContents(xs)
      case (locale, xs) => List(Span.create(locale, Dox.toInlineContents(xs)))
    }.toList
  }

  def toInlines: List[Inline] =
    if (isSimple)
      Dox.toInlineContents(_neutral_source)
    else
      List(this)

  def makeParagraphs: List[Paragraph] =
    if (isSimple)
      Dox.toParagraphs(_neutral_source)
    else
      _make_paragraphs

  private def _make_paragraphs: List[Paragraph] = {
    _source_locale_vector.flatMap {
      case (locale, xs) if locale == LocaleUtils.C => Dox.toParagraphs(xs)
      case (locale, xs) => List(Paragraph.create(locale, xs))
    }.toList
  }

  def +:(p: Char): I18NFragment = _prepend(p.toString)

  def +:(p: String): I18NFragment = _prepend(p)

  private def _prepend(p: String): I18NFragment = {
    def _prepend_(xs: List[Dox]): List[Dox] = xs match {
    case Nil => List(Text(p))
    case x :: xs => x match {
      case m: Text => m.prepend(p) :: xs
      case m => Text(p) :: x :: xs
    }
    }
    if (_has_source_provenance) {
      val source = _source_entries.map {
        case (locale, xs) if locale == LocaleUtils.C => locale -> _prepend_(xs)
        case entry => entry
      }
      val updated = if (source.exists(_._1 == LocaleUtils.C)) source else
        source :+ (LocaleUtils.C -> _prepend_(Nil))
      copy(contents = I18NFragment._source_container(updated, _legacy_contents.mapValues(_prepend_)))
    } else {
      copy(contents = contents.mapValues(_prepend_))
    }
  }

  def mapValues(f: List[Dox] => List[Dox]): I18NFragment =
    if (_has_source_provenance)
      copy(contents = I18NFragment._source_container(
        _source_entries.map { case (locale, xs) => locale -> f(xs) },
        _legacy_contents.mapValues(f)
      ))
    else
      I18NFragment(contents.mapValues(f))

  def trimSingleLine: I18NFragment = {
    if (_has_source_provenance) {
      val source = _source_entries.map(_trim_single_line)
      val legacy = I18NContainer.create(_legacy_contents.localeVector.map(_trim_single_line))
      copy(contents = I18NFragment._source_container(source, legacy))
    } else {
      val lv = contents.localeVector
      val r = lv.map(_trim_single_line)
      copy(contents = I18NContainer.create(r))
    }
  }

  private def _trim_single_line(p: (Locale, List[Dox])): (Locale, List[Dox]) = {
    val (k, vs) = p
    case class Z(
      xs: Vector[Dox] = Vector.empty,
      isdone: Boolean = false
    ) {
      def r = xs.toList

      def +(rhs: Dox) =
        if (isdone)
          this
        else
          rhs match {
            case m: Text =>
              val s = m.contents
              if (s.contains("\n") || s.contains("\r"))
                copy(xs = xs :+ Text(DoxUtils.trimSingleLine(s)), isdone = true)
              else
                copy(xs = xs :+ m)
            case m: Paragraph =>
              if (xs.isEmpty)
                copy(xs = xs :+ rhs, isdone = true)
              else
                copy(isdone = true)
            case m => copy(xs = xs :+ rhs)
          }
    }
    val r = vs.foldLeft(Z())(_+_).r
    // val r = _find_text(v) match {
    //   case Some(s) => List(Text(DoxUtils.trimSingleLine(s.contents)))
    //   case None => Nil
    // }
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
    val a: Vector[(Locale, List[Dox])] = _legacy_contents.localeVector
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

  private def _legacy_contents: I18NContainer[List[Dox]] =
    if (_has_source_provenance) {
      val others = _source_entries.collect {
        case (locale, xs) if !_is_canonical_locale(locale) =>
          locale -> _legacy_localized_source(xs)
      }.toMap
      I18NContainer(contents.c, contents.en, contents.ja, others)
    }
    else
      contents

  private def _neutral_source: List[Dox] =
    if (_has_source_provenance)
      contents.map.getOrElse(LocaleUtils.C, Nil)
    else if (contents.isSimple || contents.c != contents.en)
      contents.c
    else
      Nil

  private def _exact_source(locale: Locale): Option[List[Dox]] =
    if (_has_source_provenance)
      contents.map.get(locale)
    else if (contents.isSimple || locale == LocaleUtils.C)
      None
    else {
      locale match {
        case Locale.ENGLISH => Some(contents.en)
        case Locale.JAPANESE => Some(contents.ja)
        case _ => contents.map.get(locale)
      }
    }

  private def _has_exact_localized_source: Boolean =
    if (_has_source_provenance)
      contents.map.exists {
        case (locale, xs) => locale != LocaleUtils.C && xs.nonEmpty
      }
    else if (contents.isSimple)
      false
    else
      contents.en.nonEmpty || contents.ja.nonEmpty || contents.map.values.exists(_.nonEmpty)

  private def _legacy_localized_source(xs: List[Dox]): List[Dox] =
    if (_is_distributed_source)
      _neutral_source ++ xs
    else
      xs

  private def _is_distributed_source: Boolean =
    contents.map.get(LocaleUtils.C).exists(_ != contents.c)

  private def _is_canonical_locale(locale: Locale): Boolean =
    locale == LocaleUtils.C || locale == LocaleUtils.en || locale == LocaleUtils.ja

  private def _source_entries: Vector[(Locale, List[Dox])] =
    if (_has_source_provenance)
      contents.map.toVector
    else
      contents.localeVector

  private def _source_locale_vector: Vector[(Locale, List[Dox])] =
    if (_has_source_provenance) {
      val neutral = contents.map.get(LocaleUtils.C).filter(_.nonEmpty).toVector.map(LocaleUtils.C -> _)
      val localized = _localized_source_locale_vector
      neutral ++ localized
    } else {
      contents.localeVector.filter(_._2.nonEmpty)
    }

  private def _localized_source_locale_vector: Vector[(Locale, List[Dox])] =
    _source_entries.collect {
      case (locale, xs) if locale != LocaleUtils.C && xs.nonEmpty &&
          locale.toLanguageTag.nonEmpty && locale.toLanguageTag != "und" => locale -> xs
    }.sortBy(_._1.toLanguageTag)

  private def _has_source_provenance: Boolean = contents.map.nonEmpty
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

  private case class SourceZ(
    z: Z = Z(),
    sourcec: Vector[Dox] = Vector.empty,
    sourcels: Map[Locale, Vector[Dox]] = Map.empty
  ) {
    def r = {
      val legacy = if (z.ls.isEmpty)
        I18NContainer.make(z.xs.toList)
      else
        I18NContainer.createSeq(z.ls)
      val source =
        (if (sourcec.nonEmpty) Vector(LocaleUtils.C -> sourcec) else Vector.empty) ++
          sourcels.toVector
      I18NFragment._create_source(source, legacy)
    }

    def +(rhs: Dox): SourceZ = rhs.getLanguage match {
      case Some(l) =>
        val legacy = z.ls.get(l) match {
          case Some(v) => z.ls + (l -> (v :+ rhs))
          case None => z.ls + (l -> (z.xs :+ rhs))
        }
        val source = sourcels.get(l) match {
          case Some(v) => sourcels + (l -> (v :+ rhs))
          case None => sourcels + (l -> Vector(rhs))
        }
        copy(z = z.copy(ls = legacy), sourcels = source)
      case None =>
        _make_locales(rhs) match {
          case Some(lrs) => lrs.foldLeft(SourceZZ(this))(_+_).r
          case None =>
            val legacy = z.ls.keys.foldLeft(z.ls)((acc, locale) =>
              acc |+| Map(locale -> Vector(rhs)))
            copy(
              z = z.copy(xs = z.xs :+ rhs, ls = legacy),
              sourcec = sourcec :+ rhs
            )
        }
    }
  }

  private case class SourceZZ(accumulator: SourceZ) {
    def r = accumulator

    def +(rhs: (Locale, Vector[Dox])) = {
      val (locale, rs) = rhs
      val legacy = accumulator.z.ls.get(locale) match {
        case Some(v) => accumulator.z.ls + (locale -> (v ++ rs))
        case None => accumulator.z.ls + (locale -> (accumulator.z.xs ++ rs))
      }
      if (locale == LocaleUtils.C)
        copy(accumulator = accumulator.copy(
          z = accumulator.z.copy(ls = legacy),
          sourcec = accumulator.sourcec ++ rs
        ))
      else {
        val source = accumulator.sourcels.get(locale) match {
          case Some(v) => accumulator.sourcels + (locale -> (v ++ rs))
          case None => accumulator.sourcels + (locale -> rs)
        }
        copy(accumulator = accumulator.copy(
          z = accumulator.z.copy(ls = legacy),
          sourcels = source
        ))
      }
    }
  }

  private def _create_distill(ps: Seq[Dox]): I18NFragment = {
    ps.foldLeft(SourceZ())(_+_).r
  }

  private def _make_locales(p: Dox): Option[Vector[(Locale, Vector[Dox])]] = {
    val a = _create_distill(p.elements)
    if (a.isSimple) {
      None
    } else {
      val b: Vector[(Locale, List[Dox])] = a._source_locale_vector
      val c = b.map {
        case (k, vs) => k -> Vector(p.copyV(vs).toOption.get)
      }
      Some(c)
    }
  }

  def createList(ps: List[List[Dox]]): I18NFragment = {
    val a = ps.map(create)
    val b: List[Vector[(Locale, List[Dox])]] = a.map(_._source_locale_vector)
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
    val source = if (p.map.nonEmpty)
      p.map.toVector.map { case (k, v) => k -> List(Text(v)) }
    else if (p.isSimple)
      Vector(LocaleUtils.C -> List(Text(p.c)))
    else
      a
    _create_source(source, I18NContainer.create(a))
  }

  def create[A <: Dox](p: I18NHangar[A]): I18NFragment = {
    I18NFragment(I18NContainer.createList(p))
  }

  def create[A <: Dox](p: I18NContainer[A]): I18NFragment =
    I18NFragment(p.mapValues(x => List(x)))

  def createString(p: Map[Locale, String]): I18NFragment = {
    val a = p.mapValues(x => List(Text(x)))
    _create_source(a.toVector)
  }

  def createDox(p: Seq[(Locale, Seq[Dox])]) =
    _create_source(p)

  def enja(en: String, ja: String) = createDox(List(
    LocaleUtils.en -> List[Dox](Text(en)),
    LocaleUtils.ja -> List[Dox](Text(ja))
  ))

  def parseInclusion(p: String): I18NFragment =
    Dox2Parser.parseI18NFragmentC(p).foldConclusion(c => I18NFragment.create(c.message))

  def parseOptionInclusion(p: String): Option[I18NFragment] =
    Dox2Parser.parseI18NFragmentOptionC(p).foldConclusion(c => Some(I18NFragment.create(c.message)))

  def getC(name: String, p: XNode): Consequence[Option[I18NFragment]] =
    for {
      a <- XmlUtils.getI18NContainerC(p, name)
      r <- a.traverse(x => _make_i18nfragment(x))
    } yield r

  private def _make_i18nfragment(p: I18NContainer[List[XNode]]): Consequence[I18NFragment] =
  Consequence {
    val a = p.mapValues(xs => xs.map(PureParser.build))
    val source = if (a.map.nonEmpty) a.map.toVector else a.localeVector
    _create_source(source, a)
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

  private def _create_source(p: Seq[(Locale, Seq[Dox])]): I18NFragment =
    _create_source(p, I18NContainer.createSeq(p))

  private def _create_source(
    p: Seq[(Locale, Seq[Dox])],
    legacy: I18NContainer[List[Dox]]
  ): I18NFragment =
    I18NFragment(_source_container(p, legacy))

  private def _source_container(
    p: Seq[(Locale, Seq[Dox])],
    legacy: I18NContainer[List[Dox]]
  ): I18NContainer[List[Dox]] = {
    val source = p.foldLeft(Map.empty[Locale, List[Dox]]) {
      case (z, (locale, xs)) =>
        z + (locale -> (z.getOrElse(locale, Nil) ++ xs.toList))
    }
    I18NContainer(
      legacy.c,
      legacy.en,
      legacy.ja,
      source
    )
  }
}
