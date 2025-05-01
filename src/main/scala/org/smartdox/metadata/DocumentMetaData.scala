package org.smartdox.metadata

import scala.util.Try
import org.joda.time.DateTime
import com.typesafe.config.{Config => Hocon}
import org.goldenport.i18n.I18NString
import org.goldenport.util.VectorUtils
import org.goldenport.util.AnyUtils
import org.smartdox._
import org.smartdox.generator.Context

/*
 * @since   Apr. 29, 2025
 * @version Apr. 30, 2025
 * @author  ASAMI, Tomoharu
 */
case class DocumentMetaData(
  title: Option[String] = None,
  description: Option[String] = None,
  author: Option[String] = None,
  keywords: List[String] = Nil,
  datePublished: Option[DateTime] = None,
  dateModefied: Option[DateTime] = None
) {
  import DocumentMetaData._

  def isEmpty = title.isEmpty && description.isEmpty && author.isEmpty && keywords.isEmpty && dateModefied.isEmpty && dateModefied.isEmpty

  def toOption = if (isEmpty) None else Some(this)

  def complementTitleDate(
    ptitle: InlineContents,
    pdate: InlineContents
  )(implicit context: Context) = {
    val t = title orElse _to_text(ptitle)
    val d = _to_date(pdate)
    val (dp, dm) = (datePublished, dateModefied) match {
      case (Some(p), Some(m)) => (Some(p), Some(m))
      case (Some(p), None) => (Some(p), d)
      case (None, Some(m)) => (d, Some(m))
      case (None, None) => (d, None)
    }
    copy(
      title = title orElse t,
      datePublished = dp,
      dateModefied = dm
    )
  }

  private def _to_text(p: InlineContents): Option[String] = p match {
    case Nil => None
    case xs => Some(Dox.toText(xs))
  }

  private def _to_date(
    p: InlineContents
  )(implicit context: Context): Option[DateTime] =
    _to_text(p).flatMap(x => Try {

      ???
    }.toOption)

  def +(rhs: DocumentMetaData): DocumentMetaData =
    DocumentMetaData(
      title orElse rhs.title,
      description orElse rhs.description,
      author orElse rhs.author,
      (keywords ::: rhs.keywords).distinct,
      datePublished orElse rhs.datePublished,
      dateModefied orElse rhs.dateModefied
    )

  def toFlattenVector: Vector[(String, String)] =
    VectorUtils.buildTupleVector(
      PROP_TITLE -> title,
      PROP_DESCRIPTION -> description,
      PROP_AUTHOR -> author,
      PROP_KEYWORDS -> _keywords_string,
      PROP_DATE_PUBLISHED -> datePublished.map(_to_string),
      PROP_DATE_MODIFIED -> dateModefied.map(_to_string)
    )

  private def _keywords_string: Option[String] =
    keywords match {
      case Nil => None
      case xs => Some(xs.mkString(","))
    }

  private def _to_string(p: DateTime) =
    AnyUtils.toString(p)
}

object DocumentMetaData {
  final val PROP_TITLE = "title"
  final val PROP_DESCRIPTION = "description"
  final val PROP_AUTHOR = "author"
  final val PROP_KEYWORDS = "keywords"
  final val PROP_DATE_PUBLISHED = "datePublished"
  final val PROP_DATE_MODIFIED = "dateModified"

  val empty = DocumentMetaData()

  def create(hocon: Hocon): DocumentMetaData = {
    ???
  }
}
