package org.smartdox.metadata

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._

/*
 * @since   Feb. 23, 2025
 *  version Nov. 21, 2025
 *  version Jun. 25, 2026
 * @version Jul. 13, 2026
 * @author  ASAMI, Tomoharu
 */
case class Bibliography(
  entries: Vector[Bibliography.Entry] = Vector.empty
) {
  def definitions: Vector[Bibliography.Entry] = entries

  def toHistory: History = {
    val slots = entries.flatMap(_.toHistorySlot)
    History(slots)
  }
}

object Bibliography {
  implicit val circeconf: Configuration = Configuration.default.withDefaults.withSnakeCaseMemberNames

  val empty = Bibliography()

  case class Identifiers(
    doi: Option[String] = None,
    isbn: Option[String] = None,
    issn: Option[String] = None,
    url: Option[String] = None,
    urn: Option[String] = None,
    arxiv: Option[String] = None,
    github: Option[String] = None,
    wikidata: Option[String] = None
  )
  object Identifiers {
    val empty: Identifiers = Identifiers()
    implicit val identifiersEncoder: Encoder.AsObject[Identifiers] = deriveConfiguredEncoder
  }

  case class Bibtex(
    entryType: Option[String] = None,
    sourceUrl: Option[String] = None,
    raw: Option[String] = None
  )
  object Bibtex {
    val empty: Bibtex = Bibtex()
    implicit val bibtexEncoder: Encoder.AsObject[Bibtex] = deriveConfiguredEncoder
  }

  case class SourceRef(
    sourcePath: String,
    publicPath: String,
    category: Option[String],
    citationKey: String,
    ordinal: Int
  )
  object SourceRef {
    implicit val sourceRefEncoder: Encoder.AsObject[SourceRef] = deriveConfiguredEncoder
  }

  case class Quality(
    missingCitation: Boolean = false,
    missingTerms: Boolean = false,
    missingSource: Boolean = false,
    missingNarrative: Boolean = false,
    bibtexOnly: Boolean = false,
    needsCuration: Boolean = false
  )
  object Quality {
    val empty: Quality = Quality()
    implicit val qualityEncoder: Encoder.AsObject[Quality] = deriveConfiguredEncoder
  }

  case class Entry(
    id: String,
    key: Option[String],
    slug: String,
    entryType: String,
    title: String,
    summary: Option[String],
    category: Option[String],
    sourcePath: String,
    publicPath: String,
    authors: Vector[String] = Vector.empty,
    publishedAt: Option[String] = None,
    publisher: Option[String] = None,
    sourceUrl: Option[String] = None,
    accessedAt: Option[String] = None,
    terms: Vector[String] = Vector.empty,
    tags: Vector[String] = Vector.empty,
    citation: Option[String] = None,
    identifiers: Identifiers = Identifiers.empty,
    bibtex: Bibtex = Bibtex.empty,
    bodyHtml: String = "",
    sourceKind: String = "internal",
    refs: Vector[String] = Vector.empty,
    sourceRefs: Vector[SourceRef] = Vector.empty,
    needsResolution: Boolean = false,
    quality: Quality = Quality.empty
  ) {
    def toHistorySlot: Vector[History.Slot] = Vector.empty
  }
  object Entry {
    implicit val entryEncoder: Encoder.AsObject[Entry] = deriveConfiguredEncoder
  }

  implicit val bibliographyEncoder: Encoder.AsObject[Bibliography] = deriveConfiguredEncoder

  def toJsonString(p: Bibliography): String =
    p.asJson.spaces2 + "\n"
}
