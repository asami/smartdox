package org.smartdox.metadata

import scala.xml._
import java.time.Instant
import java.time.format.DateTimeFormatter
import java.time.ZoneOffset
import org.goldenport.i18n.I18NString
import org.smartdox._

/*
 * @since   Jul. 22, 2025
 * @version Jul. 24, 2025
 * @author  ASAMI, Tomoharu
 */
case class AtomFeed(
  id: String,
  title: String,
  updated: Instant,
  author: Option[AtomFeed.Person] = None,
  contributors: List[AtomFeed.Person] = Nil,
  links: List[AtomFeed.Link] = Nil,
  categories: List[AtomFeed.Category] = Nil,
  generator: Option[AtomFeed.Generator] = None,
  icon: Option[String] = None,
  logo: Option[String] = None,
  rights: Option[String] = None,
  subtitle: Option[String] = None,
  entries: List[AtomFeed.Entry] = Nil
) {
  def nonEmpty = entries.nonEmpty

  def toAtomString: String = {
    val xml: Elem = toAtomXml
    new PrettyPrinter(120, 2).format(xml)
  }

  def toAtomXml: Elem = {
    val feed = this
    val dateFmt = DateTimeFormatter.ISO_INSTANT

    <feed xmlns="http://www.w3.org/2005/Atom">
    <id>{feed.id}</id>
    <title>{feed.title}</title>
    <updated>{dateFmt.format(feed.updated)}</updated>
    {
      feed.author.map { a =>
        <author>
        <name>{a.name}</name>
        {a.uri.map(uri => <uri>{uri}</uri>).getOrElse(NodeSeq.Empty)}
        {a.email.map(email => <email>{email}</email>).getOrElse(NodeSeq.Empty)}
        </author>
      }.getOrElse(NodeSeq.Empty)
    }
    {
      feed.links.map { link =>
        <link href={link.href}
        rel={link.rel.orNull}
        type={link.`type`.orNull}
        hreflang={link.hreflang.orNull}
        title={link.title.orNull}
        length={link.length.map(_.toString).orNull} />
      }
    }
    {
      feed.entries.map { e =>
        <entry>
        <id>{e.id}</id>
        <title>{e.title}</title>
        <updated>{dateFmt.format(e.updated)}</updated>
        {e.published.map(p => <published>{dateFmt.format(p)}</published>).getOrElse(NodeSeq.Empty)}
        {
          e.summary.map(s => <summary>{s}</summary>).getOrElse(NodeSeq.Empty)
        }
        {
          e.links.map(l => <link href={l.href}/> )
        }
        {
          e.content.map { c =>
            <content type={c.`type`}>
            {c.value.getOrElse("")}
            </content>
          }.getOrElse(NodeSeq.Empty)
        }
        </entry>
      }
    }
    </feed>
  }
}

object AtomFeed {
  case class Entry(
    id: String,
    title: String,
    updated: Instant,
    published: Option[Instant] = None,
    author: Option[Person] = None,
    contributors: List[Person] = Nil,
    links: List[Link] = Nil,
    categories: List[Category] = Nil,
    content: Option[Content] = None,
    summary: Option[String] = None,
    rights: Option[String] = None,
    source: Option[AtomFeed] = None
  )

  case class Person(
    name: String,
    uri: Option[String] = None,
    email: Option[String] = None
  )

  case class Link(
    href: String,
    rel: Option[String] = None,
    `type`: Option[String] = None,
    hreflang: Option[String] = None,
    title: Option[String] = None,
    length: Option[Long] = None
  )

  case class Category(
    term: String,
    scheme: Option[String] = None,
    label: Option[String] = None
  )

  case class Generator(
    text: String,
    uri: Option[String] = None,
    version: Option[String] = None
  )

  case class Content(
    `type`: String, // e.g., "text", "html", "xhtml"
    value: Option[String] = None,
    src: Option[String] = None
  )
}

case class AtomFeedBag(
  ja: AtomFeed,
  en: AtomFeed
)

