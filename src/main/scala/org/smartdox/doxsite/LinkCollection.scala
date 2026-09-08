package org.smartdox.doxsite

import java.net.URI
import org.goldenport.RAISE
import org.goldenport.tree._
import org.goldenport.values.PathName
import org.goldenport.i18n.I18NHangar
import org.goldenport.i18n.I18NString
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.doxsite.LinkEnabler.LinkEmbedder.LinkHolder
import org.smartdox.doxsite.LinkEnabler.LinkEmbedder.Link
import org.smartdox.doxsite.LinkCollector.SiteScanner.Scanner.FigureHolder
import org.smartdox.doxsite.LinkCollector.SiteScanner.Scanner.TableHolder
import org.smartdox.doxsite.LinkCollector.SiteScanner.Scanner.ProgramHolder

/*
 * @since   Nov. 14, 2025
 *  version Nov. 22, 2025
 *  version Dec. 16, 2025
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
case class LinkCollection(
  tree: Tree[LinkCollection.DoxLinks]
) {
  def get(pathname: String): Option[LinkCollection.DoxLinks] = tree.getContent(pathname)
}

object LinkCollection {
  case class IncomingLink(
    kind: IncomingLink.Kind,
    source: PathName,
    doc: DocumentMetaData,
    links: I18NHangar[Hyperlink]
  ) {
    private val _link_mark = DoxSite.Config.WorkAround.textMark.article

    def toListContent(newsource: PathName): ListContent = {
      val uri = new URI(DoxSite.relativePublicPath(newsource.v, source.v))
      val tooltip = doc.getEffectiveTooltip
      doc.title.map(_make_link(uri, _, tooltip)) getOrElse Hyperlink.createArticle(source.toString)
    }

    private def _make_link(uri: URI, p: I18NFragment, tooltip: Option[I18NString]): I18NFragment =
      p.mapValues(_make_link(uri, _, tooltip))

    private def _make_link(uri: URI, ps: List[Dox], tooltip: Option[I18NString]): List[Hyperlink] = {
      val a = Dox.toInlineContents(Text(_link_mark) :: ps)
      List(Hyperlink.createArticle(a, uri, tooltip, source))
    }
  }
  object IncomingLink {
    sealed trait Kind
    object Kind {
      case object Direct extends Kind
      case object Supercede extends Kind
    }

    def direct(pathname: PathName, doc: DocumentMetaData, links: I18NHangar[Hyperlink]) = IncomingLink(Kind.Direct, pathname, doc, links)
  }

  case class IncomingLinkHolder(
    links: Vector[IncomingLink] = Vector.empty
  ) {
    def +(p: IncomingLinkHolder): IncomingLinkHolder = copy(links = links ++ p.links)

    def addDirect(source: PathName, doc: DocumentMetaData, p: Link): IncomingLinkHolder = copy(links = links :+ IncomingLink.direct(source, doc, p.hyperlinks))

    def filterNot(p: LinkHolder): IncomingLinkHolder = {
      val excludes: Set[String] = p.links.flatMap { link =>
        link.pathname.map { base =>
          val effectivebase = DoxSiteEffectiveContent.effectivePath(base.v)
          DoxSiteEffectiveContent.effectivePath(effectivebase, link.href.toString)
        }
      }.toSet
      def _is_match_(link: IncomingLink): Boolean =
        excludes.contains(link.source.v)
      copy(links = links.filterNot(_is_match_))
    }


    def toListContents(newsource: PathName): Vector[ListContent] =
      links.map(_.toListContent(newsource))
  }
  object IncomingLinkHolder {
    val empty = IncomingLinkHolder()

    def createDirect(source: PathName, doc: DocumentMetaData, p: Link): IncomingLinkHolder = IncomingLinkHolder(
      Vector(IncomingLink.direct(source, doc, p.hyperlinks))
    )
  }

  case class DoxLinks(
    dox: Dox,
    internalLinks: LinkHolder = LinkHolder.empty,
    externalLinks: LinkHolder = LinkHolder.empty,
    glossaryLinks: LinkHolder = LinkHolder.empty,
    figures: FigureHolder = FigureHolder.empty,
    tables: TableHolder = TableHolder.empty,
    programs: ProgramHolder = ProgramHolder.empty,
    incomingLinks: IncomingLinkHolder = IncomingLinkHolder.empty
  ) {
    def addIncomingLinks(p: IncomingLinkHolder) = copy(incomingLinks = incomingLinks + p)
    def addIncomingLink(source: PathName, doc: DocumentMetaData, p: Link) = copy(incomingLinks = incomingLinks.addDirect(source, doc, p))
  }
  object DoxLinks {
    sealed trait Candidate {
      def addIncoming(source: PathName, doc: DocumentMetaData, p: Link): Candidate
    }
    object Candidate {
      case class Complete(doxlinks: DoxLinks) extends Candidate {
        def add(p: Incoming): Complete = copy(doxlinks = doxlinks.addIncomingLinks(p.incoming))
        def addIncoming(source: PathName, doc: DocumentMetaData, p: Link): Complete = copy(doxlinks = doxlinks.addIncomingLink(source, doc, p))
      }
      case class Incoming(incoming: IncomingLinkHolder) extends Candidate {
        def add(p: Incoming): Incoming = copy(incoming = incoming + p.incoming)
        def addIncoming(source: PathName, doc: DocumentMetaData, p: Link): Incoming = copy(incoming = incoming.addDirect(source, doc, p))
      }
      object Incoming {
        def create(source: PathName, doc: DocumentMetaData, p: Link): Incoming = Incoming(IncomingLinkHolder.createDirect(source, doc, p))
      }
    }
  }

  class Builder() {
    val tree = Tree.create[DoxLinks.Candidate]()

    def build(): LinkCollection = {
      val a = tree.filterMap(_to_doxlinks)
      LinkCollection(a)
    }

    private def _to_doxlinks: PartialFunction[DoxLinks.Candidate, DoxLinks] = {
      case DoxLinks.Candidate.Complete(doxlinks) => doxlinks
    }

    def add(
      logicalPath: PathName,
      dox: Dox,
      internallinks: LinkHolder,
      externallinks: LinkHolder,
      glossarylinks: LinkHolder,
      figures: FigureHolder,
      tables: TableHolder,
      programs: ProgramHolder
    ): Unit = {
      val dl = DoxLinks(dox, internallinks, externallinks, glossarylinks, figures, tables)
      _set(logicalPath.v, DoxLinks.Candidate.Complete(dl))
      for (x <- internallinks.links) {
        _set_internallink(logicalPath, dox, x)
      }
    }

    private def _set(pathname: String, p: DoxLinks.Candidate): Unit = {
      val x = tree.getContent(pathname) match {
        case Some(s) => s match {
          case m: DoxLinks.Candidate.Incoming => p match {
            case mm: DoxLinks.Candidate.Incoming => m.add(mm)
            case mm: DoxLinks.Candidate.Complete => mm.add(m)
          }
          case _ => RAISE.noReachDefect
        }
        case None => p
      }
      tree.setContent(pathname, x)
    }

    private def _set_internallink(
      logicalpath: PathName,
      dox: Dox,
      p: Link
    ): Unit = {
      val pathname = DoxSiteEffectiveContent.effectivePath(logicalpath.v, p.href.toString)
      val doc = Dox.toDocument(dox).head.metadata
      val x = tree.getContent(pathname) match {
        case Some(s) => s.addIncoming(logicalpath, doc, p)
        case None =>
          DoxLinks.Candidate.Incoming.create(logicalpath, doc, p)
      }
      tree.setContent(pathname, x)
    }

  }
  object Builder {
  }
}
