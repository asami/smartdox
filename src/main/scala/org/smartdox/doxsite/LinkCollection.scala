package org.smartdox.doxsite

import java.net.URI
import org.goldenport.RAISE
import org.goldenport.tree._
import org.goldenport.values.PathName
import org.goldenport.i18n.I18NHangar
import org.goldenport.i18n.I18NString
import org.goldenport.io.UriUtils
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.doxsite.LinkEnabler.LinkEmbedder.LinkHolder
import org.smartdox.doxsite.LinkEnabler.LinkEmbedder.Link
import org.smartdox.doxsite.LinkCollector.SiteScanner.Scanner.FigureHolder
import org.smartdox.doxsite.LinkCollector.SiteScanner.Scanner.TableHolder
import org.smartdox.doxsite.LinkCollector.SiteScanner.Scanner.ProgramHolder

/*
 * @since   Nov. 14, 2025
 * @version Nov. 20, 2025
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
      val a = PathName.getRelativePath(newsource, source)
      val uri = new URI(a.v)
      val tooltip = doc.getEffectiveTooltip
      doc.title.map(_make_link(uri, _, tooltip)) getOrElse Hyperlink.createArticle(source.toString)
    }

    private def _make_link(uri: URI, p: I18NFragment, tooltip: Option[I18NString]): I18NFragment =
      p.mapValues(_make_link(uri, _, tooltip))

    private def _make_link(uri: URI, ps: List[Dox], tooltip: Option[I18NString]): List[Hyperlink] = {
      val a = Dox.toInlineContents(Text(_link_mark) :: ps)
      val to = UriUtils.changeSuffix(uri, "html")
      List(Hyperlink.createArticle(a, to, tooltip))
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
    def +(p: IncomingLinkHolder): IncomingLinkHolder = copy(links = p.links)

    def addDirect(source: PathName, doc: DocumentMetaData, p: Link): IncomingLinkHolder = copy(links = links :+ IncomingLink.direct(source, doc, p.hyperlinks))

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
      node: TreeNode[Node],
      dox: Dox,
      internallinks: LinkHolder,
      externallinks: LinkHolder,
      figures: FigureHolder,
      tables: TableHolder,
      programs: ProgramHolder
    ): Unit = {
      val dl = DoxLinks(dox, internallinks, externallinks, figures, tables)
      _set(node.pathname, DoxLinks.Candidate.Complete(dl))
      for (x <- internallinks.links) {
        _set_internallink(node, dox, x)
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
      node: TreeNode[Node],
      dox: Dox,
      p: Link
    ): Unit = {
      val pathname = StringUtils.resolvePath(node.pathname, p.href.toString)
      val doc = Dox.toDocument(dox).head.metadata
      val x = tree.getContent(pathname) match {
        case Some(s) => s.addIncoming(node.pathnameValue, doc, p)
        case None =>
          val doc = Dox.toDocument(dox).head.metadata
          DoxLinks.Candidate.Incoming.create(node.pathnameValue, doc, p)
      }
      tree.setContent(pathname, x)
    }
  }
  object Builder {
  }
}
