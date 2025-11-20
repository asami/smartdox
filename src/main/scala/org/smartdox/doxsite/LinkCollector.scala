package org.smartdox.doxsite

import java.net.URI
import java.util.Locale
import org.goldenport.tree._
import org.goldenport.values.PathName
import org.smartdox._
import org.smartdox.converter.DoxTreeScanner
import org.smartdox.doxsite.LinkEnabler.LinkEmbedder.LinkHolder

/*
 * @since   Nov. 14, 2025
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
class LinkCollector(
  config: DoxSite.Config
) {
  import LinkCollector._

  def apply(p: Tree[Node]): LinkCollection = {
    val scanner = new SiteScanner()
    p.traverse(scanner)
    val a = scanner.collection
    val builder = new Builder(a)
    p.traverse(builder)
    builder.collection
  }
}

object LinkCollector {
  class SiteScanner() extends DoxSiteVisitor {
    import SiteScanner._

    def collection: LinkCollection = _builder.build()

    private val _builder: LinkCollection.Builder = new LinkCollection.Builder()

    override protected def enter_Content(node: TreeNode[Node], content: Node): Unit = {
      content match {
        case m: Page => _scan_link(node, node.pathnameValue, m)
        case _ => {}
      }
    }

    private def _scan_link(node: TreeNode[Node], pathname: PathName, p: Page): Unit = {
      val dox = p.dox
      val scanner = new Scanner()
      dox.traverse(scanner)
      _builder.add(
        node,
        dox,
        scanner.internalLinks,
        scanner.externalLinks,
        scanner.figures,
        scanner.tables,
        scanner.programs
      )
    }
  }
  object SiteScanner {
    class Scanner() extends DoxTreeScanner {
      import Scanner._

      private var _internal_links: LinkHolder = LinkHolder.empty
      private var _external_links: LinkHolder = LinkHolder.empty
      private var _figures: FigureHolder = FigureHolder.empty
      private var _tables: TableHolder = TableHolder.empty
      private var _programs: ProgramHolder = ProgramHolder.empty

      def internalLinks = _internal_links
      def externalLinks = _external_links
      def figures = _figures
      def tables = _tables
      def programs = _programs

      private def _add_internal_link(p: Hyperlink) = {
        _internal_links = _internal_links.add(get_locale, p)
      }

      private def _add_external_link(p: Hyperlink) = {
        _external_links = _external_links.add(get_locale, p)
      }

      override protected def enter_Hyperlink(p: Hyperlink): Unit = {
        if (p.isLocalOrRelative)
          _scan_link(p, p.href)
        else
          _external_link(p)
      }

      private def _scan_link(
        dox: Hyperlink,
        href: URI
      ): Unit = {
        _add_internal_link(dox)
      }

      private def _external_link(dox: Hyperlink) = {
        _add_external_link(dox)
      }

      override protected def enter_Figure(p: Figure): Unit = {
        val caption = p.caption
        if (caption.nonEmpty)
          _figures = _figures.add(get_locale, p, caption)
      }

      override protected def enter_Table(p: Table): Unit = {
        p.caption.foreach { caption =>
          _tables = _tables.add(get_locale, p, caption)
        }
      }

      override protected def enter_Program(p: Program): Unit = {
        p.caption.foreach { c =>
          val caption = Caption(c)
          _programs = _programs.add(get_locale, p, caption)
        }
      }
    }
    object Scanner {
      case class FigureHolder(
        figures: Vector[FigureHolder.Slot] = Vector.empty
      ) {
        import FigureHolder._

        def add(locale: Option[Locale], p: Figure, c: Figcaption): FigureHolder = locale match {
          case Some(s) => add(s, p, c)
          case None => add(p, c)
        }

        def add(locale: Locale, p: Figure, c: Figcaption): FigureHolder =
          copy(figures = figures :+ Slot(Some(locale), p, c))
        def add(p: Figure, c: Figcaption): FigureHolder =
          copy(figures = figures :+ Slot(None, p, c))
      }
      object FigureHolder {
        val empty = FigureHolder()

        case class Slot(locale: Option[Locale], figure: Figure, caption: Figcaption)
      }

      case class TableHolder(
        tables: Vector[TableHolder.Slot] = Vector.empty
      ) {
        import TableHolder._

        def add(locale: Option[Locale], p: Table, c: Caption): TableHolder = locale match {
          case Some(s) => add(s, p, c)
          case None => add(p, c)
        }
        def add(locale: Locale, p: Table, c: Caption): TableHolder =
          copy(tables = tables :+ Slot(Some(locale), p, c))
        def add(p: Table, c: Caption): TableHolder =
          copy(tables = tables :+ Slot(None, p, c))
      }
      object TableHolder {
        val empty = TableHolder()

        case class Slot(locale: Option[Locale], table: Table, caption: Caption)
      }

      case class ProgramHolder(
        programs: Vector[ProgramHolder.Slot] = Vector.empty
      ) {
        import ProgramHolder._

        def add(locale: Option[Locale], p: Program, c: Caption): ProgramHolder = locale match {
          case Some(s) => add(s, p, c)
          case None => add(p, c)
        }
        def add(locale: Locale, p: Program, c: Caption): ProgramHolder =
          copy(programs = programs :+ Slot(Some(locale), p, c))
        def add(p: Program, c: Caption): ProgramHolder =
          copy(programs = programs :+ Slot(None, p, c))
      }
      object ProgramHolder {
        val empty = ProgramHolder()

        case class Slot(locale: Option[Locale], program: Program, caption: Caption)
      }
    }
  }

  class Builder(
    val scaned: LinkCollection
  ) extends DoxSiteVisitor {
    def collection: LinkCollection = scaned
  }
}
