package org.smartdox.doxsite

import org.goldenport.tree.TreeNode
import org.goldenport.util.StringUtils
import org.goldenport.values.PathName
import org.smartdox.Document

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
private[smartdox] object DoxSiteEffectiveContent {
  final case class EffectiveContent(
    logicalPath: PathName,
    sourcePath: PathName,
    page: Page
  ) {
    def getDox: Document = page.dox
  }

  def effectiveContent(node: TreeNode[Node]): Option[EffectiveContent] =
    node.getContent match {
      case Some(page: Page) if _is_document_project_index(node.pathname) =>
        None
      case Some(page: Page) =>
        Some(EffectiveContent(PathName(effectivePath(node.pathnameRelative)), PathName(node.pathnameRelative), page))
      case _ if node.name.endsWith(".dox") =>
        node.children.find(_.name == "index.dox").flatMap { child =>
          child.getContent.collect {
            case page: Page =>
              EffectiveContent(PathName(effectivePath(node.pathnameRelative)), PathName(child.pathnameRelative), page.copy(name = Node.Name(node.name)))
          }
        }
      case _ =>
        None
    }

  def effectivePath(path: String): String = {
    val normalized = path.replace('\\', '/')
    val prefix = if (normalized.startsWith("/")) "/" else ""
    val segments = normalized.split('/').toVector.filter(_.nonEmpty)
    val effectivesegments = segments match {
      case xs if xs.size >= 2 && xs.last == "index.dox" && xs(xs.size - 2).endsWith(".dox") =>
        xs.dropRight(1)
      case xs =>
        xs
    }
    prefix + effectivesegments.mkString("/")
  }

  def effectivePath(basePath: String, targetPath: String): String = {
    val normalizedbase = basePath.replace('\\', '/')
    val resolvablebase =
      if (normalizedbase.isEmpty || normalizedbase.contains("/") || normalizedbase.endsWith("/"))
        normalizedbase
      else
        s"./$normalizedbase"
    val resolved = StringUtils.resolvePath(resolvablebase, targetPath)
    val doxpath =
      if (resolved.endsWith(".html"))
        StringUtils.changeSuffix(resolved, "dox")
      else
        resolved
    effectivePath(doxpath)
  }

  private def _is_document_project_index(path: String): Boolean = {
    val segments = path.stripPrefix("/").split('/').toVector.filter(_.nonEmpty)
    segments.size >= 2 && segments.last == "index.dox" && segments(segments.size - 2).endsWith(".dox")
  }
}
