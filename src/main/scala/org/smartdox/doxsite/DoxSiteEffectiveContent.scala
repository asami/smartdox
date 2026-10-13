package org.smartdox.doxsite

import java.net.URI
import org.goldenport.realm.Realm
import org.goldenport.tree.{Tree, TreeNode}
import org.goldenport.util.StringUtils
import org.goldenport.values.PathName
import org.smartdox.Document

/*
 * @since   Sep.  8, 2026
 * @version Oct.  6, 2026
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

  def isPublicDocument(node: TreeNode[Realm.Data]): Boolean =
    node.getNameSuffix match {
      case Some("dox" | "org" | "md" | "markdown") =>
        _outermost_document_project(node.getParent) match {
          case Some(project) => project.getChild("index.dox").contains(node)
          case None => true
        }
      case _ => true
    }

  private def _outermost_document_project(node: Option[TreeNode[Realm.Data]]): Option[TreeNode[Realm.Data]] =
    node.flatMap { current =>
      _outermost_document_project(current.getParent).orElse {
        if (_is_document_project(current)) Some(current) else None
      }
    }

  private def _is_document_project(node: TreeNode[Realm.Data]): Boolean =
    node.isContainer &&
      node.name.endsWith(".dox") &&
      node.getChild("index.dox").exists(_.getContent.isDefined)

  def effectiveContent(tree: Tree[Node], path: String): Option[EffectiveContent] = {
    val normalized = path.replace('\\', '/')
    val legacy = tree.getNode(effectivePath("", normalized)).flatMap(node => effectiveContent(node))
    legacy.orElse {
      if (normalized.endsWith(".html")) {
        val resolved = effectivePath(StringUtils.resolvePath("./", normalized))
        Vector("md", "markdown").toStream.flatMap { suffix =>
          tree.getNode(StringUtils.changeSuffix(resolved, suffix)).flatMap(node => effectiveContent(node))
        }.headOption
      } else {
        None
      }
    }
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
    val resolved = _resolve_path(basePath, targetPath)
    val doxpath =
      if (resolved.endsWith(".html"))
        StringUtils.changeSuffix(resolved, "dox")
      else
        resolved
    effectivePath(doxpath)
  }

  private[doxsite] def relationPath(tree: Tree[Node], basePath: String, target: URI): String = {
    val base = effectivePath(basePath)
    val targetpath = Option(target.getPath).getOrElse("")
    val resolved = if (targetpath.isEmpty) base else _resolve_path(base, targetpath)
    effectiveContent(tree, resolved).map(_.logicalPath.v).getOrElse(effectivePath("", resolved))
  }

  private def _resolve_path(basepath: String, targetpath: String): String = {
    val normalizedbase = basepath.replace('\\', '/')
    val resolvablebase =
      if (normalizedbase.isEmpty)
        "./"
      else if (normalizedbase.contains("/") || normalizedbase.endsWith("/"))
        normalizedbase
      else
        s"./$normalizedbase"
    StringUtils.resolvePath(resolvablebase, targetpath)
  }

  def publicPath(path: String): String = {
    val normalized = path.replace('\\', '/')
    val prefix = if (normalized.startsWith("/")) "/" else ""
    val segments = normalized.split('/').toVector.filter(_.nonEmpty)
    val publicsegments = segments match {
      case xs if xs.size >= 2 &&
        (xs.last == "index.dox" || xs.last == "index.html") &&
        xs(xs.size - 2).endsWith(".dox") =>
        xs.dropRight(2) :+ s"${xs(xs.size - 2).stripSuffix(".dox")}.html"
      case xs if xs.lastOption.exists(name => Vector(".dox", ".md", ".markdown").exists(name.endsWith)) =>
        xs.dropRight(1) :+ StringUtils.changeSuffix(xs.last, "html")
      case xs => xs
    }
    prefix + publicsegments.mkString("/")
  }

  private def _is_document_project_index(path: String): Boolean = {
    val segments = path.stripPrefix("/").split('/').toVector.filter(_.nonEmpty)
    segments.size >= 2 && segments.last == "index.dox" && segments(segments.size - 2).endsWith(".dox")
  }
}
