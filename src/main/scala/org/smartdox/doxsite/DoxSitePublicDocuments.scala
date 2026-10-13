package org.smartdox.doxsite

import org.goldenport.realm.Realm
import org.goldenport.tree.{ContentTreeVisitor, TreeNode}
import org.goldenport.util.StringUtils

/*
 * @since   Oct.  7, 2026
 * @version Oct.  7, 2026
 */
object DoxSitePublicDocuments {
  final case class Source(sourcePath: String, publicPath: String)

  /** Structural admission only; parsed strategy/status selection remains in DoxSite. */
  def sources(realm: Realm, config: DoxSite.Config = DoxSite.Config.default): Vector[Source] = {
    val rule = DoxSiteBuilder.Rule(config)
    val selected = Vector.newBuilder[Source]
    realm.backend.traverse(new ContentTreeVisitor[Realm.Data] {
      override protected def start_Content(node: TreeNode[Realm.Data], content: Realm.Data): Unit =
        _collect(node, content)

      override protected def enter_Content(node: TreeNode[Realm.Data], content: Realm.Data): Unit =
        _collect(node, content)

      private def _collect(node: TreeNode[Realm.Data], content: Realm.Data): Unit =
        content match {
          case _: Realm.StringData if node.getNameSuffix.exists(Set("dox", "org", "md", "markdown")) &&
            !_ignored(node) =>
            rule.getTargetName(node).foreach { target =>
              val sourcepath = node.pathnameRelative
              val selectedpath = if (sourcepath.indexOf('/') < 0) target else StringUtils.changeLeafRelative(sourcepath, target)
              selected += Source(sourcepath, DoxSiteEffectiveContent.publicPath(selectedpath))
            }
          case _ => ()
        }

      private def _ignored(node: TreeNode[Realm.Data]): Boolean =
        rule.isIgnore(node) || node.getParent.exists(_ignored)
    })
    selected.result().sortBy(source => (source.sourcePath, source.publicPath))
  }
}
