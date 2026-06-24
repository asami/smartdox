package org.smartdox.metadata

import org.smartdox._
import org.smartdox.doxsite.LinkCollection
import org.smartdox.semanticweb.Site.SiteModel

/*
 * @since   Feb. 23, 2025
 *  version Mar.  4, 2025
 *  version Apr. 29, 2025
 *  version Jun. 23, 2025
 *  version Jul. 23, 2025
 *  version Aug. 24, 2025
 *  version Nov. 22, 2025
 * @version Jun. 24, 2026
 * @author  ASAMI, Tomoharu
 */
case class MetaData(
  index: Index = Index.empty,
  glossary: Glossary = Glossary.empty,
  bibliography: Bibliography = Bibliography.empty,
  references: References = References.empty,
  categories: CategoryCollection = CategoryCollection.empty,
  keywords: KeywordCollection = KeywordCollection.empty,
  tags: TagCollection = TagCollection.create(),
  notices: Notices = Notices.empty,
  atomFeed: Option[AtomFeedBag] = None,
  history: History = History.empty,
  dashboard: DoxSiteDashboard = DoxSiteDashboard.empty,
  linkCollection: Option[LinkCollection] = None,
  site: SiteModel = SiteModel.empty
)

object MetaData {
  val empty = MetaData()
}
