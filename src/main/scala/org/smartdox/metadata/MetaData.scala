package org.smartdox.metadata

import org.smartdox._

/*
 * @since   Feb. 23, 2025
 *  version Mar.  4, 2025
 *  version Apr. 29, 2025
 *  version Jun. 23, 2025
 * @version Jul. 23, 2025
 * @author  ASAMI, Tomoharu
 */
case class MetaData(
  index: Index = Index.empty,
  glossary: Glossary = Glossary.empty,
  references: References = References.empty,
  categories: CategoryCollection = CategoryCollection.empty,
  keywords: KeywordCollection = KeywordCollection.empty,
  tags: TagCollection = TagCollection.empty,
  notices: Notices = Notices.empty,
  atomFeed: Option[AtomFeedBag] = None
)

object MetaData {
  val empty = MetaData()
}
