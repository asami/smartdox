package org.smartdox.metadata

import org.smartdox._

/*
 * @since   Feb. 23, 2025
 *  version Feb. 23, 2025
 * @version Mar.  4, 2025
 * @author  ASAMI, Tomoharu
 */
case class MetaData(
  index: Index = Index.empty,
  glossary: Glossary = Glossary.empty,
  references: References = References.empty
)

object MetaData {
  val empty = MetaData()
}
