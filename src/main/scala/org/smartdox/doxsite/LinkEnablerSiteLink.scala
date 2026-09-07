package org.smartdox.doxsite

import java.net.URI
import org.goldenport.context.Consequence
import org.smartdox.metadata.DocumentMetaData

/*
 * @since   Sep.  7, 2026
 * @version Sep.  7, 2026
 * @author  ASAMI, Tomoharu
 */
private[doxsite] object LinkEnablerSiteLink {
  def resolve(
    sourcePath: String,
    target: URI,
    lookup: String => Option[DocumentMetaData]
  ): Consequence[SitePublicationContext.ResolvedSiteLink] =
    SitePublicationContext.resolveSiteLink(sourcePath, target, lookup)
}
