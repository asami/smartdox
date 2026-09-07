package org.smartdox.parser

import java.net.URI
import org.goldenport.collection.VectorMap
import org.goldenport.parser.ParseLocation
import org.smartdox.{Dox, Hyperlink, Text}

/*
 * @since   Sep.  7, 2026
 * @version Sep.  7, 2026
 * @author  ASAMI, Tomoharu
 */
private[parser] object DoxInlineParserSiteLink {
  def create(contents: String, location: Option[ParseLocation]): Hyperlink =
    Dox.attachLocation(
      Hyperlink(List(Text(contents)), new URI(contents), None, VectorMap("class" -> "site")),
      location
    ).asInstanceOf[Hyperlink]
}
