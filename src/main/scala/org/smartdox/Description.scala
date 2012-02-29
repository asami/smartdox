package org.smartdox

import java.net.URI
import javax.xml.datatype.XMLGregorianCalendar

/*
 * @since   Feb. 17, 2012
 *  version Feb. 17, 2012
 * @version Feb. 29, 2012
 * @author  ASAMI, Tomoharu
 */
case class Description(
    name: String,
    title: Dox = EmptyDox,
    summary: Dox = EmptyDox,
    content: Dox = EmptyDox,
    published: Option[XMLGregorianCalendar] = None,
    updated: Option[XMLGregorianCalendar] = None,
    id: Option[String] = None,
    links: List[URI] = Nil,
    categories: List[String] = Nil,
    authors: List[String] = Nil,
    contributors: List[String] = Nil,
    rights: Option[String] = None,
    source: Option[String] = None) {
}
/*
  var atomId: Option[String] = None
  var atomTitle: Option[String] = None
  var atomUpdated: Option[VDateTime] = None
  var atomPublished: Option[VDateTime] = None
  var atomCategories: List[String] = Nil
  var atomAuthors: List[String] = Nil
  var atomContributers: List[String] = Nil
  var atomRights: Option[String] = None
  var atomSource: Option[String] = None
  var atomSummary: Option[String] = None
  var atomContent: Option[String] = None
  var atomLinks: List[String] = Nil
*/