package org.smartdox

import scalaz._
import Scalaz._
import java.net.URI
import javax.xml.datatype.XMLGregorianCalendar

/*
 * @since   Feb. 17, 2012
 * @version Feb. 28, 2012
 * @author  ASAMI, Tomoharu
 */
class DescriptionVar {
  var name: String = ""
  var title: Dox = EmptyDox
  var summary: Dox = EmptyDox
  var content: Dox = EmptyDox
  var published: Option[XMLGregorianCalendar] = None
  var updated: Option[XMLGregorianCalendar] = None
  var id: Option[String] = None
  var links: List[URI] = Nil
  var categories: List[String] = Nil
  var authors: List[String] = Nil
  var contributors: List[String] = Nil
  var rights: Option[String] = None
  var source: Option[String] = None

  def titleString: String = title.toText
  def summaryString: String = summary.toText
  def contentString: String = content.toText
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