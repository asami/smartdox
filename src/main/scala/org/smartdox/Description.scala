package org.smartdox

import java.net.URI
import javax.xml.datatype.XMLGregorianCalendar
import org.goldenport.extension.IDocument
import org.goldenport.values.Designation

/*
 * @since   Feb. 17, 2012
 *  version Feb. 29, 2012
 *  version Jan. 20, 2014
 *  version Jan.  4, 2019
 *  version Aug. 15, 2020
 *  version Sep. 29, 2020
 *  version Oct. 18, 2020
 *  version Nov. 18, 2020
 * @version Dec. 27, 2020
 * @author  ASAMI, Tomoharu
 */
case class Description(
  designation: Option[Designation] = None,
  titleOption: Option[Dox] = None,
  resume: Resume = Resume.empty,
  content: Dox = EmptyDox,
  published: Option[XMLGregorianCalendar] = None,
  updated: Option[XMLGregorianCalendar] = None,
  id: Option[String] = None,
  links: List[URI] = Nil,
  categories: List[String] = Nil,
  authors: List[String] = Nil,
  contributors: List[String] = Nil,
  rights: Option[String] = None,
  source: Option[String] = None
) extends IDocument {
  def name: String = designation.map(_.name) getOrElse ""
  def title: Dox = designation.map(_.label).map(Dox.text) getOrElse EmptyDox
  def summary = resume.effectiveSummary
  def effectiveSummary = resume.effectiveSummary
  //  def effectiveBrief = resume.effectiveBrief
  def brief = resume.brief
  def caption = resume.caption

  def withName(p: String) = copy(designation = Some(Designation(p)))
  def withDesignation(p: Designation) = copy(designation = Some(p))
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

object Description {
  val empty = Description()

  trait Holder extends Resume.Holder {
  }

  def apply(name: Designation): Description = Description(Some(name))

  def apply(name: Designation, p: Dox): Description = Description(Some(name), content = p)

  def apply(p: Dox): Description = Description(content = p)

  def name(name: String): Description = Description(Some(Designation(name)))

  def name(name: String, p: Dox): Description = Description(Some(Designation(name)), content = p)
}
