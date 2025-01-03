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
 *  version Dec. 27, 2020
 *  version Mar. 16, 2021
 *  version Jun. 28, 2021
 *  version Sep. 20, 2023
 *  version Jul.  7, 2024
 * @version Nov. 13, 2024
 * @author  ASAMI, Tomoharu
 */
case class Description(
  designation: Designation = Designation.empty,
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
) extends IDocument with Designation.Holder {
  def title: Dox = titleOption getOrElse Dox.text(designation.label)
  def summary = resume.effectiveSummary
  def effectiveSummary = resume.effectiveSummary
  //  def effectiveBrief = resume.effectiveBrief
  def brief = resume.brief
  def caption = resume.caption

  def print = toString

  def withName(p: String) = withDesignation(Designation(p))
  def withDesignation(p: Designation) = copy(designation = p)
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

  trait Holder extends Designation.Holder with Resume.Holder {
    def description: Description
    def designation = description.designation
    def resume = description.resume
  }

  def apply(name: Designation, p: Dox): Description = Description(name, content = p)

  def apply(p: Dox): Description = Description(content = p)

  def name(name: String): Description = Description(Designation(name))

  def name(name: String, p: Dox): Description = Description(Designation(name), content = p)

  def nameParagraph(name: String, p: String): Description = Description(Designation(name), Paragraph.text(p))

  def parse(p: Dox): Description = p match {
    case m: Section => m.distillDescription
    case m => apply(p)
  }
}
