package org.smartdox.doxsite

import org.goldenport.realm.Realm
import org.smartdox._
import org.goldenport.values.{PathName => LibPathName}

/*
 * @since   Feb. 25, 2025
 * @version Mar.  9, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait Node {
  def name: Node.Name
}

object Node {
  case class Name(name: String)
  object Name {
    val root = Name("")
  }

  case class PathName(pathname: LibPathName)
}

case class Page(
  name: Node.Name,
  dox: Dox
) extends Node {
  def pageId: Page.Id = ???

  def toRealmData: Realm.Data = Realm.StringData(dox.toString)
}

object Page {
  case class Id()

  def apply(name: String, dox: Dox): Page = Page(Node.Name(name), dox)
}
