package org.smartdox.doxsite

import org.goldenport.RAISE
import org.goldenport.realm.Realm
import org.goldenport.values.{PathName => LibPathName}
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.generator.Context

/*
 * @since   Feb. 25, 2025
 *  version Mar.  9, 2025
 *  version Apr. 30, 2025
 * @version Jun. 16, 2025
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
  def pageId: Page.Id = RAISE.notImplementedYetDefect

  lazy val title: String = Dox.getTitleString(dox) getOrElse "Unknown"
  def getMetadata: Option[DocumentMetaData] = dox match {
    case m: Document => Some(m.head.metadata)
    case m: Head => Some(m.metadata)
    case _ => None
  }

  def toRealmData: Realm.Data = Realm.StringData(dox.toString)
}

object Page {
  case class Id()

  def apply(name: String, dox: Dox): Page = Page(Node.Name(name), dox)
}
