package org.smartdox.doxsite

import java.net.URI
import java.io.File
import java.time.Instant
import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.realm.Realm
import org.goldenport.values.{PathName => LibPathName}
import org.smartdox._
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.metadata.Category
import org.smartdox.metadata.DoxCacheControl
import org.smartdox.generator.Context

/*
 * @since   Feb. 25, 2025
 *  version Mar.  9, 2025
 *  version Apr. 30, 2025
 *  version Jun. 24, 2025
 * @version Jul. 26, 2025
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
  dox: Dox,
  lastModified: Option[Instant] = None
) extends Node {
  def pageId: Page.Id = RAISE.notImplementedYetDefect

  lazy val titleDefault: String = Dox.distillTitleStringDefault(dox) getOrElse "Unknown"

  def getMetadata: Option[DocumentMetaData] =
    getHead.map(_.metadata)

  // def getDoxCacheControl: Option[DoxCacheControl] =
  //   getHead.flatMap(_.doxCacheControl)

  def getHead: Option[Head] = dox match {
    case m: Document => Some(m.head)
    case m: Head => Some(m)
    case _ => None
  }

  def withDox(p: Dox) = copy(dox = p)
  def withlastModified(p: Option[Instant]) = copy(lastModified = p)

  def toRealmData: Realm.Data = Realm.StringData(dox.toString, lastModified)
}

object Page {
  case class Id()

  def apply(
    name: String,
    dox: Dox
  ): Page = Page(Node.Name(name), dox)

  def apply(
    name: String,
    dox: Dox,
    lastmodified: Long
  ): Page = Page(Node.Name(name), dox, Some(Instant.ofEpochMilli(lastmodified)))

  def apply(
    name: String,
    dox: Dox,
    lastmodified: Option[Instant]
  ): Page = Page(Node.Name(name), dox, lastmodified)
}

trait MetaDataNode extends Node

case class CategoryMetaData(
  name: Node.Name,
  category: Category
) extends MetaDataNode {
}
object CategoryMetaData {
  def apply(name: String, c: Category): CategoryMetaData = CategoryMetaData(Node.Name(name), c)

  def error(name: String, e: Throwable) =
    CategoryMetaData(Node.Name(name), Category.error(name, e))
}

case class HoconMetaData(
  name: Node.Name,
  hocon: Hocon
) extends MetaDataNode {
}

case class ImageNode(
  name: Node.Name,
  file: File
) extends Node
