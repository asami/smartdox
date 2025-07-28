package org.smartdox.metadata

import scalaz._, Scalaz._
import scala.xml.{Node => XNode, _}
import java.time.Instant
import org.goldenport.context.Consequence
import org.goldenport.xml.XmlUtils

/*
 * @since   Jul. 26, 2025
 * @version Jul. 26, 2025
 * @author  ASAMI, Tomoharu
 */
case class DoxCacheControl(
  lastModified: Option[Instant]
) {
  def isFlush(lastmodified: Option[Instant]): Boolean =
    (lastModified, lastmodified) match {
      case (Some(a), Some(b)) => a.isBefore(a)
      case _ => true
    }

  def isAvailable(lastmodified: Instant): Boolean =
    lastModified.fold(false)(_.isAfter(lastmodified))

  def mark(): DoxCacheControl = copy(lastModified = Some(Instant.now()))

  def print(buf: StringBuilder): Unit = {
    XmlUtils.printOpenTag(buf, "doxCacheControl")
    XmlUtils.printObject(buf, "lastModified", lastModified)
    XmlUtils.printCloseTag(buf, "doxCacheControl")
  }
}

object DoxCacheControl {
  def marked(): DoxCacheControl = DoxCacheControl(Some(Instant.now()))

  def parse(p: XNode): Consequence[Option[DoxCacheControl]] =
    for {
      x <- XmlUtils.getElementC(p, "doxCacheControl")
      c <- x.traverse(_build)
    } yield c

  private def _build(p: Elem): Consequence[DoxCacheControl] = {
    for {
      lastmodified <- XmlUtils.getInstantC(p, "lastModified")
    } yield {
      DoxCacheControl(lastmodified)
    }
  }
}
