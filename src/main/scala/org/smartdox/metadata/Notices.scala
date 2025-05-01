package org.smartdox.metadata

import org.joda.time.DateTime
import org.goldenport.i18n.I18NString
import org.smartdox._

/*
 * @since   Apr. 28, 2025
 * @version Apr. 30, 2025
 * @author  ASAMI, Tomoharu
 */
case class Notices(
  notices: Vector[Notices.Notice] = Vector.empty
) {
}

object Notices {
  val empty = Notices()

  case class Notice(title: String, published: DateTime)

  case class Builder() {
    def build(): Notices = ???
  }
}
