package org.smartdox.metadata

import org.smartdox._

/*
 * @since   Feb. 23, 2025
 * @version Nov. 21, 2025
 * @author  ASAMI, Tomoharu
 */
case class Bibliography(
  definitions: Vector[Bibliography.Definition] = Vector.empty
) {
  def toHistory: History = {
    val slots = definitions.flatMap(_.toHistorySlot)
    History(slots)
  }
}

object Bibliography {
  val empty = Bibliography()

  case class Definition() {
    def toHistorySlot: Vector[History.Slot] = Vector.empty
  }
}
