package org.smartdox.gadget

import org.smartdox._

/*
 * Derived from org.goldenport.sdoc.parts.SHistory.
 * 
 * @since   Oct. 26, 2008
 *  version Jul. 15, 2010
 *  version Jul. 11, 2020
 * @version Nov.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class History(
) {
  def isEmpty: Boolean = true

  def toTable: Table = Table.empty // TODO
}

object History {
  val empty = History()
}
