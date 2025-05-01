package org.smartdox.specdoc

import org.smartdox._

/*
 * @since   Oct. 13, 2008
 *  version Oct. 21, 2008
 * @version Jun. 13, 2020
 */
abstract class SDCategory(val name: String) {
  def label = name // XXX
  def tableHead: Seq[Dox]
  def tableRow(anEntity: SDEntity): Seq[Dox]
}
