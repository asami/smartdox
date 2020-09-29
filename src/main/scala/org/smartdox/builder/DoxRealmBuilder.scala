package org.smartdox.builder

import org.goldenport.realm.Realm
import org.smartdox._
import org.smartdox.generator.Context

/*
 * @since   Jul. 26, 2020
 * @version Aug. 13, 2020
 * @author  ASAMI, Tomoharu
 */
class DoxRealmBuilder(val context: Context) {
  import DoxRealmBuilder._

  def createCursor: Cursor = ???

  def build(): Realm = ???

}

object DoxRealmBuilder {
  class Cursor() {
    def enterTopic(title: Dox, subtitle: String): Unit = ???
    def leaveTopic(): Unit = ???
    def enterPage(title: Dox, usbtitle: String): Unit = ???
    def leavePage(): Unit = ???
    def enterDivision(title: Dox): Unit = ???
    def leaveDivision(): Unit = ???

    def setDescription(p: Description): Unit = ???
    def setResume(p: Resume): Unit = ???
    def addContent(p: Dox): Unit = ???
    def addTable(caption: String, p: Table): Unit = ???
  }
}
